;;; magent-submit-pr.el --- Finish Magent work as a pull request -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai, vc
;; Package-Requires: ((emacs "30.1") (magent "0"))

;;; Commentary:

;; This package registers the Magent /submit-pr workflow.  The workflow owns
;; one explicit state object and advances through named stages so process and
;; agent callbacks do not form a deeply nested continuation tree.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magent-command)

(defconst magent-submit-pr--subject-regexp
  (concat
   "\\`\\(?:feat\\|fix\\|refactor\\|docs\\|test\\|chore"
   "\\|build\\|ci\\|perf\\|style\\)"
   "\\(?:([a-z0-9][a-z0-9_.-]*?)\\)?!?"
   ": [^[:cntrl:]]+\\'")
  "Regexp matching an accepted Conventional Commit subject.")

(defconst magent-submit-pr--subject-prompt
  (concat
   "Write one Conventional Commit subject for the exact staged changes in "
   "%s on branch %s. Inspect them with git diff --cached as needed. Use only "
   "read-only Git inspection; do not edit files, alter Git state, push, or "
   "create a pull request.\n\nOptional context: %s\n\nReturn only the "
   "subject line. Do not use Markdown or a code fence.")
  "Prompt used to write the commit subject.")

(defconst magent-submit-pr--body-prompt
  (concat
   "Write the pull request for commit %s on branch %s in %s. Inspect the "
   "committed diff as needed. Use only read-only Git inspection; do not edit "
   "files, alter Git state, push, or create the pull request yourself. The "
   "body should have a concise Summary section.\n\nOptional context: %s\n\n"
   "Return only the Markdown body and no surrounding prose or code fence.")
  "Prompt used to write the pull request body.")

(cl-defstruct (magent-submit-pr--state
               (:constructor magent-submit-pr--state-create))
  "Mutable state owned by one /submit-pr invocation."
  invocation
  root
  context
  branch
  reuse-branch-p
  process
  process-buffer
  commit
  subject)

(defun magent-submit-pr--active-p (state)
  "Return non-nil when STATE still owns an active invocation."
  (eq (magent-command-invocation-status
       (magent-submit-pr--state-invocation state))
      'active))

(defun magent-submit-pr--cleanup (state)
  "Stop and discard process resources owned by STATE."
  (let ((process (magent-submit-pr--state-process state))
        (buffer (magent-submit-pr--state-process-buffer state)))
    (setf (magent-submit-pr--state-process state) nil
          (magent-submit-pr--state-process-buffer state) nil)
    (when (and (processp process) (process-live-p process))
      (delete-process process))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun magent-submit-pr--fail (state message)
  "Fail STATE with MESSAGE and report the Git work already completed."
  (when (magent-submit-pr--active-p state)
    (magent-submit-pr--cleanup state)
    (let ((invocation (magent-submit-pr--state-invocation state)))
      (magent-command-set-cancel-function invocation nil)
      (magent-command-fail
       invocation
       (format
        "Finish work stopped: %s\n\nBranch: %s\nCommit: %s"
        message
        (or (magent-submit-pr--state-branch state) "not created")
        (or (magent-submit-pr--state-commit state) "not created"))))))

(defun magent-submit-pr--process-output (buffer)
  "Return BUFFER contents, or an empty string if BUFFER is dead."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun magent-submit-pr--run (state label command next-stage)
  "Run COMMAND for STATE and advance to NEXT-STAGE on success.
LABEL is reported as command progress."
  (when (magent-submit-pr--active-p state)
    (magent-command-progress
     (magent-submit-pr--state-invocation state) (concat label "..."))
    (let* ((buffer (generate-new-buffer " *magent-submit-pr*"))
           (default-directory (magent-submit-pr--state-root state))
           (process-environment
            (cons "GIT_TERMINAL_PROMPT=0" process-environment))
           child
           sentinel)
      (setq sentinel
            (lambda (finished _event)
              (when (and (memq (process-status finished) '(exit signal))
                         (eq finished
                             (magent-submit-pr--state-process state))
                         (not (process-get finished 'submit-pr-done)))
                (process-put finished 'submit-pr-done t)
                (let ((status (process-exit-status finished))
                      (output (magent-submit-pr--process-output buffer)))
                  (setf (magent-submit-pr--state-process state) nil
                        (magent-submit-pr--state-process-buffer state) nil)
                  (when (buffer-live-p buffer)
                    (kill-buffer buffer))
                  (when (magent-submit-pr--active-p state)
                    (if (zerop status)
                        (condition-case err
                            (magent-submit-pr--advance
                             state next-stage output)
                          ((error quit)
                           (magent-submit-pr--fail
                            state (error-message-string err))))
                      (magent-submit-pr--fail
                       state
                       (format "%s failed (%s): %s"
                               (mapconcat #'shell-quote-argument command " ")
                               status
                               (string-trim output)))))))))
      (condition-case err
          (progn
            (setq child
                  (make-process
                   :name "magent-submit-pr"
                   :buffer buffer
                   :command command
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :noquery t
                   :sentinel #'ignore))
            (setf (magent-submit-pr--state-process state) child
                  (magent-submit-pr--state-process-buffer state) buffer)
            (set-process-sentinel child sentinel)
            (when (memq (process-status child) '(exit signal))
              (funcall sentinel child "finished\n")))
        ((error quit)
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (magent-submit-pr--fail state (error-message-string err)))))))

(defun magent-submit-pr--extract-subject (content)
  "Extract one valid Conventional Commit subject from CONTENT."
  (let ((case-fold-search nil))
    (or (cl-find-if
         (lambda (line)
           (and (<= (length line) 120)
                (string-match-p magent-submit-pr--subject-regexp line)))
         (split-string content "[\n\r]+" t "[ \t]+"))
        (error "No valid Conventional Commit subject in: %S" content))))

(defun magent-submit-pr--normalize-body (content)
  "Return a non-empty pull request body normalized from CONTENT."
  (let* ((body (string-trim content))
         (opening-end (and (string-prefix-p "```" body)
                           (string-match "\n" body))))
    (when (and opening-end (string-suffix-p "```" body))
      (setq body (string-trim (substring body (1+ opening-end) -3))))
    (when (string-empty-p body)
      (error "Pull request body is empty"))
    body))

(defun magent-submit-pr--extract-url (output)
  "Return the first HTTP URL from successful gh OUTPUT."
  (if (string-match "https?://[^[:space:]]+" output)
      (match-string 0 output)
    (user-error "gh succeeded but returned no PR URL")))

(defun magent-submit-pr--context (state)
  "Return optional model context stored in STATE."
  (let ((context (magent-submit-pr--state-context state)))
    (if (string-empty-p context) "(none)" context)))

(defun magent-submit-pr--submit-step
    (state label prompt next-stage)
  "Submit PROMPT for STATE and advance to NEXT-STAGE.
LABEL identifies the model step in progress and failure messages."
  (let ((invocation (magent-submit-pr--state-invocation state)))
    (magent-command-progress invocation (format "Writing %s..." label))
    (magent-command-submit-step
     invocation prompt
     (lambda (status result)
       (when (magent-submit-pr--active-p state)
         (if (eq status 'completed)
             (condition-case err
                 (magent-submit-pr--advance
                  state next-stage
                  (magent-agent-result-content-string result))
               ((error quit)
                (magent-submit-pr--fail state (error-message-string err))))
           (magent-submit-pr--fail
            state
            (format "%s step ended with %s: %s"
                    label status
                    (magent-agent-result-content-string result)))))))))

(defun magent-submit-pr--advance (state stage &optional output)
  "Advance STATE through STAGE using optional process or model OUTPUT."
  (pcase stage
    ('resolve-root
     (setf (magent-submit-pr--state-root state)
           (file-name-as-directory
            (file-truename (string-trim output))))
     (magent-submit-pr--run
      state "Reading branch"
      (list "git" "-C" (magent-submit-pr--state-root state)
            "branch" "--show-current")
      'read-branch))
    ('read-branch
     (let ((current-branch (string-trim output)))
       (when (string-prefix-p "submit-pr/" current-branch)
         (setf (magent-submit-pr--state-branch state) current-branch
               (magent-submit-pr--state-reuse-branch-p state) t)))
     (magent-submit-pr--run
      state "Checking worktree"
      (list "git" "-C" (magent-submit-pr--state-root state)
            "status" "--porcelain=v1" "--untracked-files=all")
      'check-worktree))
    ('check-worktree
     (when (string-empty-p output)
       (user-error "The worktree has no changes to finish"))
     (let ((branch (magent-submit-pr--state-branch state))
           (reuse-p (magent-submit-pr--state-reuse-branch-p state)))
       (magent-submit-pr--run
        state
        (if reuse-p "Reusing branch" "Creating branch")
        (if reuse-p
            (list "git" "-C" (magent-submit-pr--state-root state)
                  "switch" branch)
          (list "git" "-C" (magent-submit-pr--state-root state)
                "switch" "-c" branch))
        'switch-branch)))
    ('switch-branch
     (magent-submit-pr--run
      state "Staging changes"
      (list "git" "-C" (magent-submit-pr--state-root state) "add" "-A")
      'stage-changes))
    ('stage-changes
     (magent-submit-pr--run
      state "Checking staged changes"
      (list "git" "-C" (magent-submit-pr--state-root state)
            "diff" "--cached" "--name-only")
      'check-staged))
    ('check-staged
     (when (string-empty-p (string-trim output))
       (user-error "No staged changes remain to commit"))
     (magent-submit-pr--submit-step
      state "commit message"
      (format magent-submit-pr--subject-prompt
              (magent-submit-pr--state-root state)
              (magent-submit-pr--state-branch state)
              (magent-submit-pr--context state))
      'write-subject))
    ('write-subject
     (setf (magent-submit-pr--state-subject state)
           (magent-submit-pr--extract-subject output))
     (magent-submit-pr--run
      state "Creating commit"
      (list "git" "-C" (magent-submit-pr--state-root state)
            "commit" "-m" (magent-submit-pr--state-subject state))
      'create-commit))
    ('create-commit
     (magent-submit-pr--run
      state "Reading commit"
      (list "git" "-C" (magent-submit-pr--state-root state)
            "rev-parse" "HEAD")
      'read-commit))
    ('read-commit
     (setf (magent-submit-pr--state-commit state) (string-trim output))
     (magent-submit-pr--run
      state "Pushing branch"
      (list "git" "-C" (magent-submit-pr--state-root state)
            "push" "--set-upstream" "origin"
            (magent-submit-pr--state-branch state))
      'push-branch))
    ('push-branch
     (magent-submit-pr--submit-step
      state "pull request"
      (format magent-submit-pr--body-prompt
              (magent-submit-pr--state-commit state)
              (magent-submit-pr--state-branch state)
              (magent-submit-pr--state-root state)
              (magent-submit-pr--context state))
      'write-body))
    ('write-body
     (magent-submit-pr--run
      state "Creating pull request"
      (list "gh" "pr" "create"
            "--head" (magent-submit-pr--state-branch state)
            "--title" (magent-submit-pr--state-subject state)
            "--body" (magent-submit-pr--normalize-body output))
      'create-pull-request))
    ('create-pull-request
     (let* ((invocation (magent-submit-pr--state-invocation state))
            (url (magent-submit-pr--extract-url output))
            (report
             (format
              (concat "Finish work completed.\n\nBranch: %s\n"
                      "Commit: %s\nPull request: %s")
              (magent-submit-pr--state-branch state)
              (magent-submit-pr--state-commit state)
              url)))
       (magent-command-set-cancel-function invocation nil)
       (magent-command-respond invocation report)
       (magent-command-complete invocation report)))
    (_ (error "Unknown /submit-pr stage: %S" stage))))

(defun magent-submit-pr--start (invocation)
  "Start a /submit-pr workflow for INVOCATION."
  (let* ((scope (magent-command-invocation-origin-scope invocation))
         (root (and (stringp scope)
                    (file-name-as-directory (expand-file-name scope)))))
    (unless root
      (user-error "/submit-pr requires a filesystem-backed project"))
    (let ((state
           (magent-submit-pr--state-create
            :invocation invocation
            :root root
            :context (or (magent-command-invocation-argument invocation) "")
            :branch (format "submit-pr/%s"
                            (format-time-string "%Y%m%d-%H%M%S")))))
      (magent-command-defer invocation)
      (magent-command-set-cancel-function
       invocation (lambda () (magent-submit-pr--cleanup state)))
      (magent-submit-pr--run
       state "Resolving repository"
       (list "git" "-C" root "rev-parse" "--show-toplevel")
       'resolve-root))))

;;;###autoload
(defun magent-submit-pr-register ()
  "Register the Magent /submit-pr command."
  (magent-command-register
   "submit-pr"
   :description "Create a branch, commit and push all changes, then open a PR."
   :title "Finish work as a pull request"
   :session-policy 'isolated
   :source-layer 'user
   :requires-project t
   :required-tools '(bash)
   :handler #'magent-submit-pr--start))

(provide 'magent-submit-pr)
;;; magent-submit-pr.el ends here
