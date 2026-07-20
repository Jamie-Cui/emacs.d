;;; magent-submit-pr-test.el --- Tests for magent-submit-pr -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for the /submit-pr workflow stages and parsers.

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (expand-file-name "../site-lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))

(require 'magent-submit-pr)

(defun magent-submit-pr-test--invocation (&rest args)
  "Return an active command invocation initialized with ARGS."
  (apply #'magent-command-invocation-create :status 'active args))

(ert-deftest magent-submit-pr-test-extracts-subject-line ()
  (should
   (equal (magent-submit-pr--extract-subject
           "Suggested subject:\nrefactor(llm): simplify submit workflow\n")
          "refactor(llm): simplify submit workflow")))

(ert-deftest magent-submit-pr-test-rejects-invalid-or-long-subject ()
  (should-error
   (magent-submit-pr--extract-subject "Simplify submit workflow"))
  (should-error
   (magent-submit-pr--extract-subject
    (concat "refactor(llm): " (make-string 121 ?x))))
  (should-error
   (magent-submit-pr--extract-subject "feat(Invalid): uppercase scope")))

(ert-deftest magent-submit-pr-test-normalizes-pr-body ()
  (should (equal (magent-submit-pr--normalize-body "  ## Summary\nDone.  ")
                 "## Summary\nDone."))
  (should
   (equal (magent-submit-pr--normalize-body
           "```markdown\n## Summary\nDone.\n```")
          "## Summary\nDone."))
  (should-error (magent-submit-pr--normalize-body "  "))
  (should-error (magent-submit-pr--normalize-body "```markdown\n```")))

(ert-deftest magent-submit-pr-test-extracts-pr-url ()
  (should
   (equal (magent-submit-pr--extract-url
           "Created pull request: https://github.com/example/repo/pull/42\n")
          "https://github.com/example/repo/pull/42"))
  (should-error (magent-submit-pr--extract-url "Pull request created")))

(ert-deftest magent-submit-pr-test-runs-new-branch-workflow-in-order ()
  (let* ((root (file-name-as-directory
                (file-truename temporary-file-directory)))
         (invocation (magent-submit-pr-test--invocation))
         (state
          (magent-submit-pr--state-create
           :invocation invocation
           :root root
           :context "Keep compatibility"
           :branch "submit-pr/20260720-120000"))
         (outputs
          `((read-branch . "main\n")
            (check-worktree . " M lisp/modules/llm.el\n")
            (switch-branch . "")
            (stage-changes . "")
            (check-staged . "lisp/modules/llm.el\n")
            (create-commit . "")
            (read-commit . "deadbeef\n")
            (push-branch . "")
            (create-pull-request
             . "https://github.com/example/repo/pull/42\n")))
         run-stages
         commands
         model-stages
         prompts
         response
         completion
         cancel-function)
    (cl-letf (((symbol-function 'magent-submit-pr--run)
               (lambda (_state _label command next-stage)
                 (push next-stage run-stages)
                 (push command commands)
                 (magent-submit-pr--advance
                  state next-stage (alist-get next-stage outputs))))
              ((symbol-function 'magent-submit-pr--submit-step)
               (lambda (_state _label prompt next-stage)
                 (push next-stage model-stages)
                 (push prompt prompts)
                 (magent-submit-pr--advance
                  state next-stage
                  (pcase next-stage
                    ('write-subject
                     "refactor(llm): simplify submit workflow")
                    ('write-body "```markdown\n## Summary\nSimplified.\n```")))))
              ((symbol-function 'magent-command-set-cancel-function)
               (lambda (_invocation function)
                 (setq cancel-function function)))
              ((symbol-function 'magent-command-respond)
               (lambda (_invocation content &optional _metadata)
                 (setq response content)))
              ((symbol-function 'magent-command-complete)
               (lambda (_invocation result)
                 (setq completion result))))
      (magent-submit-pr--advance state 'resolve-root root))
    (should
     (equal (nreverse run-stages)
            '(read-branch check-worktree switch-branch stage-changes
              check-staged create-commit read-commit push-branch
              create-pull-request)))
    (should (equal (nreverse model-stages) '(write-subject write-body)))
    (setq commands (nreverse commands)
          prompts (nreverse prompts))
    (should
     (equal (nth 2 commands)
            (list "git" "-C" root "switch" "-c"
                  "submit-pr/20260720-120000")))
    (should
     (equal (car (last commands))
            '("gh" "pr" "create"
              "--head" "submit-pr/20260720-120000"
              "--title" "refactor(llm): simplify submit workflow"
              "--body" "## Summary\nSimplified.")))
    (should (string-match-p "Optional context: Keep compatibility"
                            (car prompts)))
    (should (equal response completion))
    (should (string-match-p "Commit: deadbeef" completion))
    (should (string-match-p "pull/42" completion))
    (should-not cancel-function)))

(ert-deftest magent-submit-pr-test-reuses-submit-branch ()
  (let* ((root (file-name-as-directory temporary-file-directory))
         (state
          (magent-submit-pr--state-create
           :invocation (magent-submit-pr-test--invocation)
           :root root
           :context ""
           :branch "submit-pr/new"))
         calls)
    (cl-letf (((symbol-function 'magent-submit-pr--run)
               (lambda (_state label command next-stage)
                 (push (list label command next-stage) calls))))
      (magent-submit-pr--advance state 'read-branch "submit-pr/existing\n")
      (magent-submit-pr--advance state 'check-worktree " M file.el\n"))
    (should (magent-submit-pr--state-reuse-branch-p state))
    (should (equal (magent-submit-pr--state-branch state)
                   "submit-pr/existing"))
    (should
     (equal (cadar calls)
            (list "git" "-C" root "switch" "submit-pr/existing")))))

(ert-deftest magent-submit-pr-test-rejects-empty-change-sets ()
  (let ((state
         (magent-submit-pr--state-create
          :invocation (magent-submit-pr-test--invocation)
          :root temporary-file-directory
          :context ""
          :branch "submit-pr/new")))
    (should-error
     (magent-submit-pr--advance state 'check-worktree "")
     :type 'user-error)
    (should-error
     (magent-submit-pr--advance state 'check-staged " \n\t")
     :type 'user-error)))

(ert-deftest magent-submit-pr-test-reports-model-step-failure ()
  (let* ((state
          (magent-submit-pr--state-create
           :invocation (magent-submit-pr-test--invocation)
           :root temporary-file-directory
           :context ""
           :branch "submit-pr/new"))
         failure)
    (cl-letf (((symbol-function 'magent-command-progress) #'ignore)
              ((symbol-function 'magent-command-submit-step)
               (lambda (_invocation _prompt callback &rest _args)
                 (funcall callback 'failed 'result)))
              ((symbol-function 'magent-agent-result-content-string)
               (lambda (_result) "backend unavailable"))
              ((symbol-function 'magent-submit-pr--fail)
               (lambda (_state message) (setq failure message))))
      (magent-submit-pr--submit-step
       state "commit message" "prompt" 'write-subject))
    (should
     (equal failure
            "commit message step ended with failed: backend unavailable"))))

(ert-deftest magent-submit-pr-test-cleanup-owns-process-and-buffer ()
  (let* ((buffer (generate-new-buffer " *magent-submit-pr-test*"))
         (process (make-pipe-process
                   :name "magent-submit-pr-test"
                   :buffer buffer
                   :noquery t))
         (state
          (magent-submit-pr--state-create
           :invocation (magent-submit-pr-test--invocation)
           :process process
           :process-buffer buffer)))
    (magent-submit-pr--cleanup state)
    (should-not (process-live-p process))
    (should-not (buffer-live-p buffer))
    (should-not (magent-submit-pr--state-process state))
    (should-not (magent-submit-pr--state-process-buffer state))))

(ert-deftest magent-submit-pr-test-process-failure-includes-command-output ()
  (let* ((state
          (magent-submit-pr--state-create
           :invocation (magent-submit-pr-test--invocation)
           :root temporary-file-directory))
         failure
         advanced)
    (cl-letf (((symbol-function 'magent-command-progress) #'ignore)
              ((symbol-function 'magent-submit-pr--fail)
               (lambda (_state message) (setq failure message)))
              ((symbol-function 'magent-submit-pr--advance)
               (lambda (&rest _args) (setq advanced t))))
      (magent-submit-pr--run
       state "Failing" '("sh" "-c" "printf boom; exit 7") 'next)
      (while (magent-submit-pr--state-process state)
        (accept-process-output nil 0.1)))
    (should-not advanced)
    (should (string-match-p "failed (7): boom" failure))))

(ert-deftest magent-submit-pr-test-cancelled-process-cannot-advance ()
  (let* ((invocation (magent-submit-pr-test--invocation))
         (state
          (magent-submit-pr--state-create
           :invocation invocation
           :root temporary-file-directory))
         process
         advanced
         failure)
    (cl-letf (((symbol-function 'magent-command-progress) #'ignore)
              ((symbol-function 'magent-submit-pr--advance)
               (lambda (&rest _args) (setq advanced t)))
              ((symbol-function 'magent-submit-pr--fail)
               (lambda (&rest _args) (setq failure t))))
      (magent-submit-pr--run state "Waiting" '("sh" "-c" "cat") 'next)
      (setq process (magent-submit-pr--state-process state))
      (setf (magent-command-invocation-status invocation) 'cancelled)
      (magent-submit-pr--cleanup state)
      (accept-process-output nil 0.1))
    (should-not (process-live-p process))
    (should-not advanced)
    (should-not failure)))

(ert-deftest magent-submit-pr-test-registers-public-command-contract ()
  (let ((magent-command--registry nil)
        (magent-command--sequence 0)
        (magent-command-registry-changed-hook nil))
    (let ((spec (magent-submit-pr-register)))
      (should (equal (magent-command-spec-name spec) "submit-pr"))
      (should (eq (magent-command-spec-handler spec)
                  #'magent-submit-pr--start))
      (should (eq (magent-command-spec-session-policy spec) 'isolated))
      (should (eq (magent-command-spec-source-layer spec) 'user))
      (should (magent-command-spec-requires-project spec))
      (should (equal (magent-command-spec-required-tools spec) '(bash))))))

(provide 'magent-submit-pr-test)
;;; magent-submit-pr-test.el ends here
