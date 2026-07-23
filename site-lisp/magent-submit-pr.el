;;; magent-submit-pr.el --- Finish Magent work as a pull request -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai, vc
;; Package-Requires: ((emacs "30.1") (magent "0"))

;;; Commentary:

;; This package registers the Magent /submit-pr workflow.  Trusted Elisp owns
;; the Git/GitHub sequence and yields only process and agent Steps that need
;; Magent's asynchronous lifecycle.

;;; Code:

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
   "%s on branch %s. Inspect them with `git -C %s diff --cached` as needed. "
   "Use only read-only Git inspection; do not edit files, alter Git state, "
   "push, or create a pull request.\n\nOptional context: %s\n\nReturn only "
   "the subject line. Do not use Markdown or a code fence.")
  "Prompt used to write the commit subject.")

(defconst magent-submit-pr--body-prompt
  (concat
   "Write the pull request for commit %s on branch %s in %s. Inspect the "
   "committed diff with read-only `git -C %s` commands as needed. Do not "
   "edit files, alter Git state, push, or create the pull request yourself. "
   "The body should have a concise Summary section.\n\nOptional context: "
   "%s\n\nReturn only the Markdown body and no surrounding prose or code "
   "fence.")
  "Prompt used to write the pull request body.")

(defun magent-submit-pr--extract-subject (content)
  "Extract one valid Conventional Commit subject from CONTENT."
  (let ((case-fold-search nil))
    (catch 'subject
      (dolist (line (split-string content "[\n\r]+" t "[ \t]+"))
        (when (and (<= (length line) 120)
                   (string-match-p magent-submit-pr--subject-regexp line))
          (throw 'subject line)))
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
    (error "gh succeeded but returned no pull request URL")))

(magent-command-defworkflow magent-submit-pr--workflow (invocation)
  "Create and publish one pull request for INVOCATION's working directory."
  (let* ((origin
          (file-name-as-directory
           (expand-file-name
            (or (magent-command-invocation-origin-directory invocation)
                default-directory))))
         (context
          (let ((argument (magent-command-invocation-argument invocation)))
            (if (string-empty-p (or argument "")) "(none)" argument)))
         (environment '(("GIT_TERMINAL_PROMPT" . "0")))
         branch
         commit)
    (condition-case err
        (let* ((root
                (file-name-as-directory
                 (file-truename
                  (string-trim
                   (magent-command-process
                       "Resolve repository"
                       (list "git" "-C" origin
                             "rev-parse" "--show-toplevel")
                     :directory origin
                     :environment environment)))))
               (current-branch
                (string-trim
                 (magent-command-process
                     "Read current branch"
                     '("git" "branch" "--show-current")
                   :directory root
                   :environment environment))))
          (setq branch
                (if (string-prefix-p "submit-pr/" current-branch)
                    current-branch
                  (format "submit-pr/%s"
                          (format-time-string "%Y%m%d-%H%M%S"))))

          (when
              (string-empty-p
               (magent-command-process
                   "Check worktree"
                   '("git" "status" "--porcelain=v1"
                     "--untracked-files=all")
                 :directory root
                 :environment environment))
            (error "The worktree has no changes to finish"))

          (magent-command-process
              (if (equal branch current-branch)
                  "Reuse pull request branch"
                "Create pull request branch")
              (if (equal branch current-branch)
                  (list "git" "switch" branch)
                (list "git" "switch" "-c" branch))
            :directory root
            :environment environment)

          (magent-command-process
              "Stage changes" '("git" "add" "-A")
            :directory root
            :environment environment)

          (when
              (string-empty-p
               (string-trim
                (magent-command-process
                    "Check staged changes"
                    '("git" "diff" "--cached" "--name-only")
                  :directory root
                  :environment environment)))
            (error "No staged changes remain to commit"))

          (let* ((subject
                  (magent-submit-pr--extract-subject
                   (magent-command-agent
                       "Write commit subject"
                       (format magent-submit-pr--subject-prompt
                               root branch root context)
                     :required-tools '(bash))))
                 (_commit-output
                  (magent-command-process
                      "Create commit"
                      (list "git" "commit" "-m" subject)
                    :directory root
                    :environment environment)))
            (setq commit
                  (string-trim
                   (magent-command-process
                       "Read commit" '("git" "rev-parse" "HEAD")
                     :directory root
                     :environment environment)))

            (magent-command-process
                "Push branch"
                (list "git" "push" "--set-upstream" "origin" branch)
              :directory root
              :environment environment)

            (let* ((body
                    (magent-submit-pr--normalize-body
                     (magent-command-agent
                         "Write pull request"
                         (format magent-submit-pr--body-prompt
                                 commit branch root root context)
                       :required-tools '(bash))))
                   (url
                    (magent-submit-pr--extract-url
                     (magent-command-process
                         "Create pull request"
                         (list "gh" "pr" "create"
                               "--head" branch
                               "--title" subject
                               "--body" body)
                       :directory root
                       :environment environment
                       :record-command nil))))
              (format
               (concat "Finish work completed.\n\nBranch: %s\n"
                       "Commit: %s\nPull request: %s")
               branch commit url))))
      (error
       (signal
        (car err)
        (cons
         (format
          "Finish work stopped: %s\n\nBranch: %s\nCommit: %s"
          (error-message-string err)
          (or branch "not created")
          (or commit "not created"))
         (cddr err)))))))

;;;###autoload
(defun magent-submit-pr-register ()
  "Register the Magent /submit-pr command."
  (magent-command-register
   "submit-pr"
   :description "Create a branch, commit and push all changes, then open a PR."
   :title "Finish work as a pull request"
   :session-policy 'isolated
   :workflow #'magent-submit-pr--workflow
   :source-layer 'user
   :requires 'subr-x))

(provide 'magent-submit-pr)
;;; magent-submit-pr.el ends here
