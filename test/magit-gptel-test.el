;;; magit-gptel-test.el --- Tests for magit-gptel -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for commit-message normalization and insertion.

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (expand-file-name "../site-lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))

(require 'magit-gptel)

(ert-deftest magit-gptel-normalize-removes-inline-code-markers ()
  (should
   (equal
    (magit-gptel--normalize-commit-message
     (concat "refactor(memory): Remove `status` command\n\n"
             "Drop the obsolete status display."))
    (concat "refactor(memory): Remove status command\n\n"
            "Drop the obsolete status display."))))

(ert-deftest magit-gptel-normalize-unwraps-fenced-message ()
  (should
   (equal
    (magit-gptel--normalize-commit-message
     "```text\nstyle(llm): Reformat skill entry\n```")
    "style(llm): Reformat skill entry")))

(ert-deftest magit-gptel-extractor-does-not-return-partial-subject ()
  (should-not
   (magit-gptel--extract-commit-subject
    "feat: Remove `magent-open-memory-status`")))

(ert-deftest magit-gptel-extractor-finds-complete-subject-line ()
  (should
   (equal
    (magit-gptel--extract-commit-subject
     "Suggested commit:\nrefactor(memory): Remove status command")
    "refactor(memory): Remove status command")))

(ert-deftest magit-gptel-reasoning-fallback-strips-inline-markup ()
  (let ((request
         (magit-gptel-request-create
          :kind 'commit-message
          :reasoning-chunks
          '("refactor(memory): Remove `status` command"))))
    (should
     (equal (magit-gptel--reasoning-fallback-text request)
            "refactor(memory): Remove status command"))))

(ert-deftest magit-gptel-format-errors-do-not-block-insertion ()
  (let ((target (generate-new-buffer " *magit-gptel-test-target*"))
        (commit-message
         "feat(llm): add finish-work command to create PRs from changes"))
    (unwind-protect
        (let ((request
               (magit-gptel-request-create
                :id "test-request"
                :kind 'commit-message
                :repo-root default-directory
                :target-buffer target)))
          (cl-letf (((symbol-function 'magit-gptel--show-commit-preview)
                     (lambda (&rest _args)
                       (ert-fail "Formatting opened a commit preview"))))
            (magit-gptel--apply-commit-response
             request commit-message nil))
          (with-current-buffer target
            (should (equal (string-trim-right (buffer-string))
                           commit-message))))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest magit-gptel-non-conventional-response-is-applied ()
  (let ((target (generate-new-buffer " *magit-gptel-test-target*"))
        (commit-message "Update staged changes."))
    (unwind-protect
        (let ((request
               (magit-gptel-request-create
                :id "test-request"
                :kind 'commit-message
                :repo-root default-directory
                :target-buffer target)))
          (magit-gptel--apply-commit-response request commit-message nil)
          (with-current-buffer target
            (should (equal (string-trim-right (buffer-string))
                           commit-message))))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest magit-gptel-long-subject-is-applied-with-log-message ()
  (let ((target (generate-new-buffer " *magit-gptel-test-target*"))
        (commit-message
         "feat(isync): Add mbsync configuration for Outlook mail mirroring")
        log-message)
    (unwind-protect
        (let ((request
               (magit-gptel-request-create
                :id "test-request"
                :kind 'commit-message
                :repo-root default-directory
                :target-buffer target)))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq log-message
                             (apply #'format format-string args)))))
            (magit-gptel--apply-commit-response request commit-message nil))
          (with-current-buffer target
            (should (equal (string-trim-right (buffer-string))
                           commit-message)))
          (should (string-match-p
                   "subject is 64 characters (maximum is 50)"
                   log-message)))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest magit-gptel-prompt-includes-classification-guardrails ()
  (should
   (string-match-p
    (regexp-quote "Removing a command is not a feat")
    magit-gptel--default-commit-prompt))
  (should
   (string-match-p
    (regexp-quote "A whitespace-only or line-wrapping change is style")
    magit-gptel--default-commit-prompt))
  (should-not
   (string-match-p
    (regexp-quote "Capitalize the first word of the description")
    magit-gptel--default-commit-prompt))
  (should-not
   (string-match-p "useful)A prompt adapted"
                   magit-gptel--default-commit-prompt)))

(provide 'magit-gptel-test)
;;; magit-gptel-test.el ends here
