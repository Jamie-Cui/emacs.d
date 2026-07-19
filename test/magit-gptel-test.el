;;; magit-gptel-test.el --- Tests for magit-gptel -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for commit-message normalization and validation.

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

(ert-deftest magit-gptel-validator-accepts-valid-message ()
  (should-not
   (magit-gptel--commit-message-error
    (concat "refactor(memory): Remove status command\n\n"
            "Drop the obsolete status display."))))

(ert-deftest magit-gptel-validator-rejects-invalid-subjects ()
  (should
   (equal
    (magit-gptel--commit-message-error
     "feature(memory): Add status command")
    "Subject is not a valid Conventional Commit"))
  (should
   (equal
    (magit-gptel--commit-message-error
     "refactor(memory): remove status command")
    "Description must begin with an uppercase letter"))
  (should
   (equal
    (magit-gptel--commit-message-error
     "refactor(Memory): Remove status command")
    "Scope must be lowercase"))
  (should
   (equal
    (magit-gptel--commit-message-error
     "feat: Remove magent-open-memory-status")
    "Removal descriptions cannot use the feat type"))
  (should
   (equal
    (magit-gptel--commit-message-error
     "refactor(memory): Remove status command.")
    "Subject ends with punctuation")))

(ert-deftest magit-gptel-validator-allows-long-subjects ()
  (should-not
   (magit-gptel--commit-message-error
    "feat(isync): Add mbsync configuration for Outlook mail mirroring")))

(ert-deftest magit-gptel-validator-requires-one-body-separator ()
  (should
   (equal
    (magit-gptel--commit-message-error
     "refactor(memory): Remove status command\nBody text")
    "Commit body must begin after exactly one blank line"))
  (should
   (equal
    (magit-gptel--commit-message-error
     "refactor(memory): Remove status command\n\n\nBody text")
    "Commit body must begin after exactly one blank line")))

(ert-deftest magit-gptel-reasoning-fallback-strips-inline-markup ()
  (let ((request
         (magit-gptel-request-create
          :kind 'commit-message
          :reasoning-chunks
          '("refactor(memory): Remove `status` command"))))
    (should
     (equal (magit-gptel--reasoning-fallback-text request)
            "refactor(memory): Remove status command"))))

(ert-deftest magit-gptel-invalid-response-is-not-applied ()
  (let ((target (generate-new-buffer " *magit-gptel-test-target*"))
        preview)
    (unwind-protect
        (let ((request
               (magit-gptel-request-create
                :id "test-request"
                :kind 'commit-message
                :repo-root default-directory
                :target-buffer target)))
          (cl-letf (((symbol-function 'magit-gptel--show-commit-preview)
                     (lambda (_request message reason)
                       (setq preview (list message reason)))))
            (magit-gptel--apply-commit-response
             request "feat: remove status command" nil))
          (should
           (equal preview
                  '("feat: remove status command"
                    "Description must begin with an uppercase letter")))
          (with-current-buffer target
            (should (string-empty-p (buffer-string)))))
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
   (string-match-p "useful)A prompt adapted"
                   magit-gptel--default-commit-prompt)))

(provide 'magit-gptel-test)
;;; magit-gptel-test.el ends here
