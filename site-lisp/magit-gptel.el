;;; magit-gptel.el --- Request-aware Magit integration for gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai, vc
;; Package-Requires: ((emacs "28.1") (magit "4.0") (gptel "0.9.8") (transient "0.13"))

;;; Commentary:

;; This package provides request-aware gptel commands for Magit.  The key
;; design choice is to treat each LLM call as a first-class request object that
;; owns:
;;
;; - the originating repository
;; - the diff snapshot sent to the model
;; - the target buffer that may be updated later
;; - the baseline commit message text at dispatch time
;;
;; That lets us reject stale callbacks instead of blindly inserting text into
;; whichever commit buffer happens to exist when the response arrives.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'gptel-request)
(require 'magit)
(require 'git-commit)
(require 'transient)

(declare-function gptel-magit-install "gptel-magit")
(declare-function gptel-magit-commit-generate "gptel-magit")
(declare-function gptel-magit-diff-explain "gptel-magit")
(declare-function gptel-fsm-info "gptel-request")
(declare-function markdown-view-mode "markdown-mode")
(declare-function gfm-view-mode "markdown-mode")

(defgroup magit-gptel nil
  "Request-aware Magit integration for gptel."
  :group 'magit
  :prefix "magit-gptel-")

(defconst magit-gptel--default-commit-prompt
  (concat
   "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

The commit message should be structured as follows:

    <type>(<optional scope>): <description>

    [optional body]

- Commits MUST be prefixed with a type, which consists of one of the followings words: build, chore, ci, docs, feat, fix, perf, refactor, style, test
- The type feat MUST be used when a commit adds a new feature
- The type fix MUST be used when a commit represents a bug fix
- An optional scope MAY be provided after a type. A scope is a phrase describing a section of the codebase enclosed in parenthesis, e.g., fix(parser):
- A description MUST immediately follow the type/scope prefix. The description is a short description of the code changes, e.g., fix: array parsing issue when multiple spaces were contained in string.
- Try to limit the whole subject line to 60 characters
- Capitalize the subject line
- Do not end the subject line with any punctuation
- A longer commit body MAY be provided after the short description, providing additional contextual information about the code changes. The body MUST begin one blank line after the description.
- Use the imperative mood in the subject line
- Keep the body short and concise (omit it entirely if not useful)"
   "A prompt adapted from Conventional Commits (https://www.conventionalcommits.org/en/v1.0.0/)."
   ;; "You write Git commit messages from staged diffs.\n\n"
   ;; "Return only the commit message.  Do not wrap it in code fences.  Do not "
   ;; "add prefaces such as \"Here is the commit message\".\n\n"
   ;; "Write a concise subject line.  Add a body only when it carries real "
   ;; "review value.  Keep the body short and factual.\n\n"
   ;; "Prefer Conventional Commit style when the change naturally fits it:\n"
   ;; "<type>(<optional-scope>): <description>\n\n"
   ;; "Use imperative mood.  Do not end the subject line with punctuation."
   )
  "Default system prompt for commit message generation.")

(defconst magit-gptel--default-diff-explain-prompt
  (concat
   "You explain Git diffs for a developer reading them in Magit.\n\n"
   "Answer in Markdown.  Focus on intent, behavior changes, risks, and any "
   "missing follow-up work.  When the diff is ambiguous, say what is certain "
   "and what is inference.")
  "Default system prompt for diff explanation.")

(defcustom magit-gptel-commit-prompt
  magit-gptel--default-commit-prompt
  "System prompt used for commit message generation."
  :type 'string
  :group 'magit-gptel)

(defcustom magit-gptel-diff-explain-prompt
  magit-gptel--default-diff-explain-prompt
  "System prompt used for explaining the current diff selection."
  :type 'string
  :group 'magit-gptel)

(custom-declare-variable
 'magit-gptel-model nil
 "Model override for Magit GPTel requests.

When nil, use the global default value of `gptel-model'."
 :type (get 'gptel-model 'custom-type)
 :group 'magit-gptel)

(custom-declare-variable
 'magit-gptel-backend nil
 "Backend override for Magit GPTel requests.

When nil, use the global default value of `gptel-backend'."
 :type (get 'gptel-backend 'custom-type)
 :group 'magit-gptel)

(defcustom magit-gptel-max-diff-chars 120000
  "Maximum diff payload size sent to the model.

Large diffs are truncated after this many characters, but a stat/summary block
is always kept ahead of the patch."
  :type 'integer
  :group 'magit-gptel)

(defcustom magit-gptel-commit-reasoning-fallback t
  "Whether to recover commit subjects from reasoning-only responses.

Some providers expose reasoning as a separate gptel callback and may omit
normal response text.  When this option is non-nil, commit generation can scan
that reasoning text for a Conventional Commit subject.  The full reasoning text
is never inserted into the commit buffer."
  :type 'boolean
  :group 'magit-gptel)

(defcustom magit-gptel-request-params nil
  "Extra request parameters for Magit GPTel requests.

These parameters are merged into a request-local copy of `magit-gptel-model',
so they do not mutate global gptel model state.  Values here take precedence
over model-specific `:request-params'."
  :type 'plist
  :group 'magit-gptel)

(defcustom magit-gptel-commit-buffer-wait-seconds 2.0
  "How long `magit-gptel-commit-create' waits for the commit buffer."
  :type 'number
  :group 'magit-gptel)

(defcustom magit-gptel-auto-install t
  "Whether to install Magit transient/keymap entries automatically."
  :type 'boolean
  :group 'magit-gptel)

(defcustom magit-gptel-explain-buffer-name "*magit-gptel explain*"
  "Buffer used to display diff explanations."
  :type 'string
  :group 'magit-gptel)

(defcustom magit-gptel-preview-buffer-name "*magit-gptel preview*"
  "Buffer used to display commit messages that were not applied automatically."
  :type 'string
  :group 'magit-gptel)

(defcustom magit-gptel-commit-transient-key "g"
  "Key used in `magit-commit' for AI-assisted commit drafting."
  :type 'string
  :group 'magit-gptel)

(defcustom magit-gptel-diff-transient-key "e"
  "Key used in `magit-diff' for AI diff explanation."
  :type 'string
  :group 'magit-gptel)

(defcustom magit-gptel-commit-buffer-key (kbd "C-c C-g")
  "Key used in `git-commit-mode' to draft a message with AI."
  :type 'key-sequence
  :group 'magit-gptel)

(defcustom magit-gptel-cancel-key (kbd "C-c M-k")
  "Key used in `git-commit-mode' to cancel the active AI request."
  :type 'key-sequence
  :group 'magit-gptel)

(cl-defstruct (magit-gptel-request
               (:constructor magit-gptel-request-create)
               (:copier nil))
  "State owned by a single Magit/GPTel request."
  id
  kind
  repo-root
  source-buffer
  target-buffer
  request-buffer
  snapshot
  baseline-text
  baseline-tick
  response-chunks
  reasoning-chunks
  unsupported-response
  status
  applied-callback)

(defvar magit-gptel--installed nil
  "Non-nil once Magit GPTel bindings have been installed.")

(defvar magit-gptel--request-counter 0
  "Monotonic counter used to build request ids.")

(defvar magit-gptel--live-requests (make-hash-table :test 'equal)
  "Table of live request objects keyed by request id.")

(defvar-local magit-gptel--active-request-id nil
  "Request id currently owned by this buffer, if any.")

(defvar-local magit-gptel--generated-message nil
  "Last commit message inserted by `magit-gptel'.")

(defvar magit-gptel--commit-wait-generation 0
  "Monotonic counter used to invalidate stale commit-buffer wait timers.")

(defun magit-gptel--next-request-id ()
  "Return a fresh Magit GPTel request id."
  (format "magit-gptel-%06d" (cl-incf magit-gptel--request-counter)))

(defun magit-gptel--default-backend ()
  "Return the backend to use for this request."
  (or magit-gptel-backend
      (default-value 'gptel-backend)))

(defun magit-gptel--default-model ()
  "Return the model to use for this request."
  (or magit-gptel-model
      (default-value 'gptel-model)))

(defun magit-gptel--merge-plists (&rest plists)
  "Merge PLISTS into a new plist, with later values taking precedence."
  (let (result)
    (dolist (plist plists)
      (let ((tail plist))
        (while tail
          (setq result (plist-put result (pop tail) (pop tail))))))
    result))

(defun magit-gptel--model-for-request ()
  "Return the model object to use for an isolated Magit GPTel request."
  (let ((model (magit-gptel--default-model)))
    (if (and magit-gptel-request-params (symbolp model))
        (let ((copy (make-symbol (symbol-name model))))
          (setplist copy (copy-sequence (symbol-plist model)))
          (put copy :request-params
               (magit-gptel--merge-plists
                (copy-sequence (get model :request-params))
                magit-gptel-request-params))
          copy)
      model)))

(defun magit-gptel--repo-root (&optional buffer)
  "Return repository root for BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or (magit-toplevel)
        (user-error "Not inside a Git repository"))))

(defun magit-gptel--current-branch (repo-root)
  "Return a readable branch or commit identifier for REPO-ROOT."
  (let ((default-directory repo-root))
    (or (magit-git-string "symbolic-ref" "--quiet" "--short" "HEAD")
        (magit-git-string "rev-parse" "--short" "HEAD")
        "HEAD")))

(defun magit-gptel--truncate-diff (diff)
  "Truncate DIFF when it is larger than `magit-gptel-max-diff-chars'."
  (if (and magit-gptel-max-diff-chars
           (> (length diff) magit-gptel-max-diff-chars))
      (concat (substring diff 0 magit-gptel-max-diff-chars)
              (format
               "\n\n[diff truncated after %d characters by magit-gptel]\n"
               magit-gptel-max-diff-chars))
    diff))

(defun magit-gptel--capture-staged-snapshot (repo-root)
  "Capture staged diff state for REPO-ROOT."
  (let* ((default-directory repo-root)
         (repo-name (file-name-nondirectory
                     (directory-file-name repo-root)))
         (branch (magit-gptel--current-branch repo-root))
         (summary (magit-git-output "diff" "--cached"
                                    "--stat=80,120"
                                    "--summary"
                                    "--no-ext-diff"
                                    "--no-color"
                                    "--submodule=diff"
                                    "-M"))
         (patch (magit-git-output "diff" "--cached"
                                  "--patch"
                                  "--no-ext-diff"
                                  "--no-color"
                                  "--submodule=diff"
                                  "-M")))
    (unless (string-match-p "[^[:space:]]" patch)
      (user-error "No staged changes to summarize"))
    (list :repo-root repo-root
          :repo-name repo-name
          :branch branch
          :summary (string-trim-right (or summary ""))
          :patch (string-trim-right patch)
          :payload
          (string-join
           (delq nil
                 (list
                  (format "Repository: %s" repo-name)
                  (format "Branch: %s" branch)
                  (when (string-match-p "[^[:space:]]" (or summary ""))
                    (format "Staged change summary:\n%s"
                            (string-trim-right summary)))
                  (format "Staged patch:\n%s"
                          (magit-gptel--truncate-diff patch))))
           "\n\n"))))

(defun magit-gptel--capture-diff-snapshot ()
  "Capture the current Magit diff selection as a stable text snapshot."
  (unless (derived-mode-p 'magit-mode)
    (user-error "Not in a Magit buffer"))
  (let* ((repo-root (magit-gptel--repo-root))
         (section (magit-current-section))
         (scope (magit-diff-scope section t))
         (diff-type (magit-diff-type section))
         (regionp (eq scope 'region))
         (owner-section (if regionp section section))
         (raw-text (cond
                    ((null scope)
                     (user-error "Point is not on a diff or hunk section"))
                    (regionp
                     ;; When Magit reports a region, explain the enclosing hunk.
                     (buffer-substring-no-properties
                      (oref owner-section start)
                      (oref owner-section end)))
                    (t
                     (buffer-substring-no-properties
                      (oref owner-section start)
                      (oref owner-section end))))))
    (unless (string-match-p "[^[:space:]]" raw-text)
      (user-error "No diff text available at point"))
    (list :repo-root repo-root
          :repo-name (file-name-nondirectory (directory-file-name repo-root))
          :branch (magit-gptel--current-branch repo-root)
          :scope scope
          :diff-type diff-type
          :region-derived regionp
          :text (string-trim-right raw-text)
          :payload
          (string-join
           (list
            (format "Repository: %s"
                    (file-name-nondirectory (directory-file-name repo-root)))
            (format "Branch: %s" (magit-gptel--current-branch repo-root))
            (format "Scope: %s%s"
                    scope
                    (if regionp " (explaining enclosing hunk)" ""))
            (format "Diff type: %s" diff-type)
            "Diff snapshot:"
            (magit-gptel--truncate-diff raw-text))
           "\n\n"))))

(defun magit-gptel--find-commit-buffer (repo-root)
  "Return the live commit buffer associated with REPO-ROOT, if any."
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (and git-commit-mode
            (ignore-errors
              (equal (magit-toplevel) repo-root)))))
   (buffer-list)))

(defun magit-gptel--wait-for-commit-buffer (repo-root callback)
  "Call CALLBACK with the commit buffer for REPO-ROOT once it exists.
Uses a generation counter to invalidate stale timers when this function
is called again before a previous wait has completed."
  (let ((gen (cl-incf magit-gptel--commit-wait-generation)))
    (cl-labels
        ((recurse (remaining)
           (if-let* ((buffer (magit-gptel--find-commit-buffer repo-root)))
               (when (= gen magit-gptel--commit-wait-generation)
                 (funcall callback buffer))
             (if (<= remaining 0)
                 (when (= gen magit-gptel--commit-wait-generation)
                   (message "magit-gptel: Timed out waiting for commit buffer in %s"
                            repo-root))
               (run-at-time 0.05 nil #'recurse (- remaining 0.05))))))
      (recurse magit-gptel-commit-buffer-wait-seconds))))

(defun magit-gptel--request-for-buffer (&optional buffer)
  "Return the active request owned by BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (and magit-gptel--active-request-id
         (gethash magit-gptel--active-request-id
                  magit-gptel--live-requests))))

(defun magit-gptel--register-request (request)
  "Register REQUEST as live and mark its owner buffer."
  (puthash (magit-gptel-request-id request)
           request
           magit-gptel--live-requests)
  (when-let* ((buffer (magit-gptel-request-target-buffer request)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local magit-gptel--active-request-id
                    (magit-gptel-request-id request)))))
  request)

(defun magit-gptel--cleanup-request (request)
  "Remove REQUEST from live state and kill its request buffer."
  (remhash (magit-gptel-request-id request) magit-gptel--live-requests)
  (when-let* ((buffer (magit-gptel-request-target-buffer request)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (equal magit-gptel--active-request-id
                     (magit-gptel-request-id request))
          (setq-local magit-gptel--active-request-id nil)))))
  (when-let* ((request-buffer (magit-gptel-request-request-buffer request)))
    (when (buffer-live-p request-buffer)
      (kill-buffer request-buffer))))

(defun magit-gptel--request-error-string (info)
  "Extract a readable error message from gptel INFO."
  (or (plist-get info :status)
      (when-let* ((error-data (plist-get info :error)))
        (if (stringp error-data)
            error-data
          (format "%s" error-data)))
      "Request failed"))

(defun magit-gptel--display-text-buffer (buffer-name text &optional markdown)
  "Show TEXT in BUFFER-NAME.
When MARKDOWN is non-nil, prefer a markdown viewing mode when available."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (cond
         ((and markdown (fboundp 'markdown-view-mode))
          (markdown-view-mode))
         ((and markdown (fboundp 'gfm-view-mode))
          (gfm-view-mode)
          (view-mode 1))
         (t
          (text-mode)
          (view-mode 1)))))
    (pop-to-buffer buffer)
    buffer))

(defun magit-gptel--show-commit-preview (request message reason)
  "Display MESSAGE for REQUEST in a preview buffer with REASON."
  (magit-gptel--display-text-buffer
   magit-gptel-preview-buffer-name
   (string-join
    (list
     "Magit GPTel generated a commit message but did not apply it automatically."
     ""
     (format "Reason: %s" reason)
     (format "Repository: %s" (magit-gptel-request-repo-root request))
     (format "Request: %s" (magit-gptel-request-id request))
     ""
     message)
    "\n")))

(defun magit-gptel--unwrap-code-fence (text)
  "Remove a single surrounding fenced code block from TEXT."
  (let* ((trimmed (string-trim text))
         (lines (split-string trimmed "\n")))
    (if (and (>= (length lines) 2)
             (string-prefix-p "```" (string-trim (car lines)))
             (string= "```" (string-trim (car (last lines)))))
        (string-join (butlast (cdr lines)) "\n")
      trimmed)))

(defconst magit-gptel--commit-subject-search-regexp
  (concat "\\(?:build\\|chore\\|ci\\|docs\\|feat\\|fix\\|perf\\|"
          "refactor\\|style\\|test\\)"
          "\\(?:([^)\n\"`]+)\\)?: [^\"`\n]+")
  "Regexp matching a Conventional Commit subject inside a line.")

(defconst magit-gptel--commit-subject-line-regexp
  (concat "\\`" magit-gptel--commit-subject-search-regexp "\\'")
  "Regexp matching a full Conventional Commit subject line.")

(defun magit-gptel--commit-subject-line-p (line)
  "Return non-nil when LINE is a Conventional Commit subject."
  (string-match-p magit-gptel--commit-subject-line-regexp
                  (string-trim line)))

(defun magit-gptel--extract-commit-subject (text)
  "Extract the last Conventional Commit subject found in TEXT."
  (let (subject)
    (dolist (line (split-string text "\n"))
      (when (string-match magit-gptel--commit-subject-search-regexp line)
        (setq subject (string-trim (match-string 0 line)))))
    subject))

(defun magit-gptel--normalize-commit-message (text)
  "Normalize model TEXT into a plain commit message string."
  (let* ((unfenced (magit-gptel--unwrap-code-fence text))
         (trimmed (string-trim unfenced))
         (lines (split-string trimmed "\n"))
         (lines (if (and lines
                         (string-match-p
                         "\\`\\(?:Suggested \\)?Commit message:?\\'"
                         (string-trim (car lines))))
                    (cdr lines)
                  lines)))
    (let ((message (string-trim-right (string-join lines "\n"))))
      (if (or (string-empty-p message)
              (magit-gptel--commit-subject-line-p
               (car (split-string message "\n"))))
          message
        (or (magit-gptel--extract-commit-subject message)
            message)))))

(defun magit-gptel--reasoning-response-p (response)
  "Return non-nil when RESPONSE is a gptel reasoning callback value."
  (and (consp response)
       (eq (car response) 'reasoning)))

(defun magit-gptel--join-chunks (chunks)
  "Join CHUNKS recorded in reverse arrival order."
  (apply #'concat (nreverse (copy-sequence chunks))))

(defun magit-gptel--response-text (request)
  "Return accumulated final response text for REQUEST."
  (magit-gptel--join-chunks
   (magit-gptel-request-response-chunks request)))

(defun magit-gptel--reasoning-text (request)
  "Return accumulated reasoning text for REQUEST."
  (magit-gptel--join-chunks
   (magit-gptel-request-reasoning-chunks request)))

(defun magit-gptel--reasoning-fallback-text (request)
  "Return a safe fallback text extracted from REQUEST reasoning."
  (when (and magit-gptel-commit-reasoning-fallback
             (eq (magit-gptel-request-kind request) 'commit-message))
    (magit-gptel--extract-commit-subject
     (magit-gptel--reasoning-text request))))

(defun magit-gptel--commit-message-region ()
  "Return the region containing the editable commit message body."
  (save-excursion
    (goto-char (point-min))
    (let* ((comment-char (or comment-start "#"))
           (comment-re (concat "^" (regexp-quote comment-char)))
           (cut-re (format "^%s -\\{8,\\} >8 -\\{8,\\}$"
                           (regexp-quote comment-char)))
           (end (point-max)))
      (when (re-search-forward cut-re nil t)
        (setq end (line-beginning-position)))
      (goto-char (point-min))
      (when (re-search-forward comment-re end t)
        (setq end (line-beginning-position)))
      (cons (point-min) end))))

(defun magit-gptel--apply-commit-response (request response _info)
  "Apply commit RESPONSE for REQUEST when the target buffer is still safe."
  (let ((message (magit-gptel--normalize-commit-message response))
        (buffer (magit-gptel-request-target-buffer request)))
    (cond
     ((string-empty-p message)
      (magit-gptel--show-commit-preview request response "Model returned an empty message"))
     ((not (buffer-live-p buffer))
      (magit-gptel--show-commit-preview request message "Target commit buffer was closed"))
     (t
      (with-current-buffer buffer
        (let ((baseline (or (magit-gptel-request-baseline-text request) ""))
              (current (or (git-commit-buffer-message) "")))
          (cond
           ((and magit-gptel--active-request-id
                 (not (equal magit-gptel--active-request-id
                             (magit-gptel-request-id request))))
            (magit-gptel--show-commit-preview
             request message "A newer request now owns this buffer"))
           ((not (string= baseline current))
            (magit-gptel--show-commit-preview
             request message "The commit buffer changed while the request was running"))
           (t
            (pcase-let ((`(,beg . ,end) (magit-gptel--commit-message-region)))
              (let ((inhibit-read-only t))
                (delete-region beg end)
                (goto-char beg)
                (insert message)
                (unless (bolp)
                  (insert "\n"))
                (when (< (point) (point-max))
                  (unless (looking-at "\n")
                    (insert "\n")))
                (setq-local magit-gptel--generated-message message))
              (message "magit-gptel: Commit message inserted"))))))))))

(defun magit-gptel--show-diff-explanation (request response _info)
  "Display diff explanation RESPONSE for REQUEST."
  (let* ((snapshot (magit-gptel-request-snapshot request))
         (text (string-trim response))
         (body (string-join
                (list
                 (format "# Diff Explanation")
                 ""
                 (format "- Repository: `%s`" (plist-get snapshot :repo-name))
                 (format "- Branch: `%s`" (plist-get snapshot :branch))
                 (format "- Scope: `%s`%s"
                         (plist-get snapshot :scope)
                         (if (plist-get snapshot :region-derived)
                             " (explaining enclosing hunk)"
                           ""))
                 (format "- Diff type: `%s`" (plist-get snapshot :diff-type))
                 ""
                 text)
                "\n")))
    (magit-gptel--display-text-buffer
     magit-gptel-explain-buffer-name
     body
     t)
    (message "magit-gptel: Diff explanation ready")))

(defun magit-gptel--handle-response (request response _info)
  "Record one gptel RESPONSE event for REQUEST.

This callback intentionally does not apply or clean up the request.  gptel can
emit provider-specific intermediate events such as reasoning before the final
text, so `magit-gptel--finalize-request' owns the terminal decision."
  (cond
   ((stringp response)
    (push response (magit-gptel-request-response-chunks request)))
   ((magit-gptel--reasoning-response-p response)
    (when (stringp (cdr response))
      (push (cdr response)
            (magit-gptel-request-reasoning-chunks request))))
   ((eq response 'abort)
    (setf (magit-gptel-request-status request) 'cancelled))
   ((or (null response) (eq response t))
    nil)
   (t
    (setf (magit-gptel-request-unsupported-response request)
          response))))

(defun magit-gptel--no-final-response-message (request info)
  "Return a diagnostic for REQUEST when gptel produced no final text."
  (string-join
   (list
    "magit-gptel received no final response text."
    ""
    (if (string-empty-p (magit-gptel--reasoning-text request))
        "The provider completed without a text response."
      "The provider returned reasoning content but no normal response text.")
    ""
    "For commit generation, magit-gptel can recover a Conventional Commit"
    "subject from reasoning-only responses when one is present.  Otherwise use"
    "a model or backend setting that returns normal response content."
    ""
    (format "Repository: %s" (magit-gptel-request-repo-root request))
    (format "Request: %s" (magit-gptel-request-id request))
    (format "Status: %s" (magit-gptel--request-error-string info)))
   "\n"))

(defun magit-gptel--finalize-request (request info)
  "Finalize REQUEST after gptel reaches a terminal state."
  (unwind-protect
      (cond
       ((eq (magit-gptel-request-status request) 'cancelled)
        (message "magit-gptel: Request cancelled"))
       ((plist-get info :error)
        (setf (magit-gptel-request-status request) 'failed)
        (magit-gptel--display-text-buffer
         magit-gptel-preview-buffer-name
         (format "magit-gptel request failed.\n\n%s"
                 (magit-gptel--request-error-string info))))
       ((magit-gptel-request-unsupported-response request)
        (setf (magit-gptel-request-status request) 'failed)
        (magit-gptel--display-text-buffer
         magit-gptel-preview-buffer-name
         (format "magit-gptel received an unsupported gptel response:\n\n%s"
                 (magit-gptel-request-unsupported-response request))))
       (t
        (let ((response (or (and-let* ((text (magit-gptel--response-text request))
                                       ((not (string-empty-p text))))
                              text)
                            (magit-gptel--reasoning-fallback-text request))))
          (if (not response)
              (progn
                (setf (magit-gptel-request-status request) 'failed)
                (magit-gptel--display-text-buffer
                 magit-gptel-preview-buffer-name
                 (magit-gptel--no-final-response-message request info)))
            (setf (magit-gptel-request-status request) 'completed)
            (condition-case err
                (funcall (magit-gptel-request-applied-callback request)
                         request response info)
              (error
               (setf (magit-gptel-request-status request) 'failed)
               (magit-gptel--display-text-buffer
                magit-gptel-preview-buffer-name
                (format "magit-gptel failed to apply a response: %s"
                        (error-message-string err)))))))))
    (magit-gptel--cleanup-request request)))

(defun magit-gptel--install-finalizer (fsm request)
  "Install REQUEST finalization into gptel FSM's post handlers."
  (let* ((info (gptel-fsm-info fsm))
         (post (plist-get info :post)))
    (plist-put info :post
               (append post
                       (list (lambda (terminal-info)
                               (magit-gptel--finalize-request
                                request terminal-info))))))
  fsm)

(defun magit-gptel--dispatch-request (request prompt system-prompt)
  "Send PROMPT for REQUEST with SYSTEM-PROMPT through an isolated gptel buffer."
  (let* ((request-buffer (generate-new-buffer " *magit-gptel-request*"))
         (model (magit-gptel--model-for-request)))
    (setf (magit-gptel-request-request-buffer request) request-buffer
          (magit-gptel-request-status request) 'running)
    (magit-gptel--register-request request)
    (condition-case err
        (with-current-buffer request-buffer
          (let ((gptel-backend (magit-gptel--default-backend))
                (gptel-model model)
                (gptel-use-context nil)
                (gptel-context nil)
                (gptel-use-tools nil)
                (gptel-tools nil)
                (gptel-track-response nil)
                (gptel-prompt-transform-functions nil)
                (gptel-include-reasoning nil))
            (magit-gptel--install-finalizer
             (gptel-request
                 prompt
               :buffer request-buffer
               :system system-prompt
               :stream nil
               :callback (lambda (response info)
                           (magit-gptel--handle-response
                            request response info)))
             request)))
      (error
       (magit-gptel--cleanup-request request)
       (signal (car err) (cdr err))))
    request))

(defun magit-gptel--cancel-request (request &optional silent)
  "Cancel REQUEST.
When SILENT is non-nil, do not emit an extra status message."
  (when request
    (setf (magit-gptel-request-status request) 'cancelled)
    (if-let* ((request-buffer (magit-gptel-request-request-buffer request)))
        (if (buffer-live-p request-buffer)
            (gptel-abort request-buffer)
          (magit-gptel--cleanup-request request))
      (magit-gptel--cleanup-request request))
    (unless silent
      (message "magit-gptel: Request cancelled"))))

;;;###autoload
(defun magit-gptel-cancel ()
  "Cancel the active Magit GPTel request owned by the current buffer."
  (interactive)
  (if-let* ((request (magit-gptel--request-for-buffer)))
      (magit-gptel--cancel-request request)
    (user-error "No active Magit GPTel request for this buffer")))

;;;###autoload
(defun magit-gptel-generate-message ()
  "Generate a commit message for the current repository's staged changes."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (target-buffer (cond
                         ((derived-mode-p 'git-commit-mode) (current-buffer))
                         ((magit-gptel--find-commit-buffer
                           (magit-gptel--repo-root source-buffer)))
                         (t nil))))
    (unless target-buffer
      (user-error "No commit buffer is active for this repository"))
    (when-let* ((request (magit-gptel--request-for-buffer target-buffer)))
      (magit-gptel--cancel-request request t))
    (with-current-buffer target-buffer
      (let* ((repo-root (magit-gptel--repo-root target-buffer))
             (snapshot (magit-gptel--capture-staged-snapshot repo-root))
             (request (magit-gptel-request-create
                       :id (magit-gptel--next-request-id)
                       :kind 'commit-message
                       :repo-root repo-root
                       :source-buffer source-buffer
                       :target-buffer target-buffer
                       :snapshot snapshot
                       :baseline-text (or (git-commit-buffer-message) "")
                       :baseline-tick (buffer-chars-modified-tick)
                       :applied-callback #'magit-gptel--apply-commit-response)))
        (magit-gptel--dispatch-request
         request
         (plist-get snapshot :payload)
         magit-gptel-commit-prompt)
        (message "magit-gptel: Generating commit message...")))))

;;;###autoload
(defun magit-gptel-commit-create (&optional args)
  "Open a commit buffer using ARGS and generate a commit message in it."
  (interactive (list (magit-commit-arguments)))
  (let ((repo-root (magit-gptel--repo-root)))
    (magit-commit-create args)
    (magit-gptel--wait-for-commit-buffer
     repo-root
     (lambda (buffer)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (magit-gptel-generate-message)))))
    (message "magit-gptel: Opening commit buffer...")))

;;;###autoload
(defun magit-gptel-diff-explain ()
  "Explain the current Magit diff selection."
  (interactive)
  (when-let* ((request (magit-gptel--request-for-buffer)))
    (magit-gptel--cancel-request request t))
  (let* ((snapshot (magit-gptel--capture-diff-snapshot))
         (request (magit-gptel-request-create
                   :id (magit-gptel--next-request-id)
                   :kind 'diff-explain
                   :repo-root (plist-get snapshot :repo-root)
                   :source-buffer (current-buffer)
                   :target-buffer (current-buffer)
                   :snapshot snapshot
                   :applied-callback #'magit-gptel--show-diff-explanation)))
    (magit-gptel--dispatch-request
     request
     (plist-get snapshot :payload)
     magit-gptel-diff-explain-prompt)
    (message "magit-gptel: Explaining current diff...")))

(defun magit-gptel--install-transient (prefix anchor suffix)
  "Install SUFFIX into PREFIX after ANCHOR, logging rather than signaling."
  (condition-case err
      (transient-append-suffix prefix anchor suffix)
    (error
     (message "magit-gptel: failed to install %s into %s: %s"
              (car suffix) prefix (error-message-string err)))))

(defun magit-gptel--retire-gptel-magit ()
  "Remove legacy `gptel-magit' bindings from the current session."
  (remove-hook 'magit-mode-hook #'gptel-magit-install)
  (define-key git-commit-mode-map (kbd "M-g") nil)
  (ignore-errors
    (when (fboundp 'transient-remove-suffix)
      (transient-remove-suffix 'magit-commit #'gptel-magit-commit-generate)
      (transient-remove-suffix 'magit-diff #'gptel-magit-diff-explain)
      ;; Clean up by historical keys too in case only autoload symbols exist.
      (transient-remove-suffix 'magit-commit "g")
      (transient-remove-suffix 'magit-diff "x"))))

;;;###autoload
(defun magit-gptel-install ()
  "Install Magit GPTel keybindings and transient entries."
  (interactive)
  (unless magit-gptel--installed
    (magit-gptel--retire-gptel-magit)
    (define-key git-commit-mode-map magit-gptel-commit-buffer-key
                #'magit-gptel-generate-message)
    (define-key git-commit-mode-map magit-gptel-cancel-key
                #'magit-gptel-cancel)
    (magit-gptel--install-transient
     'magit-commit
     #'magit-commit-create
     `(,magit-gptel-commit-transient-key "Generate Commit (gptel)" magit-gptel-commit-create))
    (magit-gptel--install-transient
     'magit-diff
     #'magit-stash-show
     `(,magit-gptel-diff-transient-key "Explain (gptel)" magit-gptel-diff-explain))
    (setq magit-gptel--installed t)))

(when magit-gptel-auto-install
  (magit-gptel-install))

(provide 'magit-gptel)
;;; magit-gptel.el ends here
