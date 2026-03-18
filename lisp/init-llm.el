;;; init-llm.el --- core functionality support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -----------------------------------------------------------
;; DONE llm
;;
;; agent-shell
;; gptel
;; gptel-magit
;; -----------------------------------------------------------

(use-package gptel-agent
  :ensure t
  :after gptel
  :config
  (require 'gptel-agent-tools)
  (add-to-list 'gptel-agent-dirs (concat +emacs/repo-directory "/agents"))
  (gptel-agent-update)
  ;; this package automatically add presets to gptel
  ;; @gptel-agent
  ;; @gptel-plan
  )

(use-package agent-shell
  :ensure t
  :custom
  (agent-shell-show-welcome-message nil)
  (agent-shell-header-style 'text)
  (agent-shell-show-config-icons nil))

(use-package gptel
  :ensure t
  :custom
  (gptel-rewrite-default-action 'merge)
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  (gptel-log-level 'info)
  (gptel-proxy +emacs/proxy)
  ;; re-bind key
  :bind (:map gptel-mode-map
              ("C-c C-c" . #'gptel-send)
              ("C-c RET" . #'gptel-menu))
  :config
  ;; use smae window to display gptel buffer
  (setq gptel-display-buffer-action '(display-buffer-same-window))

  ;; register gemini backend
  (gptel-make-gemini "Gemini"
    :key (auth-source-pick-first-password :host "gemini")
    :stream t)

  ;; register aliyun backend
  (defvar +gptel/aliyun
    (gptel-make-deepseek "Aliyun"
      :host "dashscope.aliyuncs.com/compatible-mode/v1"
      :endpoint "/chat/completions"
      :stream t
      :key (auth-source-pick-first-password :host "aliyun")
      :models '((qwen3-coder-next :request-params (:enable_thinking t))
                glm-5
                (qwen3-max :request-params (:enable_thinking t))
                qwen-plus
                deepseek-r1
                qwen3-coder-plus))))

(defvar +gptel/sssaicode
  (gptel-make-openai "SssAiCode"
    :host "https://codex1.sssaicode.com/api/v1"
    :endpoint "/chat/completions"
    :stream t
    :key (auth-source-pick-first-password :host "sssaicode")
    :models '(gpt-5.4)))

;; register zhipu backend
(defvar +gptel/zhipu
  (gptel-make-deepseek "Zhipu"
    :host "open.bigmodel.cn/api/coding/paas/v4"
    :endpoint "/chat/completions"
    :stream t
    :key (auth-source-pick-first-password :host "zhipu")
    :models '(glm-4.7)))

;; register local backend
;; NOTE to make ollama work through LAN, on its server
;; see: https://github.com/ollama/ollama/blob/main/docs/faq.md
;; (defvar +gptel/local-backend
;;   (gptel-make-openai "OpenWebUI"
;;     :host "localhost:8080"
;;     :protocol "http"
;;     :endpoint "/api/chat/completions"
;;     :stream t
;;     :key "sk-02bdf77754894f87b8988711c7d15b67"
;;     :models '(qwen2.5-coder:latest)))

;; set default values
(setopt gptel-backend +gptel/aliyun)
(setopt gptel-model 'qwen3-max)

;; set context
(setf (alist-get 'org-mode gptel-prompt-prefix-alist) "=@Jamie=\n")
(setf (alist-get 'org-mode gptel-response-prefix-alist) "=@AI=\n")

;; set hook
(add-hook 'gptel-mode-hook
          (lambda () (insert "* Default Context\n=@Jamie=")))

(defun +gptel/remove-headings (beg end)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward org-heading-regexp end t)
        (forward-line 0)
        (delete-char (1+ (length (match-string 1))))
        (insert-and-inherit "*")
        (end-of-line)
        (skip-chars-backward " \t\r")
        (insert-and-inherit "*")))))

(add-hook 'gptel-post-response-functions #'+gptel/remove-headings)

;; -----------------------------------------------------------
;; PlantUML Beautification (using gptel-rewrite)
;; -----------------------------------------------------------

(defvar +gptel/beautify-plantuml-directive
  "You are a PlantUML expert. Beautify and improve the PlantUML diagram while preserving its semantic meaning. Improve layout, add appropriate styling/colors, organize elements logically, add skinparams for professional appearance. Return ONLY the improved PlantUML code without any explanations or markdown formatting."
  "Rewrite directive for PlantUML beautification.")

(defun gptel-beautify-plantuml ()
  "Beautify PlantUML source block at point using gptel-rewrite.
This selects the PlantUML code region and invokes gptel's rewrite
functionality, allowing you to diff/ediff/merge the changes."
  (interactive)
  (require 'gptel-rewrite)
  ;; 1. Validate we're in org-mode
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))

  ;; 2. Validate we're in a PlantUML source block
  (let* ((info (org-babel-get-src-block-info))
         (lang (car info)))
    (unless info
      (user-error "Not in a source block"))
    (unless (string= lang "plantuml")
      (user-error "Not in a PlantUML block (current: %s)" lang))

    ;; 3. Select the code region
    (let ((code-start (save-excursion
                        (org-babel-goto-src-block-head)
                        (forward-line 1)
                        (point)))
          (code-end (save-excursion
                      (org-babel-goto-src-block-head)
                      (re-search-forward "^[ \t]*#\\+end_src")
                      (match-beginning 0))))
      ;; 4. Set region and invoke gptel-rewrite
      (goto-char code-start)
      (push-mark code-end t t)
      (let ((gptel--rewrite-directive +gptel/beautify-plantuml-directive))
        (gptel--suffix-rewrite)))))

(use-package gptel-magit
  :ensure t
  :hook (magit-mode . gptel-magit-install)
  :config
  (setopt gptel-magit-model 'qwen-plus)
  (setopt gptel-magit-commit-prompt
          (concat
           gptel-magit-prompt-conventional-commits
           "\n\nAdditional hard requirements for this setup:\n"
           "- The first line MUST fit within 50 characters, counting all spaces and punctuation\n"
           "- The first line MUST already be a single line before any editor wrapping or filling\n"
           "- If needed, abbreviate aggressively and drop secondary details to satisfy the limit\n"
           "- If your first attempt is longer than 50 characters, rewrite it until it is 50 characters or shorter\n"
           "- The body MAY be longer, but only after one blank line following the first line\n"
           "- Return plain text only\n"
           "- Never use markdown, code fences, or labels such as ```commit\n"))

  ;; gptel-magit expects a single final response string.  Disable streaming and
  ;; reasoning blocks for commit generation so the callback does not receive
  ;; chunked text or (reasoning . text) payloads.
  (cl-defun gptel-magit--request (&rest args)
    "Call `gptel-request' with ARGS using magit-safe response settings."
    (declare (indent 1))
    (let* ((gptel-backend (or gptel-magit-backend gptel-backend))
           (gptel-model (or gptel-magit-model gptel-model))
           (gptel-stream nil)
           (gptel-include-reasoning nil)
           (model-sym (and (symbolp gptel-model) gptel-model))
           (had-request-params
            (and model-sym (plist-member (symbol-plist model-sym) :request-params)))
           (original-request-params
            (and model-sym (get model-sym :request-params))))
      (unwind-protect
          (progn
            (when model-sym
              (put model-sym :request-params
                   (plist-put (copy-sequence original-request-params)
                              :enable_thinking :json-false)))
            (apply #'gptel-request args))
        (when model-sym
          (if had-request-params
              (put model-sym :request-params original-request-params)
            (remprop model-sym :request-params))))))

  ;; Keep upstream formatting behavior, but strip fenced code wrappers that
  ;; some models add around the commit message.
  (defun +gptel-magit--strip-fences (text)
    "Remove surrounding fenced code blocks from TEXT."
    (let ((text (or text "")))
      (setq text
            (replace-regexp-in-string
             "\\`[[:space:]\n\r]*```[[:alnum:]_-]*[ \t]*\n?" ""
             text))
      (replace-regexp-in-string
       "\n?```[ \t]*\\'" ""
       text)))

  (defun +gptel-magit--split-message (text)
    "Split commit message TEXT into subject and optional body."
    (let* ((text (string-trim (or text "")))
           (match (string-match "\n[ \t]*\n" text)))
      (if match
          (list (substring text 0 match)
                (substring text (match-end 0)))
        (list text nil))))

  (defun +gptel-magit--shorten-subject (subject &optional max-length)
    "Shrink SUBJECT to MAX-LENGTH with cheap local abbreviations."
    (let* ((max-length (or max-length 50))
           (case-fold-search t)
           (subject (string-trim (or subject "")))
           (replacements '(("\\_<configurations?\\_>" . "cfg")
                           ("\\_<format\\(ting\\)?\\_>" . "fmt")
                           ("\\_<messages?\\_>" . "msg")
                           ("\\_<reorganize\\_>" . "reorg")
                           ("\\_<improve\\_>" . "impr")
                           ("\\_<enhance\\_>" . "enh")
                           ("\\_<initialize\\_>" . "init")
                           ("\\_<integration\\_>" . "intg")
                           ("\\_<temporary\\_>" . "temp")
                           ("\\_<reasoning\\_>" . "reason")
                           ("\\_<streaming\\_>" . "stream"))))
      (dolist (replacement replacements)
        (setq subject
              (replace-regexp-in-string
               (car replacement) (cdr replacement) subject t)))
      (setq subject
            (replace-regexp-in-string "[[:space:]]+" " " subject))
      (if (<= (length subject) max-length)
          subject
        (let* ((matched (string-match "\\`\\([^:]+: \\)\\(.*\\)\\'" subject))
               (prefix (if matched (match-string 1 subject) ""))
               (desc   (if matched (match-string 2 subject) subject))
               (limit (max 0 (- max-length (length prefix))))
               (words (split-string desc " " t))
               (result ""))
          (dolist (word words)
            (let ((candidate (if (string-empty-p result)
                                 word
                               (concat result " " word))))
              (when (<= (length candidate) limit)
                (setq result candidate))))
          (string-trim-right (concat prefix result))))))

  (defun +gptel-magit--fill-body-line (line &optional fill-width)
    "Wrap LINE to FILL-WIDTH while preserving bullet prefixes."
    (let ((fill-width (or fill-width 72)))
      (cond
       ((string-empty-p (string-trim line))
        "")
       ((string-match "\\`\\([[:space:]]*[-*+] \\)\\(.*\\)\\'" line)
        (let ((prefix (match-string 1 line))
              (content (match-string 2 line)))
          (with-temp-buffer
            (insert prefix content)
            (setq fill-column fill-width)
            (setq fill-prefix (make-string (length prefix) ?\s))
            (fill-region (point-min) (point-max))
            (buffer-string))))
       (t
        (with-temp-buffer
          (insert line)
          (setq fill-column fill-width)
          (fill-region (point-min) (point-max))
          (buffer-string))))))

  (defun +gptel-magit--fill-body (body &optional fill-width)
    "Wrap BODY to FILL-WIDTH while preserving bullet item boundaries."
    (mapconcat (lambda (line)
                 (+gptel-magit--fill-body-line line fill-width))
               (split-string (or body "") "\n")
               "\n"))

  (cl-defun gptel-magit--format-commit-message (message)
    "Format commit message MESSAGE nicely."
    (pcase-let* ((raw (pcase message
                        ((pred stringp) message)
                        (`(reasoning . ,text) text)
                        (_ "")))
                 (clean (+gptel-magit--strip-fences raw))
                 (`(,subject ,body) (+gptel-magit--split-message clean))
                 (subject (replace-regexp-in-string
                           "[[:space:]\n\r]+"
                           " "
                           (string-trim subject)))
                 (subject (+gptel-magit--shorten-subject subject 50))
                 (body (and body
                            (not (string-empty-p (string-trim body)))
                            (+gptel-magit--fill-body body 72))))
      (concat subject
              (if body
                  (concat "\n\n" body)
                ""))))
  )

(provide 'init-llm)
