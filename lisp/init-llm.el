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
      :models '(glm-5 qwen3-max qwen-plus deepseek-r1 qwen3-coder-plus)))

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
  (setopt gptel-model 'glm-5)

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
  )

(use-package gptel-magit
  :ensure t
  :hook (magit-mode . gptel-magit-install)
  :config
  ;; HACK override this function cause it does not handling (reasoning . text)
  (cl-defun gptel-magit--format-commit-message (message)
    "Format commit message MESSAGE nicely."
    (with-temp-buffer
      (cond
       ((stringp message)
        (insert message))
       ((listp message)
        (insert (cdr message)))
       (t
        (message "Unknown message type")))
      (text-mode)
      (setq fill-column git-commit-summary-max-length)
      (fill-region (point-min) (point-max))
      (buffer-string))))

(provide 'init-llm)
