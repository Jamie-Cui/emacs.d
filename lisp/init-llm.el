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
  (agent-shell-show-welcome-message nil))

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
              ("C-c RET" . #'gptel-send))
  :config

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
      :models '(qwen3-max qwen-plus deepseek-r1 qwen3-coder-plus)))

  ;; register local backend
  ;; NOTE to make ollama work through LAN, on its server
  ;; see: https://github.com/ollama/ollama/blob/main/docs/faq.md
  (defvar +gptel/local-backend
    (gptel-make-openai "OpenWebUI"
      :host "localhost:8080"
      :protocol "http"
      :endpoint "/api/chat/completions"
      :stream t
      :key "sk-02bdf77754894f87b8988711c7d15b67"
      :models '(qwen2.5-coder:latest)))

  ;; set default values
  (setopt gptel-backend +gptel/aliyun)
  (setopt gptel-model 'qwen3-coder-plus)

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

  (add-hook 'gptel-post-response-functions #'+gptel/remove-headings))

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

;; using claude-code from foreign api
;; see: https://help.aliyun.com/zh/model-studio/claude-code
;; 
;; just in case you want claude to work in bash shell terminal, you need
;; export ANTHROPIC_BASE_URL=https://dashscope.aliyuncs.com/api/v2/apps/claude-code-proxy
;; export ANTHROPIC_AUTH_TOKEN=sk-***************************
;; 
;; NOTE Why not melpa? coz it requires vterm and I prefer eat.
;;
;; NOTE How to use cluade-code in emacs with local model?
;; see: https://github.com/musistudio/claude-code-router/tree/main
;;
;; (use-package claude-code
;;   :load-path (lambda () (concat +emacs/repo-directory "/thirdparty/claude-code.el/"))
;;   :custom 
;;   (claude-code-toggle-auto-select t)
;;   :config
;;   (setopt claude-code-terminal-backend 'eat) ;; this is the default
;;   (setenv "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC" "1") ;; improves speed
;;   (setenv "ANTHROPIC_BASE_URL" "https://dashscope.aliyuncs.com/api/v2/apps/claude-code-proxy")
;;   (setenv "ANTHROPIC_AUTH_TOKEN" (cadr (auth-source-user-and-password "bailian.console.aliyun.com")))
;;   )

(provide 'init-llm)
