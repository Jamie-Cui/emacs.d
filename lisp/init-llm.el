;;; init-llm.el --- core functionality support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package agent-shell
  :ensure t)

(use-package gptel
  :ensure t
  :custom
  (gptel-rewrite-default-action 'merge)
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  (gptel-log-level 'info)
  ;; re-bind key
  ;; :bind (:map gptel-mode-map
  ;;             ("C-c C-c" . #'gptel-send)
  ;;             ("C-c RET" . nil))
  :config
  ;; register remote backend
  (defvar +gptel/remote-backend
    (gptel-make-deepseek "bailian.aliyun"
      :host "dashscope.aliyuncs.com/compatible-mode/v1"
      :endpoint "/chat/completions"
      :stream t
      :key (auth-source-pick-first-password :host "bailian.console.aliyun.com")
      :models '(qwen-plus deepseek-r1 qwen3-coder-plus)))

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
  (setopt gptel-backend +gptel/remote-backend)
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
;; HACK Why not melpa? coz it requires vterm and I prefer eat.
;;
;; TODO how to use cluade-code in emacs with local model?
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
