;;; init-llm.el --- core functionality support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(+package/ensure-install
 '(
   ;; llm client
   gptel
   ;; add-ons
   gptel-magit
   ))

(use-package gptel
  :ensure t
  :custom
  (gptel-rewrite-default-action 'merge)
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  (gptel-log-level 'info)
  :config

  ;; register remote backend
  (defvar +gptel/remote-backend
    (gptel-make-deepseek "bailian.aliyun"
      :host "dashscope.aliyuncs.com/compatible-mode/v1"
      :endpoint "/chat/completions"
      :stream t
      :key (auth-source-pick-first-password :host "bailian.console.aliyun.com")
      :models '(qwen-plus deepseek-r1)))

  ;; register local backend
  (defvar +gptel/local-backend
    (gptel-make-ollama "ollama"        
      :host "localhost:11434"           
      :stream t                          
      :models '(deepseek-r1:7b)))

  ;; set default values
  (setopt gptel-backend +gptel/local-backend)
  (setopt gptel-model 'deepseek-r1:7b)

  ;; set context
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "=@Jamie=\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "=@AI=\n")

  ;; set hook
  (add-hook 'gptel-mode-hook
            (lambda ()
              (insert "** Default Context\n=@Jamie=")
              ))
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

;; using claude-code from foreign api
;; see: https://help.aliyun.com/zh/model-studio/claude-code
;; 
;; just in case you want claude to work in bash shell terminal, you need
;; export ANTHROPIC_BASE_URL=https://dashscope.aliyuncs.com/api/v2/apps/claude-code-proxy
;; export ANTHROPIC_AUTH_TOKEN=sk-***************************
;; 
;; HACK Why not melpa? coz it requires vterm and I prefer eat.
;;
(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (setopt claude-code-terminal-backend 'eat) ;; this is the default
  (setenv "ANTHROPIC_BASE_URL" "https://dashscope.aliyuncs.com/api/v2/apps/claude-code-proxy")
  (setenv "ANTHROPIC_AUTH_TOKEN" (cadr (auth-source-user-and-password "bailian.console.aliyun.com")))
  )

(provide 'init-llm)
