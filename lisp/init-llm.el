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
  :config
  (setq gptel-model 'qwen-plus
        gptel-default-mode 'org-mode
        gptel-org-branching-context t
        gptel-log-level 'info
        gptel-backend
        (gptel-make-deepseek "bailian.aliyun"
          :host "dashscope.aliyuncs.com/compatible-mode/v1"
          :endpoint "/chat/completions"
          :stream t
          :key (auth-source-pick-first-password :host "bailian.console.aliyun.com")
          :models '(qwen-plus deepseek-r1)
          ))
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "=@Jamie=\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "=@AI=\n")

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

;; see: https://help.aliyun.com/zh/model-studio/claude-code
;; in bash, you need
;; export ANTHROPIC_BASE_URL=https://dashscope.aliyuncs.com/api/v2/apps/claude-code-proxy
;; export ANTHROPIC_AUTH_TOKEN=

;; HACK Why not melpa? coz it requires vterm and i prefer eat.
;; (use-package claude-code
;;   :ensure t
;;   :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
;;   :config
;;   (setq claude-code-terminal-backend 'eat) ;; this is the default
;;   )

(provide 'init-llm)
