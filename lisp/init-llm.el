;;; init-llm.el --- core functionality support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)

(+ensure-packages-installed
 '(
   ;; llm client
   gptel
   ;; TODO cursor-like coding assistent
   ;; aidermacs
   ))


(use-package gptel
  :ensure t
  :config
  (setq gptel-model   'deepseek-r1
        gptel-default-mode 'org-mode
        gptel-org-branching-context t
        gptel-log-level 'info
        gptel-backend
        (gptel-make-deepseek "DeepSeek"
          :host "dashscope.aliyuncs.com/compatible-mode/v1"
          :endpoint "/chat/completions"
          :stream t
          :key (auth-source-pick-first-password :host "api.deepseek.com")
          :models '(deepseek-r1)))

  (general-define-key
   :keymaps 'gptel-mode-map
   "C-c C-c" #'gptel-send)

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@jamie\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@deepseek\n"))

;; (use-package aidermacs
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :config
;;   (setenv "ANTHROPIC_API_KEY" "sk-...")
;;   (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
;;   :custom
;;   (aidermacs-default-chat-mode 'architect)
;;   (aidermacs-default-model "sonnet"))

(provide 'init-llm)
