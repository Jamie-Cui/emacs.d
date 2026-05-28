;;; init-llm.el --- core functionality support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -----------------------------------------------------------
;; DONE llm
;;
;; agent-shell
;; gptel
;; magit-gptel
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
  (agent-shell-display-action
   '((display-buffer-reuse-window
      display-buffer-use-some-window
      display-buffer-pop-up-window)
     (inhibit-same-window . t)))
  (agent-shell-show-welcome-message nil)
  (agent-shell-header-style 'text)
  (agent-shell-show-config-icons nil)
  :config
  (defun +agent-shell/sss-api-key ()
    "Return the SSS API key used by Codex."
    (or (getenv "SSS_API_KEY")
        (user-error
         "SSS_API_KEY is not available in Emacs; restart Emacs or run `exec-path-from-shell-copy-env'")))

  (with-eval-after-load 'agent-shell-openai
    (setq agent-shell-openai-authentication
          (agent-shell-openai-make-authentication
           :api-key #'+agent-shell/sss-api-key)
          agent-shell-openai-codex-acp-command
          '("codex-acp"
            "-c" "model_provider=\"sss\""
            "-c" "preferred_auth_method=\"apikey\""))))

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
  ;; Display gptel buffers outside the current window.
  (setq gptel-display-buffer-action
        '((display-buffer-reuse-window
           display-buffer-use-some-window
           display-buffer-pop-up-window)
          (inhibit-same-window . t)))

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

(use-package magit-gptel
  :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/"))
  :after (gptel magit)
  :demand t
  :custom
  (magit-gptel-model 'qwen-plus)
  :config
  (setopt magit-gptel-commit-prompt
          (concat
           magit-gptel-commit-prompt
           "\n\nAdditional hard requirements for this setup:\n"
           "- The first line MUST fit within 50 characters, counting all spaces and punctuation\n"
           "- The first line MUST already be a single line before any editor wrapping or filling\n"
           "- If needed, abbreviate aggressively and drop secondary details to satisfy the limit\n"
           "- If your first attempt is longer than 50 characters, rewrite it until it is 50 characters or shorter\n"
           "- The body MAY be longer, but only after one blank line following the first line\n"
           "- Return plain text only\n"
           "- Never use markdown, code fences, or labels such as ```commit\n")))

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

(use-package magent
  :vc (:url "https://github.com/Jamie-Cui/magent" :rev "master")
  :ensure t
  :after gptel spinner
  :demand t
  :custom
  (magent-skill-directories (list (expand-file-name "skills" +emacs/repo-directory)))
  (magent-by-pass-permission t)
  ;; (magent-ui-wrap-reasoning-in-think-block nil)
  :init
  (let ((had-evil-define-key (fboundp 'evil-define-key))
        (evil-define-key-function (and (fboundp 'evil-define-key)
                                       (symbol-function 'evil-define-key))))
    (unwind-protect
        (progn
          (when (and (macrop 'evil-define-key)
                     (fboundp 'evil-define-key*))
            (fset 'evil-define-key
                  (lambda (state keymap key def &rest bindings)
                    (apply #'evil-define-key*
                           state keymap key def bindings))))
          (require 'magent))
      (if had-evil-define-key
          (fset 'evil-define-key evil-define-key-function)
        (fmakunbound 'evil-define-key))))
  :config
  (require 'magent-config)
  (global-magent-mode 1)

  ;; keybindings that should not be overriden
  (general-define-key
   :keymaps 'magent-output-mode-map
   :states '(normal visual motion)
   "?"   #'magent-transient-menu
   ))

(provide 'init-llm)
