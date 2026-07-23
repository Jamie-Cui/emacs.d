;;; llm.el --- LLM and agent integrations -*- lexical-binding: t -*-
;;; Commentary:
;; LLM and agent integrations: agent-switch, gptel, agent-shell, magent and
;; magit-gptel.
;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; -----------------------------------------------------------
;; DONE llm
;;
;; agent-switch
;; agent-shell
;; gptel
;; magit-gptel
;; -----------------------------------------------------------

(use-package agent-switch
  :vc (:url "https://github.com/Jamie-Cui/agent-switch.el" :rev "main")
  :ensure t
  :commands agent-switch
  :custom
  (agent-switch-authinfo-file (expand-file-name "~/.authinfo"))
  )

;; (use-package gptel-agent
;;   :ensure t
;;   :after gptel
;;   :config
;;   (require 'gptel-agent-tools)
;;   (add-to-list 'gptel-agent-dirs (concat +emacs/repo-directory "/agents"))
;;   (gptel-agent-update)
;;   ;; this package automatically add presets to gptel
;;   ;; @gptel-agent
;;   ;; @gptel-plan
;;   )

(defun +agent-shell/focus-input (shell-buffer)
  "Move point in visible SHELL-BUFFER to the current input's beginning."
  (when-let* ((window (get-buffer-window shell-buffer t)))
    (set-window-point
     window
     (with-current-buffer shell-buffer
       (let ((prompt-end
              (and (boundp 'comint-last-prompt)
                   (cdr-safe (symbol-value 'comint-last-prompt)))))
         (if (and (markerp prompt-end)
                  (marker-position prompt-end))
             (marker-position prompt-end)
           (point-max)))))))

(defun +agent-shell/focus-input-after-display-a (shell-buffer)
  "Move point to the input after displaying SHELL-BUFFER."
  (+agent-shell/focus-input shell-buffer))

(defun +agent-shell/focus-input-after-insert-a (&rest args)
  "Move point after a focused insertion described by ARGS."
  (unless (plist-get args :no-focus)
    (when (derived-mode-p 'agent-shell-mode)
      (+agent-shell/focus-input (current-buffer)))))

(defun +agent-shell/focus-input-when-initialized-h ()
  "Move point after this agent shell finishes initializing."
  (let ((shell-buffer (current-buffer))
        subscription)
    (setq subscription
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'init-finished
           :on-event
           (lambda (_event)
             (+agent-shell/focus-input shell-buffer)
             (agent-shell-unsubscribe :subscription subscription))))))

(use-package agent-shell
  :ensure t
  :custom
  (agent-shell-display-action 'display-buffer-other-window)
  ;; (agent-shell-display-action
  ;;  '((display-buffer-reuse-window
  ;;     display-buffer-use-some-window
  ;;     display-buffer-pop-up-window)
  ;;    (inhibit-same-window . t)))
  (agent-shell-show-welcome-message nil)
  (agent-shell-header-style 'text)
  (agent-shell-show-config-icons nil)
  (agent-shell-thought-process-expand-by-default nil)
  (agent-shell-tool-use-expand-by-default nil)
  (agent-shell-tool-use-group-expand-by-default nil)
  (agent-shell-user-message-expand-by-default nil)
  :config

  (defun +agent-shell/bind-return-in-action-keymap-a (map)
    "Bind GUI <return> in agent-shell action keymaps."
    (when (keymapp map)
      (when-let* ((action (lookup-key map (kbd "RET"))))
        (define-key map (kbd "<return>") action)))
    map)

  (defun +agent-shell/display-transient-below-window (buffer alist)
    "Display transient BUFFER below the selected window using ALIST."
    (let ((window (selected-window)))
      (display-buffer-in-direction
       buffer
       (append `((direction . below) (window . ,window)) alist))))

  (advice-remove 'agent-shell--display-buffer
                 #'+agent-shell/focus-input-after-display-a)
  (advice-add 'agent-shell--display-buffer
              :after #'+agent-shell/focus-input-after-display-a)
  (advice-remove 'agent-shell--insert-to-shell-buffer
                 #'+agent-shell/focus-input-after-insert-a)
  (advice-add 'agent-shell--insert-to-shell-buffer
              :after #'+agent-shell/focus-input-after-insert-a)
  (remove-hook 'agent-shell-mode-hook
               #'+agent-shell/focus-input-when-prompt-ready-h)
  (add-hook 'agent-shell-mode-hook
            #'+agent-shell/focus-input-when-initialized-h)

  (with-eval-after-load 'agent-shell-ui
    (advice-remove 'agent-shell-ui-make-action-keymap
                   #'+agent-shell/bind-return-in-action-keymap-a)
    (advice-add 'agent-shell-ui-make-action-keymap
                :filter-return #'+agent-shell/bind-return-in-action-keymap-a))

  ;; HACK using +emacs/proxy for codex-acp
  (with-eval-after-load 'agent-shell-openai
    (let ((proxy (concat "http://" +emacs/proxy)))
      (setq agent-shell-openai-codex-environment
            (list
             (format "http_proxy=%s" proxy)
             (format "https_proxy=%s" proxy)
             (format "HTTP_PROXY=%s" proxy)
             (format "HTTPS_PROXY=%s" proxy)
             (format "all_proxy=%s" proxy)
             (format "ALL_PROXY=%s" proxy)
             "no_proxy=localhost,127.0.0.1,::1"
             "NO_PROXY=localhost,127.0.0.1,::1"))))

  (with-eval-after-load 'agent-shell-anthropic
    (let ((proxy (concat "http://" +emacs/proxy)))
      (setq agent-shell-anthropic-claude-environment
            (list
             (format "http_proxy=%s" proxy)
             (format "https_proxy=%s" proxy)
             (format "HTTP_PROXY=%s" proxy)
             (format "HTTPS_PROXY=%s" proxy)
             (format "all_proxy=%s" proxy)
             (format "ALL_PROXY=%s" proxy)
             "no_proxy=localhost,127.0.0.1,::1"
             "NO_PROXY=localhost,127.0.0.1,::1"))))

  ;; HACK using sssaicode api key

  ;; (defun +agent-shell/sss-api-key ()
  ;;   "Return the SSS API key used by Codex."
  ;;   (or (getenv "SSS_API_KEY")
  ;;       (user-error
  ;;        "SSS_API_KEY is not available in Emacs; restart Emacs or run `exec-path-from-shell-copy-env'")))

  ;; (with-eval-after-load 'agent-shell-openai
  ;;   (setq agent-shell-openai-authentication
  ;;         (agent-shell-openai-make-authentication
  ;;          :api-key #'+agent-shell/sss-api-key)
  ;;         agent-shell-openai-codex-acp-command
  ;;         '("codex-acp"
  ;;           "-c" "model_provider=\"sss\""
  ;;           "-c" "preferred_auth_method=\"apikey\"")))
  )

(use-package agent-shell-permission-transient
  :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/"))
  :after agent-shell
  :demand t
  :bind (:map agent-shell-mode-map
              ("C-c C-p" . agent-shell-permission-transient-menu))
  :config
  (defun +agent-shell/permissions-pending-p ()
    "Return non-nil when agent-shell has a pending permission request."
    (> (agent-shell-permission-transient-pending-count) 0))

  (defun +agent-shell/configure-help-menu ()
    "Add common agent-shell actions to `agent-shell-help-menu'."
    (when-let* ((prefix (get 'agent-shell-help-menu 'transient--prefix)))
      (oset prefix display-action
            '(+agent-shell/display-transient-below-window
              (dedicated . t)
              (inhibit-same-window . t))))
    (dolist (command '(agent-shell-ui-toggle-fragment
                       agent-shell-ui-toggle-all-fragments
                       agent-shell-restart
                       agent-shell-reload
                       agent-shell-fork
                       agent-shell-permission-transient-menu
                       agent-shell-switch-buffer
                       agent-shell-other-buffer))
      (transient-remove-suffix 'agent-shell-help-menu command))
    (transient-append-suffix
      'agent-shell-help-menu 'agent-shell-previous-item
      '("z" "Toggle item" agent-shell-ui-toggle-fragment :transient t))
    (transient-append-suffix
      'agent-shell-help-menu 'agent-shell-ui-toggle-fragment
      '("Z" "Toggle all" agent-shell-ui-toggle-all-fragments :transient t))
    (transient-append-suffix
      'agent-shell-help-menu 'agent-shell-interrupt
      '("r" "Restart" agent-shell-restart))
    (transient-append-suffix
      'agent-shell-help-menu 'agent-shell-restart
      '("R" "Reload" agent-shell-reload))
    (transient-append-suffix
      'agent-shell-help-menu 'agent-shell-reload
      '("f" "Fork" agent-shell-fork))
    (transient-append-suffix
      'agent-shell-help-menu 'agent-shell-fork
      '("P" "Permissions" agent-shell-permission-transient-menu
        :if +agent-shell/permissions-pending-p))
    (transient-append-suffix
      'agent-shell-help-menu 'agent-shell-new-shell
      '("s" "Switch shell" agent-shell-switch-buffer))
    (transient-append-suffix
      'agent-shell-help-menu 'agent-shell-switch-buffer
      '("O" "Shell/viewport" agent-shell-other-buffer)))

  (+agent-shell/configure-help-menu)

  (with-eval-after-load 'evil
    (evil-define-key 'normal agent-shell-mode-map (kbd "?")
      #'agent-shell-help-menu))

  (agent-shell-permission-transient-mode +1))

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

  ;; register deepseek backend
  (defvar +llm/deepseek
    (gptel-make-deepseek "DeepSeek"
      :key (auth-source-pick-first-password :host "deepseek")
      :stream t))

  ;; register gemini backend
  (defvar +llm/gemini
    (gptel-make-gemini "Gemini"
      :key (auth-source-pick-first-password :host "gemini")
      :stream t))

  ;; register aliyun backend
  (defvar +llm/aliyun
    (gptel-make-deepseek "Aliyun"
      :host "dashscope.aliyuncs.com/compatible-mode/v1"
      :endpoint "/chat/completions"
      :stream t
      :key (auth-source-pick-first-password :host "aliyun")
      :models '(
                qwen3.7-plus
                (qwen3.7-max :request-params (:enable_thinking t))
                )))

  (defvar +llm/sssaicode
    (gptel-make-openai "SssAiCode"
      :host "https://codex1.sssaicode.com/api/v1"
      :endpoint "/chat/completions"
      :stream t
      :key (auth-source-pick-first-password :host "sssaicode")
      :models '(gpt-5.4)))

  ;; register zhipu backend
  (defvar +llm/zhipu
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
  (setopt gptel-backend +llm/deepseek)
  (setopt gptel-model 'deepseek-v4-pro)

  ;; set context
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "=@Jamie=\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "=@AI=\n")

  ;; set hook
  (add-hook 'gptel-mode-hook
            (lambda () (insert "* Default Context\n=@Jamie=")))

  (defun +llm/remove-headings (beg end)
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

  (add-hook 'gptel-post-response-functions #'+llm/remove-headings)

  (use-package magit-gptel
    :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/"))
    :after (gptel magit)
    :demand t
    :custom
    (magit-gptel-model 'deepseek-v4-flash)
    (magit-gptel-request-params '(:thinking (:type "disabled")
                                            :temperature 0.1))
    :config
    ;; Reset from the fixed package default so reloading this module never
    ;; appends another copy of the commit-message requirements.
    (setopt magit-gptel-commit-prompt
            magit-gptel--default-commit-prompt))

  ;; -----------------------------------------------------------
  ;; PlantUML Beautification (using gptel-rewrite)
  ;; -----------------------------------------------------------

  (defvar +llm/beautify-plantuml-directive
    "You are a PlantUML expert. Beautify and improve the PlantUML diagram while preserving its semantic meaning. Improve layout, add appropriate styling/colors, organize elements logically, add skinparams for professional appearance. Return ONLY the improved PlantUML code without any explanations or markdown formatting."
    "Rewrite directive for PlantUML beautification.")

  (defun +llm/beautify-plantuml ()
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
        (let ((gptel--rewrite-directive +llm/beautify-plantuml-directive))
          (gptel--suffix-rewrite)))))
  )

(use-package magent
  :vc (:url "https://github.com/Jamie-Cui/magent" :rev "master")
  :ensure t
  :after (agent-shell gptel)
  :demand t
  :custom
  ;; (magent-bypass-permission t)
  (magent-default-effort 'xhigh)
  :config
  (add-to-list 'magent-skill-directories
               (expand-file-name "skills" +emacs/repo-directory) t)
  (magent-agent-shell-ensure-config))

(use-package magent-submit-pr
  :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/"))
  :after magent
  :demand t
  :config
  (magent-submit-pr-register))


(provide 'init-llm)
;;; llm.el ends here
