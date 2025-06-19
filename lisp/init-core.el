;;; init-core.el --- core functionality support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)

(+ensure-packages-installed
 '(
   consult
   ;; show helps of fun, key, mode
   helpful
   ;; search engine
   vertico
   orderless
   ;; project engine
   projectile
   ;; complete engine
   corfu
   ;; better terminal emulater
   ;; vterm
   ;; eat
   ;; the killer app: git ui
   magit
   ;; lsp
   eglot
   consult-eglot
   flycheck-eglot
   ;; highlight todo keywords
   hl-todo
   ;; search tool based on ripgrep
   rg
   ;; better error checking
   flycheck
   flycheck-popup-tip
   ;; dashboard at startup
   dashboard
   ;; icons
   nerd-icons
   ;; modeline
   doom-modeline
   ;; cpplint
   flycheck-google-cpplint
   ;; show key frenquency
   keyfreq
   ;; llm client
   gptel
   ;; adds marginalia to the minibuffer completions
   marginalia
   ;; make line-break look nicer
   page-break-lines
   ;; Colorize color names in buffers
   ;;  rainbow-mode
   ;; code auto formating
   apheleia
   ;; Emacs Mini-Buffer Actions Rooted in Keymaps
   embark
   embark-consult
   ;; colorful compilation output
   ansi-color
   ;; make eldoc looks nicer
   eldoc-box
   ;; pdf tools
   pdf-tools
   ;; latex support 
   ;;  auctex
   ;; better snippet
   yasnippet
   consult-yasnippet
   ;; workspace
   perspective
   ;; smart-parens
   smartparens
   ;; popup window
   popwin
   ))

(use-package popwin
  :ensure t
  :custom 
  (popwin:popup-window-height 0.5)
  ;; HACK redefine special display rule
  (popwin:special-display-config
   '(;; Emacs
     ;; ("*Miniedit Help*" :noselect t)
     help-mode
     ;; (completion-list-mode :noselect t)
     ;; (compilation-mode :noselect t)
     ;; (grep-mode :noselect t)
     ;; (occur-mode :noselect t)
     ;; "*Shell Command Output*"
     ("*Flycheck errors*" :stick t)
     ;; ("*DeepSeek*" :stick t)
     (helpful-mode :stick t)
     )
   )
  :config
  (popwin-mode 1))

(use-package smartparens
  :ensure t  ;; install the package
  :hook (prog-mode text-mode markdown-mode org-mode) 
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package perspective
  :ensure t
  :custom 
  (persp-suppress-no-prefix-key-warning t)
  (persp-sort 'created)
  (persp-modestring-dividers '("[ " " ]" " | "))
  :init
  (persp-mode)
  )


(use-package gptel
  :ensure t
  :config
  (setq gptel-model   'deepseek-r1
        gptel-default-mode 'org-mode
        gptel-org-branching-context 't
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

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "** @jc\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "** @ai\n"))

;; (use-package auctex
;;   :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (require 'consult-yasnippet)
  (let ((my-yas-dir (concat jc-emacs-directory "/snippets")))
    (add-to-list 'yas-snippet-dirs my-yas-dir))
  ;; start mode globally
  (yas-global-mode 1)
  ;; reload all snippets
  (yas-reload-all)
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  )

(use-package eldoc-box
  :ensure t
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

(use-package ansi-color
  :ensure t
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package iedit
  :ensure t
  :init
  ;; Fix conflict with embark.
  (setq iedit-toggle-key-default nil))

(use-package embark
  :ensure t
  :bind
  (("C-;" . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package flycheck-popup-tip
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  ;; HACK use elgot-format when possible
  ;; https://github.com/radian-software/apheleia/issues/153#issuecomment-1446651497
  (cl-defun apheleia-indent-eglot-managed-buffer
      (&key buffer scratch callback &allow-other-keys)
    "Copy BUFFER to SCRATCH, then format scratch, then call CALLBACK."
    (with-current-buffer scratch
      (setq-local eglot--cached-server
                  (with-current-buffer buffer
                    (eglot-current-server)))
      (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
        (eglot-format-buffer))
      (funcall callback)))
  (add-to-list 'apheleia-formatters
               '(eglot-managed . apheleia-indent-eglot-managed-buffer))
  ;; HACK add all eglot-ensured modes 
  ;; This determines what formatter to use in buffers without a
  ;; setting for apheleia-formatter. The keys are major mode
  (add-to-list 'apheleia-mode-alist '(c++-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(rust-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(cmake-mode . cmake-format))
  )

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; flycheck has performace issues, make it less automate
  (flycheck-check-syntax-automatically '(save mode-enable idle-change))
  (flycheck-idle-change-delay 4)
  )

;; (use-package rainbow-mode
;;   :ensure t
;;   :hook (emacs-lisp-mode text-mode lisp-mode cc-mode cmake-mode))

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode 1))

(use-package flycheck-google-cpplint
  :ensure t
  :custom
  (flycheck-c/c++-googlelint-executable "cpplint")
  (flycheck-googlelint-verbose "0")
  (flycheck-cppcheck-standards "c++17")
  (flycheck-googlelint-linelength "80")
  (flycheck-googlelint-filter
   (concat
    "-whitespace,"
    "-whitespace/braces,"
    "-whitespace/indent,"
    "-build/include_order,"
    "-build/header_guard,"
    "-runtime/reference,"
    )))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :custom
  (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1)
  ;; see: https://github.com/kkholst/.doom.d/blob/main/config.org
  ;; We need to tweak a little bit to make cpplint and eglot to work
  ;; together.
  ;; see: https://melpa.org/#/flycheck-eglot
  ;;
  ;; see: https://github.com/flycheck/flycheck-eglot
  ;; By default, the Flycheck-Eglot considers the Eglot to be the only
  ;; provider of syntax checks. Other Flycheck checkers are ignored.
  ;; There is a variable `flycheck-eglot-exclusive' that controls this
  ;; You can override it system wide or for some major modes.
  ;;
  (flycheck-add-next-checker 'eglot-check
                             '(warning . c/c++-googlelint))
  )

(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.
  ;; To make the binding ;; available in the *Completions* buffer,
  ;; add it to the `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package
  ;; such that ;; the mode gets enabled right away. Note that this
  ;; forces loading the package.
  (marginalia-mode))

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  :config
  (setq doom-modeline-icon nil) ; optional
  ;; (if (facep 'mode-line-active)
  ;;     (set-face-attribute 'mode-line-active nil :family "0xProto Nerd Font Mono 14") ; For 29+
  ;;   (set-face-attribute 'mode-line nil :family "0xProto Nerd Font Mono 14"))
  ;; (set-face-attribute 'mode-line-inactive nil :family "0xProto Nerd Font Mono 14")
  (doom-modeline-mode 1)
  )

(use-package nerd-icons
  :ensure t)

(use-package dashboard
  :ensure t
  :custom
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook)

  ;; HACK from https://github.com/emacs-dashboard/emacs-dashboard/issues/153#issuecomment-714406661
  (defvar my-banners-dir (concat jc-emacs-directory "/data/"))
  (defun install-banners ()
    "Copy all files under under banners directory to dashboard banners directory"
    (when (boundp 'dashboard-banners-directory)
      (copy-directory my-banners-dir dashboard-banners-directory nil nil t)))
  (install-banners)

  ;; Set the title  
  (setq dashboard-banner-logo-title "It's possible to build a cabin with no foundations, but not a lasting building.")
  ;; (setq dashboard-page-separator "\n\f\n")

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-startup-banner 4) ; 4 means using 4.txt
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)))
  (setq dashboard-projects-backend 'projectile)
  (setq initial-buffer-choice (lambda() (dashboard-open)))
  (setq dashboard-after-initialize-hook (lambda() (dashboard-open)))
  )

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1)
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("DONE" success bold)
          ("BUG" error bold)
          )))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preview-current 'nil) ; do not insert unless i select it
  (corfu-preselect 'nil) ; do not preselect anything
  (corfu-quit-no-match 'separator) ;; or t
  :config
  (global-corfu-mode)
  )

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (partial-completion))))))

(use-package consult-eglot
  :ensure t)

(use-package eglot
  :ensure t
  :config
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (setq eglot-confirm-server-initiated-edits nil)
  )

(use-package magit
  :ensure t
  :after evil-collection
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (general-define-key
   :keymaps 'smerge-mode-map
   "C-c C-c"     #'smerge-keep-current))

;; vterm
;; (use-package vterm
;;   :ensure t
;;   :config
;;   (setq vterm-mode-hook (lambda() (display-line-numbers-mode -1)))

;;   ;; set tramp default encoding shell to zsh
;;   (add-to-list 'vterm-tramp-shells '("sshx" login-shell))
;;   )

;; projectile
(use-package projectile
  :ensure t
  :custom
  (projectile-project-name-function '+projectile-project-name--lower-case)
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (defun +projectile-project-name--lower-case (project-root)
    (downcase (file-name-nondirectory (directory-file-name project-root))))
  )

;; helpful
(use-package helpful
  :ensure t)

;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1))


;; vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first) 
  (vertico-count 17)
  )

;; consult
(use-package consult
  :ensure t
  :custom
  (consult-preview-max-count 17)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; my/command-wrapping-consult    ;; disable auto previews inside my command
   :preview-key '(:debounce 1 any) ;; Option 1: Delay preview
   ;; :preview-key "M-.")            ;; Option 2: Manual preview
   )
  )


(provide 'init-core)
