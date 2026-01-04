;;; init-core.el --- core functionality support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-evil)

;; -----------------------------------------------------------
;; DONE Dired
;;
;; dired-du
;; dired
;; dired-subtree
;; diredfl
;; dired-sidebar
;; -----------------------------------------------------------

;; dired hide .. and .
(add-hook 'dired-mode-hook 'dired-omit-mode)

(use-package dired-du
  :ensure t
  :custom
  (dired-du-size-format t))

(use-package dired
  :custom
  (dired-listing-switches 
   (purecopy "-ahl -v --group-directories-first"))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-omit-extensions nil)
  (dired-dwim-target t)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "h"   #'dired-up-directory
   "l"   #'dired-find-file
   "T"   #'dired-create-empty-file))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "TAB" #'dired-subtree-toggle))

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode))

(use-package dired-sidebar
  :ensure t
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :custom
  (dired-sidebar-theme 'ascii))

;; -----------------------------------------------------------
;; DONE Editing & editor
;;
;; hl-todo
;; pangu-spacing
;; diff-hl
;; cnfonts
;; smartparens
;; which-key
;; yasnippet
;; yasnippet-snippets
;; embark
;; embark-consult
;; wgrep
;; iedit
;; ansi-color
;; sudo-edit
;; -----------------------------------------------------------

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1)
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold) ;; I should schedule to do this
          ("REVIEW" warning bold) ;; I've write something uncertain, double check this
          ("HACK" warning bold) ;; This is a temp/ugly fix, and should have other solutions, fix it if possible
          ("DEPRECATED" warning bold) ;; require notice
          ("NOTE" success bold) ;; require notice
          ("DONE" success bold)
          ("FIXME" error bold) ;; require immediate action
          ("BUG" error bold) ;; require immediate action
          )))

(use-package diff-hl
  :ensure t
  :config
  (diff-hl-margin-mode 1)
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package pangu-spacing
  :ensure t
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor nil)
  (add-hook 'org-mode-hook
            '(lambda ()
               (set (make-local-variable
                     'pangu-spacing-real-insert-separtor) t))))

(use-package cnfonts
  :ensure t
  :custom
  (cnfonts-personal-fontnames 
   '(
     ("Maple Mono NL NF CN" "Maple Mono NF CN") ;; English
     ("Maple Mono NL NF CN" "Maple Mono NF CN") ;; Chinese
     nil  ;; Ext-B
     nil ;; Symbol
     nil ;; Others
     ))
  (cnfonts-use-face-font-rescale t)
  :config
  ;; NOTE you can use this to list all fonts
  ;; (cl-prettyprint (font-family-list))
  (cnfonts-mode 1))

(use-package smartparens
  :ensure t  ;; install the package
  :hook (prog-mode text-mode markdown-mode org-mode) 
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package which-key
  :ensure t
  :custom
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  (which-key-add-column-padding 1)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (let ((my-yas-dir (concat +emacs/repo-directory "/snippets")))
    (add-to-list 'yas-snippet-dirs my-yas-dir))
  ;; start mode globally
  (yas-global-mode 1)
  ;; reload all snippets
  (yas-reload-all)
  )

(use-package yasnippet-snippets
  :ensure t)

(use-package embark
  :ensure t
  :bind
  (("C-;" . embark-act))
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; 
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :custom
  (embark-help-key "?")
  ;; (embark-prompter 'embark-completing-read-prompter)
  :config
  (setq embark-indicators
        '(embark-minimal-indicator  ; default is embark-mixed-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t)

(use-package iedit
  :ensure t
  :init
  ;; Fix conflict with embark.
  (setq iedit-toggle-key-default nil))

(use-package ansi-color
  :ensure t
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package sudo-edit
  :ensure t)

;; -----------------------------------------------------------
;; DONE programming
;;
;; consult-citre
;; citre
;; flycheck-popup-tip
;; flycheck
;; flycheck-eglot
;; eglot
;; consult-eglot
;; consult-eglot-embark
;; eldoc-box
;; 
;; NOTE You need to install indexing tools for citre:
;;    universal ctags: https://github.com/universal-ctags/ctags
;;    gtags: https://github.com/universal-ctags/ctags
;;    eglot: built-in with modern emacs, uses external lsp servers
;; -----------------------------------------------------------

(use-package citre
  :ensure t
  :after (eglot projectile)
  :init
  ;; This is needed in `:init' block for lazy load to work.
  ;; (require 'citre-config)
  :custom
  ;; (citre-project-root-function #'projectile-project-root)
  (citre-default-create-tags-file-location 'global-cache)
  (citre-edit-ctags-options-manually nil)
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-enable-imenu-integration nil)
  :config
  ;; HACK only enable citre-auto-enable-citre-mode when not on tramp
  (add-hook 'find-file-hook 
            (lambda ()
              (unless (file-remote-p default-directory)
                (citre-auto-enable-citre-mode))))
  )

(use-package flycheck-popup-tip
  :ensure t
  :after evil
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
  ;; NOTE Only display the flycheck popup if we're in normal mode (for evil
  ;;   users) or if no selection or completion is active. This popup can

  ;;   interfere with the active evil mode, clear active regions, and other
  ;;   funny business (see #7242).
  (defun +syntax--disable-flycheck-popup-tip-maybe-a (&rest _)
    (if (and (bound-and-true-p evil-local-mode)
             (not (evil-emacs-state-p)))
        (evil-normal-state-p)
      (and (not (region-active-p))
           (not (ignore-errors (>= corfu--index 0))))))

  (advice-add #'flycheck-popup-tip-show-popup :before-while #'+syntax--disable-flycheck-popup-tip-maybe-a))

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

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :custom
  (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

(use-package consult-eglot
  :ensure t)

(use-package consult-eglot-embark
  :ensure t
  :after embark
  :config
  (consult-eglot-embark-mode))

(use-package eglot
  :ensure t
  :config
  ;; (add-hook 'c-ts-mode-hook 'eglot-ensure)
  ;; (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  ;; (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  ;; (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (setq eglot-ignored-server-capabilities '(
                                            :inlayHintProvider
                                            :documentHighlightProvider
                                            :semanticTokensProvider))
  (setq eglot-confirm-server-initiated-edits nil)
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  )

(use-package eldoc-box
  :ensure t
  :after eglot
  :if window-system ;; do not load eldoc-box on termial emacs
  :config
  ;; (setq eldoc-echo-area-use-multiline-p nil)
  (add-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode))

;; -----------------------------------------------------------
;; DONE Workspaces
;;
;; persp-projectile (site-lisp)
;; perspective
;; projectile
;; -----------------------------------------------------------

(use-package persp-projectile
  :after (perspective projectile)
  :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/")))

(use-package perspective
  :ensure t
  :custom 
  (persp-suppress-no-prefix-key-warning t)
  (persp-sort 'created)
  (persp-modestring-dividers '("(Proj:" ")" ""))
  (persp-show-modestring nil)
  (persp-modestring-short nil)
  :init
  (persp-mode)
  :config
  (put 'persp-selected-face 'face-alias 'success))

(use-package projectile
  :ensure t
  :custom 
  (projectile-indexing-method 'hybrid)
  ;; DO NOT add / remove project automatically
  (projectile-track-known-projects-automatically nil) 
  ;; enable cache persistently
  (projectile-enable-caching 'persistent)
  ;; each project has a separate compilation buffer
  (projectile-per-project-compilation-buffer t)
  ;; remote cache is avaliable for 5 min
  (projectile-file-exists-remote-cache-expire (* 5 60))
  ;; only git as project identifier
  (projectile-project-root-files-bottom-up '(".git"))
  ;; auto update cache when files are opened or deleted
  (projectile-auto-update-cache t)
  ;; I prefer citre, do not use built-in tag systm
  (projectile-tags-backend nil)
  :config
  ;; see: https://github.com/syl20bnr/spacemacs/issues/11381#issuecomment-481239700
  ;; (defadvice projectile-project-root (around ignore-remote first activate)
  ;;   (unless (file-remote-p default-directory) ad-do-it))
  (projectile-mode +1)

  ;; see: https://metaredux.com/posts/2025/02/03/projectile-introduces-significant-caching-improvements.html
  ;; Defer cache loading to after-init for faster startup
  (add-hook 'after-init-hook
            (lambda ()
              ;; initialize the projects cache if needed
              (unless projectile-projects-cache
                (setq projectile-projects-cache
                      (or (projectile-unserialize projectile-cache-file)
                          (make-hash-table :test 'equal))))
              (unless projectile-projects-cache-time
                (setq projectile-projects-cache-time
                      (make-hash-table :test 'equal)))
              ;; load the known projects
              (projectile-load-known-projects)
              ;; update the list of known projects
              (projectile--cleanup-known-projects)
              (when projectile-auto-discover
                (projectile-discover-projects-in-search-path))))
  )

(defun +persp/format-name-as-in-echo (name)
  "Format the perspective name given by NAME for display in the echo area."
  (if (equal name (persp-current-name))
      (setq name (format "[%s]" name)) 
    (setq name (format " %s " name))))

(defun +persp/show-name-in-echo ()
  "Show persp names in the echo area."
  (let ((message-log-max nil))
    (message (mapconcat 'identity 
                        (mapcar '+persp/format-name-as-in-echo
                                (persp-names))))))

(defun +persp/move-buffer-prev ()
  "Like persp-prev, but move current."
  (interactive)
  (let ((tmp-buffer (current-buffer)))
    (persp-forget-buffer tmp-buffer)
    (persp-prev)
    (persp-set-buffer tmp-buffer)
    (persp-switch-to-buffer tmp-buffer))
  (+persp/show-name-in-echo))

(defun +persp/move-buffer-next ()
  "Like persp-next, but move current."
  (interactive)
  (let ((tmp-buffer (current-buffer)))
    (persp-forget-buffer tmp-buffer)
    (persp-next)
    (persp-set-buffer tmp-buffer)
    (persp-switch-to-buffer tmp-buffer))
  (+persp/show-name-in-echo))

;; let 'quit signal to show the persp names
(advice-add 'keyboard-quit :before #'(lambda () (put 'quit 'error-message (+persp/show-name-in-echo))))

;; -----------------------------------------------------------
;; DONE other useful tools
;;
;; docker
;; dashboard
;; magit
;; eat
;; helpful
;; popwin
;; eaf
;; envrc
;; treesit-fold
;; -----------------------------------------------------------

(use-package docker
  :ensure t
  :custom
  (docker-show-messages nil)
  (docker-container-shell-file-name "/bin/bash"))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-banner-logo-title "It's possible to build a cabin with no foundations, but not a lasting building.")
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-set-heading-icons nil)
  (dashboard-startup-banner 4) ; 4 means using 4.txt
  (dashboard-set-file-icons nil)
  (dashboard-items '((recents  . 5)
                     (projects  . 5)
                     (bookmarks . 5)))
  (dashboard-projects-backend 'projectile)
  :config
  (dashboard-setup-startup-hook)

  ;; HACK from https://github.com/emacs-dashboard/emacs-dashboard/issues/153#issuecomment-714406661
  (defvar my-banners-dir (concat +emacs/repo-directory "/data/"))
  (defun +dashboard/install-banners ()
    "Copy all files under under banners directory to dashboard banners directory"
    (when (boundp 'dashboard-banners-directory)
      (copy-directory my-banners-dir dashboard-banners-directory nil nil t)))
  (+dashboard/install-banners)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-after-initialize-hook (lambda() (dashboard-open)))
  (setq initial-buffer-choice (lambda() (dashboard-open))))

(use-package magit
  :ensure t
  :after evil-collection
  :config
  (setq magit-tramp-pipe-stty-settings 'pty)
  (setq magit-commit-show-diff nil)
  (setq magit-branch-direct-configure nil)
  (setq magit-refresh-status-buffer nil)
  (setq magit-log-section-commit-count 20)  ; Show fewer commits
  (setq magit-auto-revert-mode nil)
  (setq magit-diff-refine-hunk t)
  (setq magit-save-repository-buffers nil)
  (setq magit-revision-insert-related-refs nil)
  (setq magit-uniquify-buffer-names nil)
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes))
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (general-define-key
   :keymaps 'smerge-mode-map
   "C-c C-c"     #'smerge-keep-current)

  ;; Install magit-auto-commit
  (transient-append-suffix 'magit-commit #'magit-commit-create
    '("a" "Auto (but fixed) commit" (lambda (&optional args)
                                      (interactive (list (magit-commit-arguments)))
                                      (let ((message "chore: stale - work still in progress"))
                                        (magit-commit-create (append args `("--message" ,message "--edit")))))))
  )

(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  (eat-enable-yank-to-terminal t)
  (eat-eshell-fallback-if-stty-not-available t)
  :config
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  (defun +eat/new ()
    (interactive) 
    (let ((current-prefix-arg ""))
      (call-interactively 'eat)))

  (general-define-key
   :states 'normal
   :keymaps 'eat-mode-map
   "p"   #'eat-yank)
  )

(use-package helpful
  :ensure t)

(use-package popwin
  :ensure t
  :after gptel
  :custom 
  (popwin:popup-window-height 0.3)
  (popwin:popup-window-width 0.3)
  (popwin:adjust-other-windows t)
  (popwin:reuse-window t)
  (popwin:special-display-config
   '(
     ("*xref*" :position bottom)
     (help-mode :position bottom :stick t :dedicated t)
     (helpful-mode :position bottom :stick t :dedicated t)
     ;; ("^\\*eshell\\*.*" :regexp t :position bottom :stick t :dedicated t)
     ;; (eat-mode :position bottom :stick t :dedicated t)
     ("*Flycheck errors*" :position bottom :stick t :dedicated t)
     ("*Messages*" :position bottom :stick t :dedicated t)
     ("*LLM response*" :position bottom :stick t :dedicated t)
     ("*scratch*" :position bottom :stick t :dedicated t)
     ((lambda (b) ; predicate for gptel buffer
        ;; NOTE: buffer check is required (#450)
        (and-let* ((buf (get-buffer (or (car-safe b) b))))
          (buffer-local-value 'gptel-mode buf)))
      :position bottom :stick t :tail t :dedicated t)
     )
   )
  :config
  (popwin-mode 1))

;; install dir: https://direnv.net/
;; brew install direnv
;; sudo dnf install direnv
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(provide 'init-core)
