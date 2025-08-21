;;; init-core.el --- core functionality support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-evil)

(+package/ensure-install
 '(
   ;; dashboard at startup
   dashboard
   ;; theme
   zenburn-theme
   ;; consult framework
   consult
   ;; show helps of fun, key, mode
   helpful
   ;; search engine
   vertico
   vertico-posframe
   ;; search with no order
   orderless
   ;; project engine
   projectile
   ;; complete engine
   corfu
   corfu-terminal ; for tty
   ;; better terminal emulater
   eat
   ;; the killer app: git ui
   magit
   ;; highlight diff
   diff-hl
   ;; lsp
   eglot
   ;; jump to eglot symbol
   consult-eglot
   ;; highlight todo keywords
   hl-todo
   ;; search tool based on ripgrep
   rg
   ;; better error checking
   flycheck
   ;; show fly check in a popup way
   flycheck-popup-tip
   ;; show lsp error
   flycheck-eglot
   ;; adds marginalia to the minibuffer completions
   marginalia
   ;; Emacs Mini-Buffer Actions Rooted in Keymaps
   embark
   embark-consult
   ;; make eldoc looks nicer
   eldoc-box
   ;; better snippet
   yasnippet
   ;; workspace
   perspective
   persp-projectile
   ;; smart-parens
   smartparens
   ;; popup window
   popwin
   ;; sudo-edit
   sudo-edit
   ;; dired
   dired-subtree
   ;; colored dired
   diredfl 
   ;; disable mouse 
   inhibit-mouse
   ;; modeline 
   doom-modeline
   ;; chinese spacing
   pangu-spacing
   ;; chinese s alignment, or valign (maybe?)
   cnfonts
   ))

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
  (cnfonts-personal-fontnames '(
                                ("Maple Mono NF CN") ;; English
                                ("Maple Mono NF CN") ;; Chinese
                                nil  ;; Ext-B
                                nil ;; Symbol
                                nil ;; Others
                                ))
  (cnfonts-use-face-font-rescale t)
  :config
  ;; NOTE you can use this to list all fonts
  ;; (cl-prettyprint (font-family-list))
  (cnfonts-mode 1)
  )

(use-package vertico-posframe
  :ensure t
  :hook (vertico-mode . vertico-posframe-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon nil) ; no icon
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-major-mode-color-icon nil)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-lsp-icon nil)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-time-live-icon nil)
  (setq doom-modeline-modal-icon nil) ; no icon
  (setq doom-modeline-modal-modern-icon nil) ; no icon
  )

(use-package inhibit-mouse
  :ensure t
  :custom
  ;; Disable highlighting of clickable text such as URLs and hyperlinks when
  ;; hovered by the mouse pointer.
  (inhibit-mouse-adjust-mouse-highlight t)
  ;; Disables the use of tooltips (show-help-function) during mouse events.
  (inhibit-mouse-adjust-show-help-function t)
  :config
  (defun +inhibit-mouse/toggle-mode ()
    "Toggle inhibit-mouse-mode."
    (interactive)
    (if inhibit-mouse-mode
        (progn
          (inhibit-mouse-mode -1)
          (message "inhibit-mouse-mode disabled"))
      (progn
        (inhibit-mouse-mode 1)
        (message "inhibit-mouse-mode enabled"))))
  (inhibit-mouse-mode)
  )

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package zenburn-theme
  :ensure t)

(use-package persp-projectile
  :ensure t)

(use-package popwin
  :ensure t
  :custom 
  (popwin:popup-window-height 0.5)
  (popwin:popup-window-width 0.3)
  (popwin:adjust-other-windows t)
  (popwin:special-display-config
   '(
     ;;
     ;; One-time notification
     ;;
     ("*xref*" :position bottom)
     ;;
     ;; bottom (only for display)
     ;;
     (help-mode :position bottom :stick t)
     (helpful-mode :position bottom :stick t)
     (compilation-mode :position bottom :stick t :tail t)
     ("*Flycheck errors*" :position bottom :stick t)
     ("*Messages*" :position bottom :stick t)
     ("*LLM response*" :position bottom :stick t)
     ;;
     ;; right, where you might need to write something inside
     ;;
     ("*scratch*" :position right :stick t)
     ((lambda (b) ; predicate
        ;; NOTE: buffer check is required (#450)
        (and-let* ((buf (get-buffer (or (car-safe b) b))))
          (buffer-local-value 'gptel-mode buf)))
      :position right :stick t :tail t)
     ;;
     ;; FIXME claude-code uses full frame, temp
     ;; (claude-code--buffer-p :position right :stick t)
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
  (persp-modestring-dividers '("[" "]" "|"))
  (persp-show-modestring 't)
  :init
  (persp-mode)
  )

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

(use-package eldoc-box
  :ensure t
  :after eglot
  :if window-system ;; do not load eldoc-box on termial emacs
  :config
  (add-hook 'eldoc-mode-hook #'eldoc-box-hover-mode)
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  )

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

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook)

  ;; HACK from https://github.com/emacs-dashboard/emacs-dashboard/issues/153#issuecomment-714406661
  (defvar my-banners-dir (concat +emacs/repo-directory "/data/"))
  (defun +dashboard/install-banners ()
    "Copy all files under under banners directory to dashboard banners directory"
    (when (boundp 'dashboard-banners-directory)
      (copy-directory my-banners-dir dashboard-banners-directory nil nil t)))
  (+dashboard/install-banners)

  ;; Set the title  
  (setq dashboard-banner-logo-title "It's possible to build a cabin with no foundations, but not a lasting building.")

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-startup-banner 4) ; 4 means using 4.txt
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                          (projects  . 5)
                          (bookmarks . 5)))
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

  ;; use corfu in eshell
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq-local corfu-auto nil)
                                (corfu-mode)))
  )

(when (not (display-graphic-p))
  (use-package corfu-terminal
    :ensure t
    :config
    (corfu-terminal-mode +1)))

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
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (setq eglot-confirm-server-initiated-edits nil)
  )

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
   "C-c C-c"     #'smerge-keep-current))

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

  (general-define-key
   :states 'normal
   :keymaps 'eat-mode-map
   "p"   #'eat-yank)
  )

;; projectile
(use-package projectile
  :ensure t
  :custom
  (projectile-project-name-function '+projectile-project-name--lower-case)
  (projectile-indexing-method 'hybrid)
  (projectile-enable-caching t)
  (projectile-per-project-compilation-buffer t)
  (projectile-switch-project-action 'projectile-find-file)
  :config
  (defun +projectile-project-name--lower-case (project-root)
    (downcase (file-name-nondirectory (directory-file-name project-root))))
  (projectile-mode +1)
  )

;; helpful
(use-package helpful
  :ensure t)

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
   )
  :config 
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref)
  )

;; sudo-edit
(use-package sudo-edit
  :ensure t)

;; dired hide .. and .
(add-hook 'dired-mode-hook 'dired-omit-mode)

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
   "T"   #'dired-create-empty-file
   )
  )

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "TAB" #'dired-subtree-toggle)
  )

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode)
  )

(use-package tramp
  :config
  ;; Enable full-featured Dirvish over TRAMP on ssh connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  ;; Tips to speed up connections
  (setq tramp-verbose 0)
  (setq tramp-chunksize 2000)
  (setq tramp-ssh-controlmaster-options nil))

;;;###autoload
(cl-defun +vertico-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory.
:args LIST
  Arguments to be appended to `consult-ripgrep-args'."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'consult)
  (setq deactivate-mark t)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (directory (or in project-root))
         (consult-ripgrep-args
          (concat "rg "
                  (if all-files "-uu ")
                  (unless recursive "--maxdepth 1 ")
                  "--null --line-buffered --color=never --max-columns=1000 "
                  "--smart-case --no-heading "
                  "--with-filename --line-number --search-zip "
                  "--hidden -g !.git -g !.svn -g !.hg "
                  (mapconcat #'identity args " ")))
         (prompt (if (stringp prompt) (string-trim prompt) "Search"))
         (query query)
         (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial _function)
          (alist-get consult-async-split-style consult-async-split-styles-alist)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                     (concat "\\" (char-to-string separator))
                                     query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                            unless (string-match-p char query)
                                            return char)
                                   "%")
                              :type perl)
                   consult-async-split-style 'perlalt))))))
    (consult--grep prompt #'consult--ripgrep-make-builder directory query)))

;;;###autoload
(defun +vertico/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+vertico-file-search :query initial-query :in directory :all-files arg))

(defun +revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))


(defun +persp/move-buffer-prev ()
  "Like persp-prev, but move current."
  (interactive)
  (let ((tmp-buffer (current-buffer)))
    (persp-forget-buffer tmp-buffer)
    (persp-prev)
    (persp-set-buffer tmp-buffer)
    (persp-switch-to-buffer tmp-buffer))
  )

(defun +persp/move-buffer-next ()
  "Like persp-next, but move current."
  (interactive)
  (let ((tmp-buffer (current-buffer)))
    (persp-forget-buffer tmp-buffer)
    (persp-next)
    (persp-set-buffer tmp-buffer)
    (persp-switch-to-buffer tmp-buffer))
  )

(provide 'init-core)
