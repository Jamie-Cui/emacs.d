;; -----------------------------------------------------------
;; Emacs native configurations
;; -----------------------------------------------------------

;; (setq custom-file
;;       (concat (file-name-directory user-init-file) "custom.el"))
;; (load custom-file custom-file)

;; load theme
;; (load-theme 'modus-vivendi t)
(load-theme 'tsdh-dark t)

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; set default font
(set-frame-font "0xProto Nerd Font Mono 14" nil t)

;; disable certain things
(menu-bar-mode 0)
(tool-bar-mode 0)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(global-display-line-numbers-mode 1)

(setq display-line-numbers-type 'relative)
(setq ring-bell-function 'ignore)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq tab-always-indent t) ;  TAB just indents the current line

;; use short answers
(setq use-short-answers t)

;; column
(setq fill-column 80)

;; add auto-mdoe list
(add-to-list 'auto-mode-alist
             '("\\(\\.ii\\|\\.\\(CC?\\|HH?\\)\\|\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\|\\.\\(cc\\|hh\\)\\)\\'"
               . c++-ts-mode))

;; HACK setup environment
;; see: https://www.emacswiki.org/emacs/ExecPath
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" "" (shell-command-to-string
                          "$SHELL --login -c 'echo $PATH'"
                          ))))
    ;; (message path-from-shell)
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; run this setup function
(set-exec-path-from-shell-PATH)

(when (eq system-type 'darwin)
                                        ; do this
                                        ; and this ...
  ;; HACK for mac only
  (setq mac-command-modifier 'meta)
  )

;; after all emacs built-in pacakges are loaded
(use-package emacs
  :custom
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.
  ;; Corfu ;; commands are hidden, since they are not used via M-x.
  ;; This setting is ;; useful beyond Corfu.
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  )

;; HACK for vertico
;; Persist history over Emacs restarts.
;; Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package dired-x
  :config
  (setq-default dired-dwim-target t)
  (setq dired-listing-switches "-alh")
  )

;; -----------------------------------------------------------
;; (my) emacs core thirdparty configurations
;; -----------------------------------------------------------

;; Enable package
(require 'package)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("nongnu"   . "http://elpa.nongnu.org/nongnu/")
        ("org"   . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; make sure package-refresh-contents will only run once
(when (not package-archive-contents)
  (package-refresh-contents))

(defun +ensure-packages-installed (packages-alist)
  "Make sure the given package is installed."
  (dolist (p packages-alist)
    (unless (package-installed-p p)
      (package-install p))))

(+ensure-packages-installed
 '(
   ;; evil-related
   evil
   evil-collection
   undo-tree
   undo-fu
   evil-multiedit
   evil-mc
   evil-goggles
   evil-nerd-commenter
   general ; more convenient way of defining keys
   ;; org-related pacakages
   evil-org
   org-download
   org-superstar
   org-fancy-priorities
   org-roam
   ;; show key helps
   which-key
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
   vterm
   ;; the killer app: git ui
   magit
   ;; lsp
   eglot
   flycheck-eglot
   ;; highlight todo keywords
   hl-todo
   ;; search tool based on ripgrep
   rg
   ;; citar
   citar
   ;; better error checking
   flycheck
   flycheck-popup-tip
   ;; better dired
   dirvish
   ;; better place to write diaries
   org-journal
   ;; tiling windown manager
   edwina
   ;; dashboard at startup
   dashboard
   ;; icons
   nerd-icons
   ;; modeline
   doom-modeline
   ;; cpplint
   flycheck-google-cpplint
   ;; bazel-mode
   bazel
   ;; protobuf-mdoe
   protobuf-mode
   ;; meson-mode
   meson-mode
   ;; allow drawing
   plantuml-mode
   ;; preview org math
   xenops
   ;; show key frenquency
   keyfreq
   ;; llm client
   gptel
   ;; export org code in colors
   engrave-faces
   ;; adds marginalia to the minibuffer completions
   marginalia
   ;; make line-break look nicer
   page-break-lines
   ;; markdown mode
   markdown-mode
   ;; Colorize color names in buffers
   rainbow-mode
   ;; cmake
   cmake-mode
   ;; code auto formating
   apheleia
   ))

;; ------------------------------------------------------------------
;; TODO
;; ------------------------------------------------------------------

(use-package flycheck-popup-tip
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
  )

(use-package apheleia
  :config
  (apheleia-global-mode +1)
  )

(use-package cmake-mode)

(use-package org-journal)

(use-package citar)

;; ------------------------------------

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package evil-nerd-commenter
  :after evil)

(use-package rainbow-mode
  :hook (emacs-lisp-mode text-mode lisp-mode cc-mode cmake-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package page-break-lines
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
  :config
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
  (setq flycheck-eglot-exclusive nil)
  (flycheck-add-next-checker 'eglot-check
                             '(warning . c/c++-googlelint))
  )


(use-package marginalia
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

;; (use-package edwina
;; :ensure t
;; :config
;; (edwina-setup-dwm-keys)
;; (edwina-mode 1)
;; )

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :config
  (setq doom-modeline-height 1) ; optional
  (if (facep 'mode-line-active)
      (set-face-attribute 'mode-line-active nil :family "0xProto Nerd Font Mono 14") ; For 29+
    (set-face-attribute 'mode-line nil :family "0xProto Nerd Font Mono 14"))
  (set-face-attribute 'mode-line-inactive nil :family "0xProto Nerd Font Mono 14")
  (doom-modeline-mode 1)
  )

(use-package nerd-icons)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;; Set the title
  ;; (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-page-separator "\n\f\n")

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; vertically center content
  (setq dashboard-vertically-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (agenda    . 5)))
  (setq dashboard-projects-backend 'projectile)
  )

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil ; too slow
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  )


(use-package evil-mc
  :ensure t
  :after evil
  :config
  (global-evil-mc-mode  1) ;; enable
  )

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-multiedit-default-keybinds)
  )


;; (use-package dirvish
;;   :ensure t
;;   :init
;;   (dirvish-override-dired-mode)
;;   :config
;;   (setq dirvish-mode-line-format
;;         '(:left (sort symlink) :right (omit yank index)))
;;   (setq dirvish-attributes
;;         '(vc-state
;;           subtree-state
;;           nerd-icons
;;           collapse
;;           git-msg
;;           file-time
;;           file-size)
;;         dirvish-side-attributes
;;         '(vc-state nerd-icons collapse file-size))

;;   (general-define-key
;;    :state 'normal
;;    :keymaps 'dirvish-mode-map
;;    "TAB"     #'dirvish-subtree-toggle
;;    "?"       #'dirvish-dispatch
;;    )
;;   )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory
   (file-truename
    "~/Library/Mobile Documents/com~apple~CloudDocs/org-remote/roam"))
  :config
  ;; If you're using a vertical completion framework, you might want
  ;; a more informative completion interface
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package hl-todo
  :config
  (global-hl-todo-mode 1)
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented,
          ;; or slow, and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to
          ;; upstream it, improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future
          ;; update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone
          ;; soon. Specifically, this means the code is deprecated,
          ;; not necessarily ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning
          ;; may vary from project to project.
          ("NOTE" success bold)
          ("DONE" success bold)
          ("BUG" error bold)
          )))

(use-package corfu
  :ensure t
  :init
  :config
  (global-corfu-mode)
  ;; Enable auto completion and configure quitting
  (setq corfu-auto t
        corfu-cycle t
        corfu-preview-current 'nil ; do not insert unless i select it
        corfu-preselect 'nil ; do not preselect anything
        corfu-quit-no-match 'separator) ;; or t
  )

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (partial-completion))))))    

(use-package eglot
  :ensure t
  :config
  (add-hook 'cc-mode 'eglot-ensure)
  (add-hook 'c++-ts-mode 'eglot-ensure)
  (add-hook 'c-ts-mode 'eglot-ensure)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  )

(use-package magit
  :ensure t
  :after evil-collection
  :config
  (general-define-key
   :keymaps 'smerge-mode-map
   "C-c C-c"     #'smerge-keep-current
   )
  )

;; projectile
(use-package vterm
  :ensure t
  :config
  (setq vterm-mode-hook (lambda() (display-line-numbers-mode -1)))
  )

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  )

;; helpful
(use-package helpful)

;; which-key
(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1))

;; vertico
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-preselect 'no-prompt) ; do not do any preselect
  (vertico-count 17)
  )

;; consult
(use-package consult
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

;; evil
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  ;; do not save history by default
  (setq undo-tree-auto-save-history nil)

  )
(use-package undo-fu)

(use-package evil
  :ensure t
  :after (:and undo-tree undo-fu)
  :preface
  (setq evil-overriding-maps nil)
  (setq evil-want-keybinding nil)
  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t)

  ;; use undo-tree
  (evil-set-undo-system 'undo-tree)

  (evil-mode 1))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(use-package general
  :after (:and evil evil-mc)
  :config
  (defconst my-leader "SPC")
  (defconst my-local-leader "SPC m")

  (general-create-definer +my-leader-def
    :prefix my-leader)

  (defun evil-keyboard-quit ()
    "Keyboard quit and force normal state."
    (interactive)
    (and evil-mode (evil-force-normal-state))
    (keyboard-quit))

  (general-define-key
   :states '(normal insert visual)
   "C-g" #'evil-keyboard-quit)

  ;; ** keybindings that should not be overriden
  (general-define-key
   :keymaps 'override
   "M-f"     #'consult-line
   "M-s"     #'save-buffer
   "M-c"     #'evil-yank
   "M-v"     #'evil-paste-before
   "M-/"     #'evilnc-comment-or-uncomment-lines
   "C-u"     #'evil-scroll-up
   "C-d"     #'evil-scroll-down
   "C-="     #'text-scale-increase
   "C--"     #'text-scale-decrease
   )

  ;; ** Global Keybindings
  (+my-leader-def
    :states 'normal
    :keymaps 'override ; prevent from being override
    ;; window-related key bindings
    "wh"     #'evil-window-left
    "wj"     #'evil-window-down
    "wk"     #'evil-window-up
    "wl"     #'evil-window-right
    "ww"     #'other-window
    "wd"     #'evil-window-delete
    "ws"     #'evil-window-split
    "wv"     #'evil-window-vsplit
    "wm"     #'maximize-window
    ;; buffeer-related key bindings
    "<"      #'consult-buffer
    "bn"     #'evil-buffer-new
    "bd"     #'kill-current-buffer
    "br"     #'revert-buffer-no-confirm
    ;; open-related key bindings
    "ot"     #'projectile-run-vterm-other-window
    "od"     #'dired-jump
    "og"     #'magit-status
    ;; prject-related key bindings
    "pa"     #'projectile-add-known-project
    "px"     #'projectile-remove-known-project
    "pp"     #'projectile-switch-project
    "pc"     #'projectile-compile-project
    "pt"     #'projectile-test-project
    "pr"     #'projectile-run-project
    "pd"     #'projectile-kill-buffers
    "pi"     #'projectile-invalidate-cache
    "pf"     #'consult-ripgrep
    "po"     #'find-sibling-file
    "SPC"    #'projectile-find-file
    ;; note functions
    "nrf"     #'org-roam-node-find
    "nri"     #'org-roam-node-insert
    ;; help functions
    "hf"     #'helpful-callable
    "hk"     #'helpful-key
    "hv"     #'helpful-variable
    "hm"     #'describe-mode
    ;; quit emacs
    "qq"     #'save-buffers-kill-terminal
    "qr"     #'restart-emacs
    ;; toggles
    "th"     #'hs-hide-level
    "tf"     #'toggle-frame-fullscreen
    "tt"     #'toggle-truncate-lines
    "tc"     #'display-fill-column-indicator-mode
    ;; code
    "cx"     #'list-flycheck-errors
    "ca"     #'eglot-code-actions
    "cf"     #'eglot-format-buffer
    ;; other 
    "."      #'find-file
    "TAB"      #'evil-switch-to-windows-last-buffer
    )
  )


;;; evil-collection
(use-package evil-collection
  :init
  (setq evil-want-keybinding nil)
  :after evil
  :config
  ;; make sure the follwing key bindings always work
  (evil-collection-init)
  )

;;;
(use-package cc-mode
  :config
  (add-to-list 'find-sibling-rules '("/\\([^/]+\\)\\.c\\(c\\|pp\\)?\\'" "\\1.h\\(h\\|pp\\)?\\'"))
  (add-to-list 'find-sibling-rules '("/\\([^/]+\\)\\.h\\(h\\|pp\\)?\\'" "\\1.c\\(c\\|pp\\)?\\'"))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
