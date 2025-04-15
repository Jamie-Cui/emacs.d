;; -----------------------------------------------------------
;; Emacs native configurations
;; -----------------------------------------------------------

;; load theme
(load-theme 'modus-vivendi t)

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

;; do not move to new line when comment ends
;; (setq comment-line-break-function 'nil)

;; column
(setq fill-column 80)

;; HACK setup environment
(add-to-list 'exec-path "/opt/homebrew/bin")

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
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; HACK for mac only
(setq mac-command-modifier 'meta)

;; HACK add homebrew to emacs's exec-path
(add-to-list 'exec-path "/opt/homebrew/bin") 

;; HACK native-comp failes on mac m1
;; see: https://github.com/d12frosted/homebrew-emacs-plus/issues/554\
;; #issuecomment-1601274371
(setenv
 "LIBRARY_PATH"
 (mapconcat
  'identity
  '(
    "/opt/homebrew/opt/gcc/lib/gcc/14"
    "/opt/homebrew/opt/libgccjit/lib/gcc/14"
    "/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin24/14")
  ":"))

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
      (package-install p)))
  )

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
   ))

;; ------------------------------------------------------------------
;; TODO

(use-package page-break-lines
  :config
  (global-page-break-lines-mode 1)
  )

(use-package org-journal)

(use-package flycheck)

(use-package citar)

;; ------------------------------------------------------------------

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

(use-package edwina
  :ensure t
  :config
  (edwina-setup-dwm-keys)
  (edwina-mode 1)
  )

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  )

(use-package nerd-icons)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;; Set the title
  (setq dashboard-banner-logo-title "Emacs")

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; vertically center content
  (setq dashboard-vertically-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (agenda    . 5)))
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


(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(vc-state
          subtree-state
          nerd-icons
          collapse
          git-msg
          file-time
          file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  )

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
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  )

(use-package magit)

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

  ;; ** keybindings that should not be overriden
  (general-define-key
   :keymaps 'override
   "M-f"     #'consult-line
   "M-s"     #'save-buffer
   "M-c"     #'evil-yank
   "M-v"     #'evil-paste-before
   "M-/"     #'comment-line
   "C-u"     #'evil-scroll-up
   "C-d"     #'evil-scroll-down
   "C-="     #'text-scale-increase
   "C--"     #'text-scale-decrease
   )

  (general-define-key
   :states 'visual
   :keymaps 'normal
   "gzA"     #'evil-mc-make-cursor-in-visual-selection-end
   "gzI"     #'evil-mc-make-cursor-in-visual-selection-beg
   "gzq"     #'evil-mc-undo-all-cursors
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
   "op"     #'dirvish-side
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
   "SPC"   #'projectile-find-file
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
   ;; other 
   "."      #'find-file
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

