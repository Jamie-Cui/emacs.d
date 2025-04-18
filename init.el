;;; -*- lexical-binding: t; -*-

;; -----------------------------------------------------------
;; Emacs native configurations
;; -----------------------------------------------------------

;; load theme
(load-theme 'modus-vivendi t)

;; defining your own
;; macos: ~/Library/Mobile Documents/com~apple~CloudDocs/org-root
;; linux: ~/org-root
;; it's recommended to symlink your remote file here
;; ln -s ~/Library/Mobile\ Documents/com\~apple\~CloudDocs/org-root .
(defvar +my-org-root-dir "~/org-root")
(make-directory (concat +my-org-root-dir "/roam") t)
(make-directory (concat +my-org-root-dir "/journal") t)
(make-directory (concat +my-org-root-dir "/deft") t)
(make-directory (concat (file-name-directory user-init-file) "/bin") t)

;; stop makding ~ files!
(setq make-backup-files nil) 

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; do not show me native-comp warning and erros
(setq native-comp-async-report-warnings-errors 'silent)

;; set default font
(set-frame-font "0xProto Nerd Font Mono 14" nil t)

;; disable certain things
(menu-bar-mode 0)
(tool-bar-mode 0)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(setq ring-bell-function 'ignore)

;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(dolist (mode '(pdf-view-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                imenu-list-minor-mode-hook
                imenu-list-major-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))
(setq-default display-line-numbers-type 'relative)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq tab-always-indent t) ;  TAB just indents the current line

;; allow use minibuffer inside minibuffer
(setq enable-recursive-minibuffers t)

;; use short answers
(setq use-short-answers t)

;; enable hideshow in all programming modes
(use-package hideshow
  :config
  (add-hook 'prog-mode-hook #'hs-minor-mode))

;; column
(setq fill-column 80)

;; add auto-mdoe list
(add-to-list
 'auto-mode-alist
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

(use-package org
  :config
  (add-to-list 'org-latex-packages-alist
               '("lambda, advantage, operators, sets, adversary, landau,\
 probability, notions, logic, ff, mm, primitives, events, complexity, oracles,\
 asymptotics, keys" "cryptocode" t))
  (add-to-list 'org-latex-packages-alist
               '("" "booktabs" t))
  (setq org-log-done t)
  (setq org-confirm-babel-evaluate nil) ; don't ask, just do it
  )

(use-package compile
  :config
  (setq compilation-scroll-output t))

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
   consult-eglot
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
   ;; Emacs Mini-Buffer Actions Rooted in Keymaps
   embark
   embark-consult
   ;; Chinese input
   rime
   ;; colorful compilation output
   ansi-color
   ;; make eldoc looks nicer
   eldoc-box
   ;; pdf tools
   pdf-tools
   ;; latex support 
   auctex
   ;; deft for note taking
   deft
   ))

;; ------------------------------------------------------------------
;; TODO
;; ------------------------------------------------------------------

(use-package auctex)

;; ------------------------------------

(use-package engrave-faces)

(use-package ox-latex
  :after engrave-faces
  :config
  (setq org-latex-src-block-backend 'engraved)
  (setq org-latex-engraved-theme 't))

(use-package pdf-tools
  :config
  (pdf-loader-install))

(use-package deft
  :config
  (setq deft-extensions '("org"))
  (setq deft-directory (concat +my-org-root-dir "/deft"))
  )

(use-package cmake-mode
  :config
  (defun +my-modify-cmake-mode-syntax-table ()
    (modify-syntax-entry "/" "w" cmake-mode-syntax-table))
  (add-hook 'cmake-mode-hook #'+my-modify-cmake-mode-syntax-table)
  )

(use-package eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :after general
  :config
  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  (general-define-key
   :states 'normal
   :keymaps 'dirvish-mode-map
   "TAB" #'dirvish-subtree-toggle
   "h"   #'dired-up-directory
   "l"   #'dired-find-file
   )
  )

(use-package iedit
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

(use-package plantuml-mode
  :after org
  :config
  (setq plantuml-jar-path
        (concat (file-name-directory user-init-file) "bin/plantuml.jar"))
  (setq org-plantuml-jar-path plantuml-jar-path)
  (setq plantuml-default-exec-mode 'executable)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   ;; this line activates plantuml
   '((plantuml . t))))

(use-package xenops
  :config
  (add-hook 'org-mode-hook #'xenops-mode)
  (setq xenops-math-image-current-scale-factor 1.2)
  (setq xenops-math-image-margin 0)
  ;; HACK error from xenops with org>9.7
  ;; https://github.com/syl20bnr/spacemacs/issues/16577
  ;; https://github.com/dandavison/xenops/pull/74/files
  ;; https://github.com/dandavison/xenops/issues/73
  (defun fn/xenops-src-parse-at-point ()
    (-if-let* ((element (xenops-parse-element-at-point 'src))
               (org-babel-info
                (xenops-src-do-in-org-mode
                 (org-babel-get-src-block-info 'light (org-element-context)))))
        (xenops-util-plist-update
         element
         :type 'src
         :language (nth 0 org-babel-info)
         :org-babel-info org-babel-info)))

  (advice-add 'xenops-src-parse-at-point
              :override 'fn/xenops-src-parse-at-point)
  )

(use-package org-journal
  :config
  (setq org-journal-dir (concat +my-org-root-dir "/journal"))
  )

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  )

(use-package org-download
  :config
  ;; see: https://www.emacswiki.org/emacs/BufferLocalVariable
  (setq-default org-download-image-dir "img")
  (setq-default org-download-heading-lvl nil) ; no headings
  (setq org-download-method 'directory)
  (setq org-download-image-org-width 500)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function
        #'org-download-link-format-function-default))

(use-package citar
  :config
  (add-to-list 'citar-bibliography (concat +my-org-root-dir "/zotero_all.bib"))
  (add-to-list 'citar-notes-paths (concat +my-org-root-dir "/roam"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))


(use-package flycheck-popup-tip
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
  )

(use-package apheleia
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
  (add-to-list 'apheleia-mode-alist '(cc-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(c++-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(c++-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(c-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(c-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(cmake-mode . cmake-format))

  )

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
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
  (setq dashboard-banner-logo-title "It's possible to build a cabin with no foundations, but not a lasting building.")
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

(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory (concat +my-org-root-dir "/roam"))

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

(use-package consult-eglot)

(use-package eglot
  :ensure t
  :config
  (add-hook 'cc-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
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
  (setq evil-want-Y-yank-to-eol t) ; this need to be set before evil
  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t)

  ;; use undo-tree
  (evil-set-undo-system 'undo-tree)

  (evil-mode 1)
  )

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
   "C-SPC"   #'toggle-input-method
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
    "wm"     #'delete-other-windows
    ;; buffeer-related key bindings
    "<"      #'consult-buffer
    "bn"     #'evil-buffer-new
    "bd"     #'kill-current-buffer
    "br"     #'revert-buffer-no-confirm
    ;; open-related key bindings
    "ot"     #'projectile-run-vterm-other-window
    "oT"     #'vterm
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
    "njj"     #'org-journal-new-entry
    "nb"      #'citar-open
    "n@"      #'citar-insert-citation
    "ny"      #'org-store-link
    "np"      #'org-insert-link
    "ne"      #'org-export-dispatch
    "nd"      #'deft
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
    "cj"     #'consult-eglot-symbols
    ;; other
    "."      #'find-file
    "TAB"    #'evil-switch-to-windows-last-buffer
    "si"     #'consult-imenu
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

;; -----------------------------------------------------------
;; System-specific configurations
;; -----------------------------------------------------------

;; HACK for mac only
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  )

;; HACK for windows wsl2 only
(when (eq system-type 'linux)
  ;; Hack from: https://gist.github.com/minorugh/1770a6aa93df5fe55f70b4d72091ff76
  ;; Emacs on WSL open links in Windows web browser
  ;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
  (when (getenv "WSLENV")
    (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
          (cmd-args '("/c" "start")))
      (when (file-exists-p cmd-exe)
        (setq browse-url-generic-program  cmd-exe
              browse-url-generic-args     cmd-args
              browse-url-browser-function 'browse-url-generic
              search-web-default-browser 'browse-url-generic))))

  (use-package rime
    :ensure t
    :config
    (setq default-input-method "rime"
          rime-show-candidate 'popup))

  (use-package org-download
    :ensure t
    :config
    (setq org-download-screenshot-method
          "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")
    )
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
