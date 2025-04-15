;;     Copyright (C) 2025 Jamie Cui

;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Also add information on how to contact you by electronic and paper mail.

;;   If the program does terminal interaction, make it output a short
;; notice like this when it starts in an interactive mode:

;;     <program>  Copyright (C) <year>  <name of author>
;;     This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
;;     This is free software, and you are welcome to redistribute it
;;     under certain conditions; type `show c' for details.

;; The hypothetical commands `show w' and `show c' should show the appropriate
;; parts of the General Public License.  Of course, your program's commands
;; might be different; for a GUI interface, you would use an "about box".

;;   You should also get your employer (if you work as a programmer) or school,
;; if any, to sign a "copyright disclaimer" for the program, if necessary.
;; For more information on this, and how to apply and follow the GNU GPL, see
;; <http://www.gnu.org/licenses/>.

;;   The GNU General Public License does not permit incorporating your program
;; into proprietary programs.  If your program is a subroutine library, you
;; may consider it more useful to permit linking proprietary applications with
;; the library.  If this is what you want to do, use the GNU Lesser General
;; Public License instead of this License.  But first, please read
;; <http://www.gnu.org/philosophy/why-not-lgpl.html>.

;; -----------------------------------------------------------
;; Emacs native configurations
;; -----------------------------------------------------------

;; load theme
(load-theme 'modus-vivendi t)

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; set default font
(set-frame-font "0xProto Nerd Font Mono 16" nil t)

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
(setq tab-always-indent t) ; hitting TAB always just indents the current line.

;; HACK setup environment
(add-to-list 'exec-path "/opt/homebrew/bin")
(setenv "PATH" "/Users/jamie/miniconda3/bin:/Users/jamie/miniconda3/condabin:/Users/jamie/.config/doom/bin:/Users/jamie/.config/emacs/bin:/Users/jamie/.emacs.d/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/Apple/usr/bin:/Library/TeX/texbin:/Users/jamie/.orbstack/bin:/Users/jamie/Desktop/emacs/nextstep/Emacs.app/Contents/MacOS")

;; HACK for mac only
(setq mac-command-modifier 'meta)

;; HACK add homebrew to emacs's exec-path
(add-to-list 'exec-path "/opt/homebrew/bin") 

;; HACK native-comp failes on mac m1
;; see: https://github.com/d12frosted/homebrew-emacs-plus/issues/554#issuecomment-1601274371
(setenv "LIBRARY_PATH"
        (mapconcat 'identity
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

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
  )

;; HACK for vertico
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; -----------------------------------------------------------
;; (my) emacs core thirdparty configurations
;; -----------------------------------------------------------

;; Enable package
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
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
   ))

;; ---------------------------------------------------------------------------
;; TODO

(use-package org-journal)

(use-package flycheck)

(use-package citar)

;; ---------------------------------------------------------------------------

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
  ;; We need to tweak a little bit to make cpplint and eglot to work together.
  ;; see: https://melpa.org/#/flycheck-eglot
  ;;
  ;; see: https://github.com/flycheck/flycheck-eglot
  ;; By default, the Flycheck-Eglot considers the Eglot to be the only
  ;; provider of syntax checks. Other Flycheck checkers are ignored.
  ;; There is a variable `flycheck-eglot-exclusive' that controls this.
  ;; You can override it system wide or for some major modes.
  ;;
  (setq flycheck-eglot-exclusive nil)
  (flycheck-add-next-checker 'eglot-check
                             '(warning . c/c++-googlelint))
  )


(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package edwina
  :ensure t
  :config
  ;; (setq display-buffer-base-action '(display-buffer-below-selected))
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
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  ;; (setq dashboard-startup-banner [VALUE])
  ;; Value can be:
  ;;  - 'official which displays the official emacs logo.
  ;;  - 'logo which displays an alternative emacs logo.
  ;;  - an integer which displays one of the text banners
  ;;    (see dashboard-banners-directory files).
  ;;  - a string that specifies a path for a custom banner
  ;;    currently supported types are gif/image/text/xbm.
  ;;  - a cons of 2 strings which specifies the path of an image to use
  ;;    and other path of a text file to use if image isn't supported.
  ;;    (cons "path/to/image/file/image.png" "path/to/text/file/text.txt").
  ;;  - a list that can display an random banner,
  ;;    supported values are: string (filepath), 'official, 'logo and integers.

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; vertically center content
  (setq dashboard-vertically-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  )

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil ; too slow
        ;; evil-goggles provides a good indicator of what has been affected.
        ;; delete/change is obvious, so I'd rather disable it for these.
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
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Library/Mobile Documents/com~apple~CloudDocs/org-remote/roam"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
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
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
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
        completion-category-overrides '((file (styles . (partial-completion))))))    

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
  ;; * Prefix Keybindings
  ;; :prefix can be used to prevent redundant specification of prefix keys
  ;; again, variables are not necessary and likely not useful if you are only
  ;; using a definer created with `general-create-definer' for the prefixes

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
