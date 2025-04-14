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
;; (load-theme 'tsdh-dark t)
(load-theme 'modus-vivendi t)

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; set default font
(set-frame-font "0xProto Nerd Font Mono" nil t)

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
   ;; show key helps
   which-key
   consult
   ;; show helps of fun, key, mode
   helpful
   ;; search engine
   vertico
   orderless ; make sure
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
   ;; highlight todo keywords
   hl-todo
   ;; search tool based on ripgrep
   rg
   ;; roam
   org-roam
   ;; citar
   citar
   ;; better error checking
   flycheck
   ;; better dired
   dirvish
   ;; better place to write diaries
   org-journal
   ;; tiling windown manager
   ;; edwina
   ;; dashboard at startup
   dashboard
   ))

;; TODO

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

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

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

(use-package org-journal)

(use-package dirvish
  :config
  (setq dirvish-side-mode-hook (lambda() (display-line-numbers-mode -1)))
  )

(use-package flycheck)

(use-package citar)

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

;; NOTE

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
        corfu-quit-no-match 'separator) ;; or t
  )

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))    

(use-package eglot)

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
  )

;; consult
(use-package consult)

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

  (evil-mode 1)

  ;; Leader key in evil mode
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  (evil-set-leader 'motion (kbd "SPC"))
  (evil-set-leader 'insert (kbd "M-SPC"))

  ;; make sure the follwing key bindings always work
  (evil-define-key nil 'global (kbd "M-f")     #'consult-line)
  (evil-define-key nil 'global (kbd "M-s")     #'save-buffer)
  (evil-define-key nil 'global (kbd "M-c")     #'evil-yank)
  (evil-define-key nil 'global (kbd "M-v")     #'evil-paste-before)
  (evil-define-key nil 'global (kbd "M-/")     #'comment-line)
  (evil-define-key nil 'global (kbd "C-u")     #'evil-scroll-up)
  (evil-define-key nil 'global (kbd "C-d")     #'evil-scroll-down)

  ;; window-related key bindings
  (evil-define-key nil 'global
    (kbd "<leader>wh")     #'evil-window-left
    (kbd "<leader>wj")     #'evil-window-down
    (kbd "<leader>wk")     #'evil-window-up
    (kbd "<leader>wl")     #'evil-window-right
    (kbd "<leader>ww")     #'other-window
    (kbd "<leader>wd")     #'evil-window-delete
    (kbd "<leader>ws")     #'evil-window-split
    (kbd "<leader>wv")     #'evil-window-vsplit
    )

  ;; buffeer-related key bindings
  (evil-define-key nil 'global
    (kbd "<leader><")      #'consult-buffer
    (kbd "<leader>bn")     #'evil-buffer-new
    (kbd "<leader>bd")     #'kill-current-buffer
    (kbd "<leader>br")     #'revert-buffer
    )

  ;; open-related key bindings
  (evil-define-key nil 'global
    (kbd "<leader>ot")     #'projectile-run-vterm-other-window
    (kbd "<leader>od")     #'dired-jump
    (kbd "<leader>og")     #'magit-status
    (kbd "<leader>op")     #'dirvish-side
    )

  ;; prject-related key bindings
  (evil-define-key nil 'global
    (kbd "<leader>pa")     #'projectile-add-known-project
    (kbd "<leader>px")     #'projectile-remove-known-project
    (kbd "<leader>pp")     #'projectile-switch-project
    (kbd "<leader>pc")     #'projectile-compile-project
    (kbd "<leader>pt")     #'projectile-test-project
    (kbd "<leader>pr")     #'projectile-run-project
    (kbd "<leader>pd")     #'projectile-kill-buffers
    (kbd "<leader>pi")     #'projectile-invalidate-cache
    (kbd "<leader>pf")     #'consult-ripgrep
    (kbd "<leader>SPC")    #'projectile-find-file
    )

  ;; note functions
  (evil-define-key nil 'global
    (kbd "<leader>nrf")     #'org-roam-node-find
    (kbd "<leader>nri")     #'org-roam-node-insert
    ;; (kbd "<leader>njj")     #'org-roam-node-insert
    )

  ;; help functions
  (evil-define-key nil 'global
    (kbd "<leader>hf")     #'helpful-callable
    (kbd "<leader>hk")     #'helpful-key
    (kbd "<leader>hv")     #'helpful-variable
    (kbd "<leader>hm")     #'describe-mode
    )

  ;; quit emacs
  (evil-define-key nil 'global
    (kbd "<leader>qq")     #'save-buffers-kill-terminal
    (kbd "<leader>qr")     #'restart-emacs
  )
)


;;; evil-collection
(use-package evil-collection
  :init
  (setq evil-want-keybinding nil)
  :after evil
  :config
  (evil-collection-init))
