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
(load-theme 'tsdh-dark t)

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; set default font
(set-frame-font "0xProto Nerd Font Mono 16" nil t)

;; disable certain things
(menu-bar-mode 0)
(tool-bar-mode 0)
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
   ))

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
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

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

(use-package magit
  :after evil
  :config
  ;; disable SPACE key binding
  (evil-define-key nil 'global (kbd "M-f")     #'consult-line)
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
  :init
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
    (kbd "<leader>bb")     #'consult-buffer
    (kbd "<leader>bn")     #'evil-buffer-new
    (kbd "<leader>bd")     #'kill-current-buffer
    )

  ;; open-related key bindings
  (evil-define-key nil 'global
    (kbd "<leader>ot")     #'vterm-other-window
    (kbd "<leader>od")     #'dired-jump
    (kbd "<leader>og")     #'magit-status
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
    (kbd "<leader>qr")     #'restart-emacs)
  )

;;; evil-collection
(use-package evil-collection
  :init
  (setq evil-want-keybinding nil)
  :after evil
  :config
  (evil-collection-init))
