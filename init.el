;; -*- lexical-binding: t; -*-

;; -----------------------------------------------------------
;; DONE Emacs native configurations
;; -----------------------------------------------------------

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "28.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; setup emacs configuration dir
(when (not (boundp' jc-emacs-directory))
  (defconst jc-emacs-directory "~/emacs.d"))

;; setup emacs org dir
(when (not (boundp' jc-org-root-dir))
  (defconst jc-org-root-dir "~/org-root"))

(add-to-list 'load-path (expand-file-name "lisp" jc-emacs-directory))

(require 'init-funs)
(require 'init-core)
(require 'init-evil)
(require 'init-dired)
(require 'init-org)
(require 'init-latex)
(require 'init-chinese)
(require 'init-os)
(require 'init-misc)

;; Set up Emacs' `exec-path' and PATH environment variable to match
;; that used by the user's shell.
(+set-emacs-exec-path-from-shell-PATH)

;; -----------------------------------------------------------
;; DONE programming modes
;; -----------------------------------------------------------

(+ensure-packages-installed
 '(
   ;; bazel mode (need config)
   bazel
   ;; cmake mode (need config)
   cmake-mode
   ;; protobuf mdoe
   protobuf-mode
   ;; meson mode
   meson-mode
   ;; markdown mode
   markdown-mode
   ;; yaml mode
   yaml-mode
   ;; pdf-tools
   pdf-tools
   ))

(use-package protobuf-mode)
(use-package meson-mode)
(use-package markdown-mode)
(use-package yaml-mode)

;; ------------------------------------------------------------------
;; DONE C/C++, cmake and bazel
;; ------------------------------------------------------------------

;; add auto-mode list
;; I prefer to use treesit-mode
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

;; sibling files
(add-to-list 'find-sibling-rules '("/\\([^/]+\\)\\.c\\(c\\|pp\\)?\\'" "\\1.h\\(h\\|pp\\)?\\'"))
(add-to-list 'find-sibling-rules '("/\\([^/]+\\)\\.h\\(h\\|pp\\)?\\'" "\\1.c\\(c\\|pp\\)?\\'"))

;; formatter
(add-to-list 'apheleia-mode-alist '(c++-ts-mode-hook . eglot-managed))
(add-to-list 'apheleia-mode-alist '(c-ts-mode-hook . eglot-managed))
(add-to-list 'apheleia-mode-alist '(cmake-mode . cmake-format))

(use-package cmake-mode
  :ensure t
  :config
  (defun +my-modify-cmake-mode-syntax-table ()
    (modify-syntax-entry ?/ "-" cmake-mode-syntax-table))
  (add-hook 'cmake-mode-hook #'+my-modify-cmake-mode-syntax-table))

(use-package bazel
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.BUILD\\'" . bazel-mode))
  (setq bazel-buildifier-before-save 't))

;; ------------------------------------------------------------------
;; TODO Rust mode
;; ------------------------------------------------------------------

(let* ((rust-files '(".rs"))
       (rust-regexp (concat (regexp-opt rust-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons rust-regexp 'rust-ts-mode)))

(let* ((rust-files '("Cargo.lock"))
       (rust-regexp (concat (regexp-opt rust-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons rust-regexp 'conf-toml-mode)))

;; ------------------------------------------------------------------
;; TODO Zig mode 
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; Markdown mode
;; ------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; ------------------------------------------------------------------
;; DONE Key Bindings
;; ------------------------------------------------------------------

(use-package general
  :ensure t
  :after (evil evil-mc)
  :config
  (defconst my-leader "SPC")
  (defconst my-local-leader "SPC m")

  (general-create-definer +my-leader-def
    :prefix my-leader)

  (general-create-definer +my-local-leader-def
    :prefix my-local-leader)

  (general-define-key
   :states '(insert replace normal visual operator)
   "C-g" #'evil-escape
   )

  ;; HACK kill current persp without asking
  (defun +persp/kill-current ()
    (interactive)
    (persp-kill (persp-current-name)))

  ;; ** keybindings that should not be overriden
  (general-define-key
   :keymaps 'override
   "M-f"     #'consult-line
   "M-Y"     #'consult-yasnippet
   "M-y"     #'yas-expand
   "M-s"     #'save-buffer
   "M-c"     #'evil-yank
   "M-v"     #'evil-paste-before
   "M-/"     #'evilnc-comment-or-uncomment-lines
   "M-z"     #'evil-undo
   "M-Z"     #'evil-redo
   "M-a"     #'mark-whole-buffer
   "C-u"     #'evil-scroll-up
   "C-d"     #'evil-scroll-down
   "C-="     #'cnfonts-increase-fontsize
   "C--"     #'cnfonts-decrease-fontsize
   "C-SPC"   #'toggle-input-method
   "C-S-h"   #'+persp/move-buffer-prev
   "C-S-l"   #'+persp/move-buffer-next
   "C-h"     #'persp-prev
   "C-l"     #'persp-next
   "C-q"     #'+persp/kill-current
   )

  ;; ** Global Keybindings
  (+my-leader-def
    :states 'normal
    :keymaps 'override ; prevent from being override
    "a" '(:ignore t :which-key "actions")
    "a RET"  #'embark-dwim
    ;; window-related key bindings

    "w" '(:ignore t :which-key "window")
    "wh"     #'evil-window-left
    "wj"     #'evil-window-down
    "wk"     #'evil-window-up
    "wl"     #'evil-window-right
    "ww"     #'other-window
    "wx"     #'evil-window-exchange
    "wd"     #'evil-window-delete
    "ws"     #'evil-window-split
    "wv"     #'evil-window-vsplit
    "wm"     #'delete-other-windows
    ;; buffeer-related key bindings
    "b" '(:ignore t :which-key "buffer")
    "bn"     #'evil-buffer-new
    "bd"     #'kill-current-buffer
    "br"     #'+revert-buffer-no-confirm
    "bc"     #'clean-buffer-list
    "bo"     #'persp-kill-other-buffers
    "B" '(:ignore t :which-key "bookmark")
    "BB"     #'consult-bookmark
    "Bn"     #'bookmark-set
    "Bd"     #'bookmark-delete
    ;; open-related key bindings
    "o" '(:ignore t :which-key "open")
    "ot"     #'eshell
    "oT"     #'project-eshell
    "od"     #'dired-jump
    "oD"     #'projectile-dired
    "og"     #'magit-status-quick
    "oG"     #'magit-project-status
    ;; project-related key bindings
    "p" '(:ignore t :which-key "project")
    "pa"     #'projectile-add-known-project
    "pd"     #'projectile-remove-known-project
    "pp"     #'projectile-switch-project
    "pC"     #'projectile-configure-project
    "pc"     #'projectile-compile-project
    "pt"     #'projectile-test-project
    "pr"     #'projectile-run-project
    "pi"     #'projectile-invalidate-cache
    "pf"     #'+vertico/project-search
    "po"     #'find-sibling-file
    ;; note functions
    "n" '(:ignore t :which-key "note")
    "n@"      #'citar-insert-citation
    "ny"      #'org-store-link
    "np"      #'org-insert-link
    "ne"      #'org-export-dispatch
    "nd"      #'deft
    "nj"      #'org-journal-new-entry
    "nrf"     #'org-roam-node-find
    "nri"     #'org-roam-node-insert
    "nrs"     #'org-roam-db-sync
    "nb"     #'citar-open
    ;; help functions
    "h" '(:ignore t :which-key "help")
    "hf"     #'helpful-callable
    "hk"     #'helpful-key
    "hv"     #'helpful-variable
    "hm"     #'describe-mode
    ;; quit emacs
    "q" '(:ignore t :which-key "quit")
    "qq"     #'save-buffers-kill-terminal
    "qr"     #'restart-emacs
    ;; toggles
    "t" '(:ignore t :which-key "toggle")
    "th"     #'hs-hide-level
    "tf"     #'toggle-frame-fullscreen
    "tt"     #'toggle-truncate-lines
    "tc"     #'display-fill-column-indicator-mode
    "tg"     #'magit-blame-addition
    ;; code
    "c" '(:ignore t :which-key "code")
    "cx"     #'list-flycheck-errors
    "ca"     #'eglot-code-actions
    "cr"     #'eglot-rename
    "cf"     #'eglot-format-buffer
    "cj"     #'consult-eglot-symbols
    ;; search
    "s" '(:ignore t :which-key "search")
    "si"     #'consult-imenu ;; search item
    ;; other
    "RET"    #'gptel
    "x"      #'scratch-buffer
    "."      #'find-file
    "<"      #'consult-buffer
    ","      #'consult-project-buffer
    "/"      #'+vertico/project-search
    "TAB"    #'evil-switch-to-windows-last-buffer
    "SPC"    #'projectile-find-file
    )

  ;; deft mode map
  (+my-local-leader-def
    :keymaps 'deft-mode-map
    :states '(normal visual)
    "A" #'deft-archive-file
    "n" #'deft-new-file
    "f" #'deft-filter
    "d" #'deft-delete-file
    "g" #'deft-refresh
    )
  )

;; -----------------------------------------------------------
;; DONE org-imgtog
;; -----------------------------------------------------------

(use-package org-imgtog
  :load-path (lambda () (concat jc-emacs-directory "/site-lisp"))
  :hook org-mode)


(use-package ultra-scroll
  :load-path (lambda () (concat jc-emacs-directory "/site-lisp"))
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1)
  )

;; -----------------------------------------------------------
;; DONE pdf-tools
;; -----------------------------------------------------------

(use-package pdf-tools
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-loader-install)
  (add-hook
   'org-mode-hook
   '(lambda ()
      (delete '("\\.pdf\\'" . default) org-file-apps)
      (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdftools-open))))
  )
