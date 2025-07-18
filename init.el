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

;; add load path
(add-to-list 'load-path (expand-file-name "lisp" jc-emacs-directory))

;; HACK setup environment
;; see: https://www.emacswiki.org/emacs/ExecPath
;; Set up Emacs' `exec-path' and PATH environment variable to match
;; that used by the user's shell.
;; 
;; This is particularly useful under Mac OS X and macOS, where GUI
;; apps are not started from a shell.
(when (not (eq system-type 'windows-nt))
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" "" (shell-command-to-string
                          "$SHELL --login -c 'echo $PATH'"
                          ))))
    ;; (message path-from-shell)
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(message "%s" exec-path)

;; additional emacs-native configurations
(require 'init-misc)

;; -----------------------------------------------------------
;; DONE Setup packages
;; -----------------------------------------------------------

;; Enable package
(require 'package)

;; setup elpa pacakges
;; (setq package-archives
;;       '(
;;         ("gnu"   . "http://elpa.gnu.org/packages/")
;;         ("nongnu"   . "http://elpa.nongnu.org/nongnu/")
;;         ("org"   . "http://orgmode.org/elpa/")
;;         ("melpa" . "http://melpa.org/packages/")))

(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("org" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; initialize packages
(package-initialize)

;; make sure package-refresh-contents will only run once
(when (not package-archive-contents)
  (package-refresh-contents))

;; HACK see: https://emacs.stackexchange.com/a/53142
;; (setq package-check-signature nil)
;; (package-install 'gnu-elpa-keyring-update)
;; (gnu-elpa-keyring-update)
;; (setq package-check-signature 'allow-unsigned)

;; -----------------------------------------------------------
;; DONE Configure Core
;; -----------------------------------------------------------

(require 'init-utils)
(require 'init-core)
(require 'init-evil)
(require 'init-dired)
(require 'init-org)
(require 'init-latex)
(require 'init-chinese)
(require 'init-os)

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
  
  ;; HACK always get a new eshell
  (defun +eshell/new ()
    (interactive)
    (eshell '(t)))

  ;; HACK kill current persp without asking
  (defun +persp/kill-current ()
    (interactive)
    (persp-kill (persp-current-name)))

  ;; ** keybindings that should not be overriden
  (general-define-key
   :keymaps 'override
   "M-f"     #'consult-line
   "C-M-i"   #'completion-at-point
   "M-y"     #'yas-expand
   "M-a"     #'mark-whole-buffer ; like mac
   "M-s"     #'save-buffer ; like mac
   "M-c"     #'evil-yank ; like mac
   "M-v"     #'evil-paste-before ; like mac
   "M-/"     #'evilnc-comment-or-uncomment-lines
   "C-u"     #'evil-scroll-up
   "C-d"     #'evil-scroll-down
   "C-="     #'cnfonts-increase-fontsize
   "C--"     #'cnfonts-decrease-fontsize
   "C-h"     #'persp-prev
   "C-l"     #'persp-next
   "C-M-h"   #'+persp/move-buffer-prev
   "C-M-l"   #'+persp/move-buffer-next
   ;; "C-SPC"   #'toggle-input-method
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
    "ot"     #'+eshell/new
    "od"     #'dired-jump
    "oc"     #'compile
    "og"     #'magit-status-quick
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
    "pH"     #'+persp/move-buffer-prev
    "pL"     #'+persp/move-buffer-next
    "ph"     #'persp-prev
    "pl"     #'persp-next
    "pq"     #'+persp/kill-current
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
    "nb"      #'citar-open
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
    "tf"     #'toggle-frame-maximized
    "tF"     #'toggle-frame-fullscreen
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
    "sh"     #'consult-history ;; search history
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
  
  ;; org mode map
  (+my-local-leader-def
    :keymaps 'org-mode-map
    :states '(normal visual)
    "e" #'org-export-dispatch
    "j" #'org-present-next
    "k" #'org-present-prev
    "q" #'org-present-quit
    "t" #'org-todo
    "p" #'org-priority
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
