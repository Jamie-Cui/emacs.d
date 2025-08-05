;; -*- lexical-binding: t; -*-

;; -----------------------------------------------------------
;; DONE Emacs native configurations
;; -----------------------------------------------------------

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

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

;; additional emacs-native configurations
(require 'init-misc)

;; -----------------------------------------------------------
;; DONE Setup packages
;; -----------------------------------------------------------

;; Enable package
(require 'package)

(setq package-archives 
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ;; ("gnu"   . "http://elpa.gnu.org/packages/")
        ;; ("nongnu"   . "http://elpa.nongnu.org/nongnu/")
        ;; ("org"   . "http://orgmode.org/elpa/")
        ;; ("melpa" . "http://melpa.org/packages/")
        ))

;; initialize packages
(package-initialize)

;; make sure package-refresh-contents will only run once
(when (not package-archive-contents)
  (package-refresh-contents))

(require 'cl-macs)

(defun +package/ensure-install (packages-alist)
  "Make sure the given package is installed."
  (dolist (p packages-alist)
    (unless (package-installed-p p)
      (package-install p))))

(defun +package/ensure-install-and-use (packages-alist)
  "Make sure the given package is installed."
  (dolist (p packages-alist)
    (unless (package-installed-p p)
      (package-install p)
      (use-package p)
      )))

;; -----------------------------------------------------------
;; DONE Configure Core
;; -----------------------------------------------------------

(require 'init-kbd) ;; keybindings 
(require 'init-evil) 
(require 'init-core)
(require 'init-org)
(require 'init-chinese)
(require 'init-os)
(require 'init-llm)

;; only load latex when using graphic
(when (display-graphic-p)
  (require 'init-latex))

(when (not (display-graphic-p))
  (require 'init-tty))

;; -----------------------------------------------------------
;; DONE programming modes
;; -----------------------------------------------------------

;; modes that does not require additional config
(+package/ensure-install-and-use
 '(
   protobuf-mode
   meson-mode
   )
 )

(+package/ensure-install
 '(
   ;; bazel mode (need config)
   bazel
   ;; cmake mode (need config)
   cmake-mode
   ;; markdown mode
   markdown-mode
   ;; k8s
   kubernetes 
   kubernetes-evil
   ;; automatically install treesit grammar
   treesit-auto
   ))

;; -----------------------------------------------------------
;; DONE tree-sitter (site-lisp)
;; -----------------------------------------------------------

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)

  ;; HACK toggle mode automatically
  (defun +treesit-auto/toggle ()
    "Toggle global-treesit-auto-mode."
    (interactive)
    (if global-treesit-auto-mode
        (progn
          (global-treesit-auto-mode -1)
          (message "global-treesit-auto-mode disabled"))
      (global-treesit-auto-mode 1)
      (message "global-treesit-auto-mode enabled")))
  )

;; -----------------------------------------------------------
;; DONE kubenetes
;; -----------------------------------------------------------

(use-package kubernetes
  :ensure t
  :custom
  (kubernetes-show-message nil)
  :config
  ;; set custom display function
  (defun kubernetes-commands-display-buffer-same-window (buffer)
    (display-buffer buffer '(display-buffer-same-window)))
  (setopt kubernetes-commands-display-buffer-function
          'kubernetes-commands-display-buffer-same-window)

  ;; disable auto refresh
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

;; ------------------------------------------------------------------
;; DONE C/C++, cmake and bazel
;; ------------------------------------------------------------------

;; sibling files (for cpp)
(add-to-list 'find-sibling-rules
             '("/\\([^/]+\\)\\.c\\(c\\|pp\\)?\\'" "\\1.h\\(h\\|pp\\)?\\'"))
(add-to-list 'find-sibling-rules
             '("/\\([^/]+\\)\\.h\\(h\\|pp\\)?\\'" "\\1.c\\(c\\|pp\\)?\\'"))

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
;; DONE Markdown mode
;; ------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

