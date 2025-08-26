;; -*- lexical-binding: t; -*-

;; -----------------------------------------------------------
;; DONE Emacs native configurations
;; -----------------------------------------------------------

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; setup emacs configuration dir
(when (not (boundp' +emacs/repo-directory))
  (defconst +emacs/repo-directory "~/emacs.d"))

;; setup emacs org dir
(when (not (boundp' +emacs/org-root-dir))
  (defconst +emacs/org-root-dir "~/org-root"))

;; add load path
(add-to-list 'load-path (expand-file-name "lisp" +emacs/repo-directory))

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

;; HACK disable check of signature
(setq package-check-signature nil)

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
;; DONE Setup environment
;; -----------------------------------------------------------

(when (not (eq system-type 'windows-nt))
  (+package/ensure-install '(exec-path-from-shell))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;; -----------------------------------------------------------
;; DONE Configure Core
;; -----------------------------------------------------------

(require 'init-kbd) 
(require 'init-evil) 
(require 'init-core)
(require 'init-misc)
(require 'init-org)
(require 'init-os)
(require 'init-llm)

;; only load latex when using graphic
(when (display-graphic-p)
  (require 'init-latex))

;; -----------------------------------------------------------
;; DONE programming modes
;; -----------------------------------------------------------

;; modes that does not require additional config
(+package/ensure-install-and-use 
 '(
   protobuf-mode
   meson-mode
   ))

(+package/ensure-install 
 '(
   ;; code auto formating
   apheleia
   ;; bazel mode (need config)
   bazel
   ;; markdown mode
   markdown-mode
   ;; automatically install treesit grammar
   treesit-auto
   ;; cpplint
   flycheck-google-cpplint
   ))

;; -----------------------------------------------------------
;; DONE flycheck-google-cpplint
;; -----------------------------------------------------------

(use-package flycheck-google-cpplint
  :ensure t
  :after flycheck-eglot
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
    ))
  :config
  (flycheck-add-next-checker 'eglot-check
                             '(warning . c/c++-googlelint))
  )

;; -----------------------------------------------------------
;; DONE apheleia
;; -----------------------------------------------------------

(use-package apheleia
  :ensure t
  :custom
  (apheleia-remote-algorithm 'local)
  :config
  (apheleia-global-mode +1)
  ;; HACK use elgot-format
  ;; https://github.com/radian-software/apheleia/issues/153#issuecomment-1446651497
  (cl-defun apheleia-indent-eglot-managed-buffer
      (&key buffer scratch callback &allow-other-keys)
    (with-current-buffer scratch
      (setq-local eglot--cached-server
                  (with-current-buffer buffer
                    (eglot-current-server)))
      (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
        (eglot-format-buffer))
      (funcall callback)))

  ;; declare new formatters for eglot
  (add-to-list 'apheleia-formatters
               '(eglot-managed . apheleia-indent-eglot-managed-buffer))

  ;; HACK use bibtex-reformat
  ;; https://github.com/radian-software/apheleia/pull/294
  (cl-defun apheleia-reformat-bibtex-buffer
      (&key buffer scratch callback &allow-other-keys)
    (with-current-buffer scratch
      (funcall (with-current-buffer buffer major-mode))
      (bibtex-reformat)
      (funcall callback)))

  ;; declare new formatters for eglot
  (add-to-list 'apheleia-formatters
               '(bibtex-format . apheleia-reformat-bibtex-buffer))

  ;; HACK add all eglot-ensured modes 
  ;; This determines what formatter to use in buffers without a
  ;; setting for apheleia-formatter. The keys are major mode
  (add-to-list 'apheleia-mode-alist '(c++-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(rust-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(cmake-ts-mode . cmake-format))
  (add-to-list 'apheleia-mode-alist '(bibtex-mode . bibtex-format))
  )

;; -----------------------------------------------------------
;; DONE tree-sitter 
;; -----------------------------------------------------------

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (rassq-delete-all 'c++-mode auto-mode-alist)
  (rassq-delete-all 'c-mode auto-mode-alist)
  (rassq-delete-all 'c-or-c++-mode auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))

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

;; ------------------------------------------------------------------
;; DONE bazel mode
;; ------------------------------------------------------------------

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

;; ------------------------------------------------------------------
;; TODO EAF
;; ------------------------------------------------------------------

;; (use-package eaf
;;   :load-path (lambda () (concat +emacs/repo-directory "/thirdparty/eaf"))
;;   :config
;;   (require 'eaf-browser)
;;   (require 'eaf-pdf-viewer)
;;   )
