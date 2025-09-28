;; -*- Lexical-binding: t; -*-

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

;; setup emacs proxy url if possible
(when (not (boundp' +emacs/proxy))
  (defconst +emacs/proxy "127.0.0.1:10808"))

;; add load path
(add-to-list 'load-path (expand-file-name "lisp" +emacs/repo-directory))

;; -----------------------------------------------------------
;; DONE Setup packages
;; -----------------------------------------------------------

;; Enable package
(require 'package)

;; REVIEW: use tuna mirros
;; (setq package-archives 
;;       '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;;         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;         ))

;; REVIEW use official
;; (setq package-archives 
;;       '(
;;         ("gnu"   . "http://elpa.gnu.org/packages/")
;;         ("nongnu"   . "http://elpa.nongnu.org/nongnu/")
;;         ("org"   . "http://orgmode.org/elpa/")
;;         ("melpa" . "http://melpa.org/packages/")
;;         ))

;; HACK disable check of signature
(setq package-check-signature nil)

;; initialize packages
(package-initialize)

;; make sure package-refresh-contents will only run once
(when (not package-archive-contents)
  (package-refresh-contents))

(require 'cl-macs)

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
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;; -----------------------------------------------------------
;; DONE Configure Core
;; -----------------------------------------------------------

(require 'init-utils) 
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
;; DONE modes and themes
;; -----------------------------------------------------------

;; modes and thems that does not require additional config
(+package/ensure-install-and-use 
 '(
   ;; docker
   docker
   ;; modes
   protobuf-mode
   meson-mode
   ;; themes
   zenburn-theme
   gruvbox-theme
   ;; practice typing
   speed-type 
   ;; show key frequency
   keyfreq
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

  ;; HACK add all eglot-ensured modes 
  ;; This determines what formatter to use in buffers without a
  ;; setting for apheleia-formatter. The keys are major mode
  (add-to-list 'apheleia-mode-alist '(c++-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(rust-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(cmake-ts-mode . cmake-format))
  )

;; -----------------------------------------------------------
;; DONE tree-sitter 
;; -----------------------------------------------------------

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (rassq-delete-all 'c++-mode auto-mode-alist)
  (rassq-delete-all 'c-mode auto-mode-alist)
  (rassq-delete-all 'c-or-c++-mode auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))

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
;; HACK fix cmake-ts-mode syntax table
;; ------------------------------------------------------------------

(defun +cmake-ts-mode/fix-syntax-table ()
  (modify-syntax-entry ?/ "-"))
(add-hook 'cmake-ts-mode-hook #'+cmake-ts-mode/fix-syntax-table)

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
;; DONE elfeed
;; ------------------------------------------------------------------

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds '(
                  ("https://eprint.iacr.org/rss/rss.xml" crypto)
                  ("https://planet.emacslife.com/atom.xml" emacs)
                  ("https://emacs-china.org/latest.rss" emacs)
                  ))
  :config
  ;; HACK from https://github.com/skeeto/elfeed/issues/466#issuecomment-1275327427
  (define-advice elfeed-search--header (:around (oldfun &rest args))
    (if elfeed-db
        (apply oldfun args)
      "No database loaded yet")))
