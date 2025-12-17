;; -*- lexical-binding: t; -*-

;;; -----------------------------------------------------------
;;; DONE Emacs native configurations
;;; -----------------------------------------------------------

;; Require Emacs 30.1 for better performance and features
(let ((minver "30.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(defcustom +emacs/repo-directory (expand-file-name "~/.emacs.d")
  "Path to emacs.d folder"
  :type 'string
  :group 'convenience)

(defcustom +emacs/org-root-dir (expand-file-name "~/org-root")
  "Path to org-root folder"
  :type 'string
  :group 'convenience)

(defcustom +emacs/proxy "127.0.0.1:10808"
  "http/https proxy."
  :type 'string
  :group 'convenience)

(when (boundp 'url-proxy-services)
  (add-to-list 'url-proxy-services `("http" . ,+emacs/proxy))
  (add-to-list 'url-proxy-services `("https" . ,+emacs/proxy)))

;; add load path
(add-to-list 'load-path (expand-file-name "lisp" +emacs/repo-directory))

;;; -----------------------------------------------------------
;;; DONE Setup folders
;;; -----------------------------------------------------------

(make-directory (concat +emacs/org-root-dir "/roam") t)
(make-directory (concat +emacs/org-root-dir "/journal") t)
(make-directory (concat +emacs/org-root-dir "/deft") t)
(make-directory (concat (file-name-directory user-init-file) "/bin") t)

;;; -----------------------------------------------------------
;;; DONE Setup packages
;;; -----------------------------------------------------------

;; Emacs 30+: Use package-quickstart for faster startup
(setopt package-quickstart t)

;; Enable package
(require 'package)

;; disable check of signature
(setq package-check-signature nil)

;; Use parallel package initialization for better performance
(setq package-native-compile t)

;; DEPRECATED Initialize packages (deferred for faster startup)
;; (package-initialize t)

;; Use package-quickstart-refresh for efficient package loading
(when (not (file-exists-p (expand-file-name "package-quickstart.el" package-user-dir)))
  (package-refresh-contents)
  (package-quickstart-refresh))

;; Use cl-lib for better performance

(require 'cl-lib)

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

;;; -----------------------------------------------------------
;;; DONE Configure Core - Deferred Loading for Faster Startup
;;; -----------------------------------------------------------

;; Emacs 30+: Use with-eval-after-load for deferred loading
(require 'init-utils)

;; Load essential modules immediately, defer non-essential ones
(require 'init-kbd) 
(require 'init-evil) 

;; Defer non-essential modules for faster startup
(with-eval-after-load 'evil
  (require 'init-core)
  (require 'init-misc)
  (require 'init-os))

;; Defer LLM and org modules
(with-eval-after-load 'init-core
  (require 'init-llm))

(with-eval-after-load 'init-llm
  (require 'init-org))

;; only load latex when using graphic (deferred)
(when (display-graphic-p)
  (with-eval-after-load 'init-org
    (require 'init-latex)))

;;; -----------------------------------------------------------
;;; DONE modes and themes - Deferred Loading
;;; -----------------------------------------------------------

;; Emacs 30+: Defer package loading to improve startup time
;; Use use-package with :defer keyword for better startup performance
(use-package protobuf-mode
  :defer t)

(use-package meson-mode
  :defer t)

(use-package zenburn-theme
  :ensure t)

(use-package gruvbox-theme
  :ensure t)

(use-package speed-type
  :defer t)

(use-package keyfreq
  :defer t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; -----------------------------------------------------------
;;; DONE flycheck-google-cpplint
;;; -----------------------------------------------------------

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

;;; -----------------------------------------------------------
;;; DONE apheleia - Deferred Loading
;;; -----------------------------------------------------------

(use-package apheleia
  :ensure t
  :custom
  (apheleia-remote-algorithm 'local)
  :hook (after-init . (lambda () (apheleia-global-mode +1)))
  ;; NOTE use elgot-format
  ;; https://github.com/radian-software/apheleia/issues/153#issuecomment-1446651497
  ;; (cl-defun apheleia-indent-eglot-managed-buffer
  ;;     (&key buffer scratch callback &allow-other-keys)
  ;;   (with-current-buffer scratch
  ;;     (setq-local eglot--cached-server
  ;;                 (with-current-buffer buffer
  ;;                   (eglot-current-server)))
  ;;     (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
  ;;       (eglot-format-buffer))
  ;;     (funcall callback)))

  ;; declare new formatters for eglot
  ;; (add-to-list 'apheleia-formatters
  ;;              '(eglot-managed . apheleia-indent-eglot-managed-buffer))

  ;; NOTE add all eglot-ensured modes 
  ;; This determines what formatter to use in buffers without a
  ;; setting for apheleia-formatter. The keys are major mode
  ;; (add-to-list 'apheleia-mode-alist '(c++-ts-mode-hook . eglot-managed))
  ;; (add-to-list 'apheleia-mode-alist '(rust-ts-mode-hook . eglot-managed))
  ;; (add-to-list 'apheleia-mode-alist '(cmake-ts-mode . cmake-format))
  )

;;; -----------------------------------------------------------
;;; DONE tree-sitter - Optimized for Emacs 30+
;;; -----------------------------------------------------------

;; Emacs 30+: Enable native compilation and use treesit-auto for better performance
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :hook (after-init . (lambda () 
                        (treesit-auto-add-to-auto-mode-alist 'all)
                        ;; Configure tree-sitter modes
                        (rassq-delete-all 'c++-mode auto-mode-alist)
                        (rassq-delete-all 'c-mode auto-mode-alist)
                        (rassq-delete-all 'c-or-c++-mode auto-mode-alist)
                        (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))
                        (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))
                        (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-ts-mode))
                        (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
                        (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))
                        ;; Auto-enable global-treesit-auto-mode
                        (global-treesit-auto-mode 1)))

  ;; NOTE toggle mode automatically
  :config
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
;; NOTE fix cmake-ts-mode syntax table
;; ------------------------------------------------------------------

(defun +cmake-ts-mode/fix-syntax-table ()
  (modify-syntax-entry ?/ "-"))
(add-hook 'cmake-ts-mode-hook #'+cmake-ts-mode/fix-syntax-table)

(use-package eldoc-cmake
  :ensure t
  :hook (cmake-ts-mode . eldoc-cmake-enable))

;; ------------------------------------------------------------------
;;; DONE bazel mode
;; ------------------------------------------------------------------

(use-package bazel
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.BUILD\\'" . bazel-mode))
  (setq bazel-buildifier-before-save 't))

;; ------------------------------------------------------------------
;;; DONE Markdown mode
;; ------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))


;; ------------------------------------------------------------------
;;; DONE (optional) elfeed
;; ------------------------------------------------------------------

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds '(
                  ("http://nullprogram.com/feed/")
                  ("https://planet.emacslife.com/atom.xml")
                  ("https://stallman.org/rss/rss.xml")
                  ("https://eprint.iacr.org/rss/rss.xml")
                  ("https://emacs-china.org/latest.rss")
                  ))
  :config
  ;; NOTE this is a hack from
  ;; https://github.com/skeeto/elfeed/issues/466#issuecomment-1275327427
  (define-advice elfeed-search--header (:around (oldfun &rest args))
    (if elfeed-db
        (apply oldfun args)
      "No database loaded yet")))

