;; -*- lexical-binding: t; -*-

;;; -----------------------------------------------------------
;;; DONE Startup Performance Optimizations
;;; -----------------------------------------------------------

(setq load-prefer-newer t)

;; Increase GC threshold during startup for faster initialization
;; Default is 800KB, which causes too many GC pauses during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Unset file name handler during init for faster I/O
;; This is restored after startup (see emacs-startup-hook below)
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Increase read-process-output-max for faster LSP/external processes
;; Default is 4KB on macOS, which is too small for LSP responses
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; Function to reset performance settings to normal values after startup
(defun +emacs/reset-performance-settings ()
  "Reset garbage collection and file handler settings after startup."
  (setq gc-cons-threshold (* 16 1024 1024))  ; 16MB normal value
  (setq gc-cons-percentage 0.1)
  (setq file-name-handler-alist default-file-name-handler-alist))

;; Add hook to reset settings after Emacs startup
(add-hook 'emacs-startup-hook #'+emacs/reset-performance-settings)

;;; -----------------------------------------------------------
;;; DONE Emacs native configurations
;;; -----------------------------------------------------------

;; Require Emacs 30.1 for better performance and features
(let ((minver "30.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Work around an Emacs 30.2 native-comp regression in built-in Org.
(defvar native-comp-jit-compilation-deny-list nil)
(dolist (regexp '(".*org-element.*" ".*org-macs.*"))
  (add-to-list 'native-comp-jit-compilation-deny-list regexp))

(defcustom +emacs/repo-directory (expand-file-name "~/.emacs.d")
  "Path to emacs.d folder"
  :type 'string
  :group 'convenience)

(defcustom +emacs/org-root-dir (expand-file-name "~/opt/org-root")
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
;;; DONE Setup folders - Lazy Creation
;;; -----------------------------------------------------------

;; Lazily create directories only when first accessed
(defun +emacs/ensure-directory (dir)
  "Lazily create DIR when first accessed.
Returns DIR after ensuring it exists."
  (make-directory dir t)
  dir)

;; Helper for org subdirectories
(defun +emacs/org-subdir (name)
  "Return path to org subdirectory NAME, creating it if needed."
  (+emacs/ensure-directory (concat +emacs/org-root-dir "/" name)))

;; Bin directory helper
(defun +emacs/bin-directory ()
  "Return path to bin directory, creating it if needed."
  (when (and user-init-file (stringp user-init-file))
    (+emacs/ensure-directory
     (concat (file-name-directory user-init-file) "/bin"))))

;;; -----------------------------------------------------------
;;; DONE Setup packages
;;; -----------------------------------------------------------

;; Load cl-lib first (required by package-quickstart)
(require 'cl-lib)

;; Emacs 30+: Use package-quickstart for faster startup
(setopt package-quickstart t)

;; Enable package
(require 'package)

;; Keep package archives explicit so local `site-lisp' packages can ensure
;; their external archive dependencies on a fresh machine.
;; (setq package-archives
;;       '(("gnu" . "https://elpa.gnu.org/packages/")
;;         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;         ("org" . "https://orgmode.org/elpa/")
;;         ("melpa" . "https://melpa.org/packages/")))

;; disable check of signature
(setq package-check-signature nil)

;; Use parallel package initialization for better performance
(setq package-native-compile t)

(setq package-install-upgrade-built-in t)

;; Initialize packages only if not already initialized
;; (package-quickstart may have already initialized packages)
(unless package--initialized
  (package-initialize))

;; Use package-quickstart-refresh for efficient package loading
;; (when (not (file-exists-p (expand-file-name "package-quickstart.el" package-user-dir)))
;;   (package-refresh-contents)
;;   (package-quickstart-refresh))

;; -----------------------------------------------------------
;; DONE Setup environment and compilation
;; -----------------------------------------------------------

(unless (eq system-type 'windows-nt)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;;; -----------------------------------------------------------
;;; DONE Development Tools & Modes
;;; -----------------------------------------------------------

;; -----------------------------------------------------------
;; DONE Load modules
;; -----------------------------------------------------------

;; Core utilities (must be loaded first)
(require 'init-utils)

;; Essential modules (immediate load)
(require 'init-evil)
(require 'init-kbd)
(require 'init-completion)

;; Core functionality (immediate load)
;; NOTE: Original with-eval-after-load 'init-utils was a no-op since init-utils
;; was already loaded above. Keeping immediate load for reliability.
(require 'init-core)
(require 'init-misc)
(require 'init-os)

;; Advanced features
(require 'init-llm)
(require 'init-org)

;; LaTeX support (GUI only)
(when (display-graphic-p)
  (require 'init-latex))

(use-package nael
  :ensure t
  :defer t
  :custom
  (nael-prepare-lsp nil)
  (nael-prepare-eglot t)
  :config
  (add-hook 'nael-mode-hook #'abbrev-mode)
  (add-hook 'nael-mode-hook #'eglot-ensure))

(use-package protobuf-mode
  :ensure t
  :defer t)

(use-package meson-mode
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package gruvbox-theme
  :ensure t
  :defer t)

(use-package speed-type
  :ensure t
  :defer t)

(use-package keyfreq
  :ensure t
  :hook (after-init . keyfreq-mode)
  :config
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
                        ;; (global-treesit-auto-mode 1)
                        ))
  :config
  ;; NOTE toggle mode automatically
  (defun +treesit-auto/toggle ()
    "Toggle global-treesit-auto-mode."
    (interactive)
    (if global-treesit-auto-mode
        (progn
          (global-treesit-auto-mode -1)
          (message "global-treesit-auto-mode disabled"))
      (global-treesit-auto-mode 1)
      (message "global-treesit-auto-mode enabled"))))

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
;; DONE (optional) elfeed
;; ------------------------------------------------------------------

(use-package elfeed
  :ensure t
  :custom
  (elfeed-db-directory (concat user-emacs-directory "elfeed/db/"))
  (elfeed-enclosure-default-dir (concat user-emacs-directory "elfeed/enclosure/"))
  (elfeed-search-filter "") ; startup with no filter
  (elfeed-feeds '(
                  ;; Emacs life
                  ("https://planet.emacslife.com/atom.xml")
                  ;; Emacs China
                  ("https://emacs-china.org/latest.rss")
                  ;; Linux Do
                  ("https://linux.do/latest.rss")
                  ;; iacr
                  ("https://eprint.iacr.org/rss/rss.xml")
                  ;; Linux Security
                  ("https://linuxsecurity.com/linuxsecurity_hybrid.xml")
                  ;; LWN
                  ("http://lwn.net/headlines/rss")
                  ;; Feisty Duck's Security and Cryptography newsletter
                  ("https://www.feistyduck.com/newsletter/feed")
                  ;; Hack News front page
                  ("https://hnrss.org/frontpage")
                  ;; 机器之心
                  ("https://plink.anyfeeder.com/weixin/almosthuman2014")
                  ;; 新智元
                  ("https://plink.anyfeeder.com/weixin/AI_era")
                  ;; 架构师之路
                  ("https://plink.anyfeeder.com/weixin/gh_10a6b96351a9")
                  ;; 棱镜
                  ("https://plink.anyfeeder.com/weixin/lengjing_qqfinance")
                  ;; 求是网
                  ("https://plink.anyfeeder.com/qstheory")
                  )))

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup))

(use-package spinner
  :ensure t)

(use-package telega
  :ensure t
  :config
  (let* ((parts (split-string +emacs/proxy ":"))
         (server (car parts))
         (port (string-to-number (cadr parts))))
    (setq telega-proxies
          (list `(:server ,server
                          :port ,port
                          :enable t
                          :type (:@type "proxyTypeSocks5"))))))

(+use-package-when-dir-exists magent
    (concat +emacs/repo-directory "/site-lisp/magent")
  :after gptel spinner
  :demand t
  :custom
  (magent-skill-directories (list (expand-file-name "skills" +emacs/repo-directory)))
  (magent-by-pass-permission t)
  ;; (magent-ui-wrap-reasoning-in-think-block nil)
  :config
  (global-magent-mode 1)

  ;; keybindings that should not be overriden
  (general-define-key
   :keymaps 'magent-output-mode-map
   :states '(normal visual motion)
   "?"   #'magent-transient-menu
   ))

;; Local packages loaded from `site-lisp' do not install `Package-Requires'.
(use-package websocket
  :ensure t
  :defer t)

(use-package webdriver
  :ensure t
  :defer t)

(use-package plz
  :ensure t
  :defer t)

(+use-package-when-dir-exists overleaf-project
    (concat +emacs/repo-directory "/site-lisp/overleaf-project")
  :demand t
  :custom
  (overleaf-use-nerdfont nil)
  :config
  ;; NOTE install this first
  ;; https://github.com/mozilla/geckodriver/releases

  ;; Example: load/save cookies from GPG encrypted file.
  ;;          (remove the .gpg extension to save unencrypted)
  (let ((cookie-file (concat user-emacs-directory "/.overleaf-cookies.gpg")))
    (setq overleaf-save-cookies
          (overleaf-save-cookies-to-file cookie-file))
    (setq overleaf-cookies
          (overleaf-read-cookies-from-file cookie-file))))

;; (+use-package-when-dir-exists edraw
;;     (concat +emacs/repo-directory "/site-lisp/el-easydraw")
;;   :after org
;;   :demand t
;;   :config
;;   (require 'edraw-org)
;;   (edraw-org-setup-default)
;;   (edraw-org-setup-exporter)

;;   ;; keybindings that should not be overriden
;;   (general-define-key
;;    :keymaps 'edraw-editor-map
;;    "<backspace>"   #'edraw-editor-delete-selected
;;    )
;;   )

;; HACK
(auto-compression-mode 0)
(auto-compression-mode 1)

;; start emacs server
(server-start)
