;;; prog.el --- general programming support -*- lexical-binding: t -*-
;;; Commentary:
;; General programming support: eglot, flycheck, citre, apheleia, treesit and
;; compilation configuration.
;;; Code:


(use-package citre
  :ensure t
  :after (eglot projectile)
  :init
  ;; This is needed in `:init' block for lazy load to work.
  ;; (require 'citre-config)
  :custom
  ;; (citre-project-root-function #'projectile-project-root)
  (citre-default-create-tags-file-location 'global-cache)
  (citre-edit-ctags-options-manually nil)
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  ;; citre makes imenu messy, i dont like it
  (citre-enable-imenu-integration nil)
  :config
  ;; gd will also triger citre-jump (with last priority, so its append)
  (setq evil-goto-definition-functions (append evil-goto-definition-functions
                                               '((lambda (symbol &rest _) (citre-jump)))))
  ;; HACK only enable citre-auto-enable-citre-mode when not on tramp
  (add-hook 'find-file-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (citre-auto-enable-citre-mode))))
  )

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)

  (defun +prog/flycheck-eldoc-function (callback &rest _)
    "Show flycheck messages at point via eldoc."
    (when-let* ((errs (flycheck-overlay-errors-at (point)))
                (msgs (mapconcat
                       (lambda (err)
                         (let ((level (flycheck-error-level err))
                               (msg (flycheck-error-message err)))
                           (propertize (format "%s: %s" level msg)
                                       'face (pcase level
                                               ('error 'error)
                                               ('warning 'warning)
                                               (_ 'success)))))
                       errs "\n")))
      (funcall callback msgs)))

  (add-hook 'flycheck-mode-hook
            (lambda ()
              (add-hook 'eldoc-documentation-functions #'+prog/flycheck-eldoc-function nil t)))

  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; flycheck has performace issues, make it less automate
  (flycheck-check-syntax-automatically '(save mode-enable idle-change))
  (flycheck-idle-change-delay 4))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :custom
  (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

(use-package consult-eglot
  :ensure t)

(use-package consult-eglot-embark
  :ensure t
  :after embark
  :config
  (consult-eglot-embark-mode 1))

(use-package eglot
  :ensure t
  :config
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider ; no highlight
                                            :semanticTokensProvider))
  (setq eglot-watch-files-outside-project-root nil)
  (setq eglot-confirm-server-initiated-edits nil)

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("uvx" "--from" "pyright" "pyright-langserver" "--stdio")))

  (add-to-list 'eglot-server-programs
               '(text-mode . ("harper-ls" "--stdio"))) ;; add harper-ls

  ;; default setup for harper-ls
  ;; see: https://writewithharper.com/docs/integrations/emacs
  (setq-default eglot-workspace-configuration
                '(:harper-ls
                  (:userDictPath ""
                                 :workspaceDictPath ""
                                 :fileDictPath ""
                                 :linters (:SpellCheck t
                                                       :SpelledNumbers :json-false
                                                       :AnA t
                                                       :SentenceCapitalization t
                                                       :UnclosedQuotes t
                                                       :WrongQuotes :json-false
                                                       :LongSentences t
                                                       :RepeatedWords t
                                                       :Spaces :json-false ;; no space!
                                                       :Matcher t
                                                       :CorrectNumberSuffix t)
                                 :codeActions (:ForceStable :json-false)
                                 :markdown (:IgnoreLinkTitle :json-false)
                                 :diagnosticSeverity "hint"
                                 :isolateEnglish :json-false
                                 :dialect "American"
                                 :maxFileLength 120000
                                 :ignoredLintsPath ""
                                 :excludePatterns [])))
  )

(use-package eldoc-box
  :ensure t
  :after eglot
  :if window-system ;; do not load eldoc-box on termial emacs
  :config
  (add-hook 'eldoc-box-buffer-setup-hook
            (lambda (_orig-buffer)
              (setq-local cursor-type nil
                          cursor-in-non-selected-windows nil)))
  (add-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode))

;; compilation mode
(setopt compilation-max-output-line-length nil) ; no max output, do not wrap
(setopt compilation-always-kill t)
(setopt ansi-color-for-compilation-mode t)
(setopt compilation-ask-about-save t)
(setopt compilation-scroll-output 'first-error)
;; NOTE always use current window for compilation
;; (add-to-list 'display-buffer-alist
;;              '("\\*compilation\\*"
;;                (display-buffer-reuse-window display-buffer-same-window)
;;                (reusable-frames . visible)
;;                (inhibit-switch-frames . nil)))

;; rust, see: https://github.com/brotzeit/rustic/blob/1f4d6a315824487c88b5e1f6c0ad8a984def2c3d/rustic-compile.el
(defvar +prog/rust-compilation-error
  (let ((err "^error[^:]*:[^\n]*\n\s*-->\s")
        (file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat err file ":" start-line ":" start-col)))
      (cons re '(1 2 3))))
  "Create hyperlink in compilation buffers for rust errors.")

(defvar +prog/rust-compilation-warning
  (let ((warning "^warning:[^\n]*\n\s*-->\s")
        (file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat warning file ":" start-line ":" start-col)))
      (cons re '(1 2 3 1)))) ;; 1 for warning
  "Create hyperlink in compilation buffers for rust warnings.")

(defvar +prog/rust-compilation-info
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *::: " file ":" start-line ":" start-col)))
      (cons re '(1 2 3 0)))) ;; 0 for info type
  "Create hyperlink in compilation buffers for file paths preceded by ':::'.")

(defvar +prog/rust-compilation-panic
  (let ((panic "thread '[^']+' panicked at '[^']+', ")
        (file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat panic file ":" start-line ":" start-col)))
      (cons re '(1 2 3))))
  "Match thread panics.")

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'rustic-error +prog/rust-compilation-error))
(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'rustic-warning +prog/rust-compilation-warning))
(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'rustic-info +prog/rust-compilation-info))
(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'rustic-panic +prog/rust-compilation-panic))

(add-to-list 'compilation-error-regexp-alist 'rustic-error)
(add-to-list 'compilation-error-regexp-alist 'rustic-warning)
(add-to-list 'compilation-error-regexp-alist 'rustic-info)
(add-to-list 'compilation-error-regexp-alist 'rustic-panic)

;; sibling files (for c/c++)
(add-to-list 'find-sibling-rules
             '("/\\([^/]+\\)\\.c\\(c\\|pp\\)?\\'" "\\1.h\\(h\\|pp\\)?\\'"))
(add-to-list 'find-sibling-rules
             '("/\\([^/]+\\)\\.h\\(h\\|pp\\)?\\'" "\\1.c\\(c\\|pp\\)?\\'"))

(setopt gdb-show-main t)

;; compile
(defun +prog/compile-with-no-preset ()
  (interactive)
  (let* ((compile-command (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) "")))
    (call-interactively 'compile)))

(defun +prog/compile-with-comint ()
  (interactive)
  (let* ((compile-command (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) ""))
         (current-prefix-arg '(4)))
    (call-interactively 'compile)))

;; Use full tree-sitter fontification by default.
(setopt treesit-font-lock-level 4)

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; -----------------------------------------------------------
;;; flycheck-google-cpplint
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
;;; apheleia - Deferred Loading
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
;;; tree-sitter - Optimized for Emacs 30+
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
  (defun +prog/treesit-auto-toggle ()
    "Toggle global-treesit-auto-mode."
    (interactive)
    (if global-treesit-auto-mode
        (progn
          (global-treesit-auto-mode -1)
          (message "global-treesit-auto-mode disabled"))
      (global-treesit-auto-mode 1)
      (message "global-treesit-auto-mode enabled"))))


(provide 'init-prog)
;;; prog.el ends here
