;;; init-misc.el --- native emacs tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Supress warnings about file
(setopt warning-suppress-log-types '((files)))

;; Set Lexical Binding Globally
(setq lexical-binding t)
(setq byte-compile-warnings '(not lexical))

;; make minimal margin
(set-fringe-mode 0)

;; stop the BELL!
(setq ring-bell-function 'ignore)

;; Emacs 30 and newer: Disable Ispell completion function.
;; Try `cape-dict' as an alternative.
(setopt text-mode-ispell-word-completion nil)

;; stop makding ~ files!
(setq make-backup-files nil) 

;; stop makding #...# files!
(setq create-lockfiles nil)

;; if use backup, put it in .emacs.d/backup
(setopt backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/")))
        tramp-backup-directory-alist backup-directory-alist)

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setopt auto-save-default t
        auto-save-include-big-deletions t
        ;; Keep it out of `doom-emacs-dir' or the local directory.
        auto-save-list-file-prefix (concat user-emacs-directory "autosave/")
        tramp-auto-save-directory  (concat user-emacs-directory "tramp-autosave/")
        auto-save-file-name-transforms
        (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                    ;; Prefix tramp autosaves to prevent conflicts with local ones
                    (concat auto-save-list-file-prefix "tramp-\\2") t)
              (list ".*" auto-save-list-file-prefix t)))

;; disable electric-indent-mode, forever
(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(internal-border-width . 5))

;; scrolling
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10000
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like diff-hl and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      tooltip-resize-echo-area t)

;; do not show me native-comp warning and erros
(setopt native-comp-async-report-warnings-errors 'silent)

;; disable certain things
(menu-bar-mode 0)
(tool-bar-mode 0)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

;; word wrap
(global-visual-line-mode 1)
(setq word-wrap-by-category t)

;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(dolist (mode '(pdf-view-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                imenu-list-minor-mode-hook
                imenu-list-major-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))
(setq-default display-line-numbers-type 't)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq tab-always-indent t) ;  TAB just indents the current line

;; dont allow use minibuffer inside minibuffer
(setq enable-recursive-minibuffers nil)

;; use short answers
(setq use-short-answers t)

;; column
(setopt fill-column 80)

;; Remove RCS, CVS, SCCS, SRC, and Bzr, because it's a lot less work for vc to
;; check them all (especially in TRAMP buffers), and who uses any of these in
;; 2021, amirite? 
;; NOTE I dont use svn and hg
(setq-default vc-handled-backends '(Git))

;; enable hideshow in all programming modes
(require 'hideshow)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Persist history over Emacs restarts.
(require 'savehist)
(savehist-mode 1)

;; commit mode
(setopt comint-scroll-to-bottom-on-output t)
(setopt ansi-color-for-comint-mode t)
(setopt comint-prompt-read-only t)
(setopt comint-buffer-maximum-size 2048)

;; compilation mode
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

;;; proced, show processes
(setopt proced-auto-update-flag t)
(setopt proced-goal-attribute nil)
(setopt proced-show-remote-processes t)
(setopt proced-enable-color-flag t)
(setopt proced-format 'short)
(setopt proced-auto-update-interval 1)

;; emacs minibufer completion
(setopt minibuffer-completion-auto-choose nil)

;; ------------------------------------------------------------------
;; Remote Configuration
;; ------------------------------------------------------------------

;; allow to use .dir_locals on remote files
(setopt enable-remote-dir-locals t)

;; compress warning at start-up
;; (setopt warning-minimum-level :emergency)

;; Imporove tramp speed
;; see: https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setopt tramp-allow-unsafe-temporary-files t ; do not warn me, please
        remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(setopt tramp-auto-save-directory (concat user-emacs-directory "tramp-autosave/"))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;; forgot why I add this ...
(setopt tramp-pipe-stty-settings "")

;; this improves magit efficiency
(unless (featurep :system 'windows)
  (setopt tramp-default-method "ssh")) ; faster than the default scp

;; allow asyn in tramp
(setopt tramp-async-enabled t)

;; ------------------------------------------------------------------
;; Eshell Configuration
;; ------------------------------------------------------------------

(setopt eshell-scroll-show-maximum-output nil
        eshell-highlight-prompt nil
        eshell-destroy-buffer-when-process-dies t)

(setopt eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input)))
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

(setopt eshell-prompt-function 
        (lambda nil
          (let* ((cwd (abbreviate-file-name (eshell/pwd))))
            (concat (propertize
                     ;; the above line
                     (format "%s [%s]"
                             (propertize (concat (user-login-name) "@" (system-name)) 
                                         'font-lock-face 'font-lock-comment-face) 
                             (propertize cwd 'font-lock-face 'font-lock-constant-face)
                             )
                     'read-only t
                     'front-sticky   '(font-lock-face read-only)
                     'rear-nonsticky '(font-lock-face read-only))
                    ;; input line
                    " $ "
                    ))))

(setopt eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face)))

;; always get a new eshell
(defun +eshell/new ()
  (interactive)
  (let ((current-prefix-arg ""))
    (call-interactively 'eshell)))

;; (defun +eshell/project-new ()
;;   (interactive)
;;   (let ((current-prefix-arg (1+ (cl-position (persp-current-name) (persp-names) :test 'equal))))
;;     (call-interactively 'eshell)))

(defun +eshell/set-proxy (proxy)
  "Set proxy environment variables and git proxy configuration."
  ;; Set environment variables
  (setenv "http_proxy" proxy)
  (setenv "https_proxy" proxy)
  (setenv "ftp_proxy" proxy)
  (setenv "HTTP_PROXY" proxy)
  (setenv "HTTPS_PROXY" proxy)
  (setenv "FTP_PROXY" proxy)
  
  ;; Set git proxy configuration
  (if proxy
      (progn (shell-command (format "git config --global http.proxy %s" proxy))
             (shell-command (format "git config --global https.proxy %s" proxy)))
    (progn (shell-command (format "git config --global --unset http.proxy"))
           (shell-command (format "git config --global --unset https.proxy")))))

(defun eshell/set-proxy ()
  "Set proxy environment variables and git proxy configuration."
  (interactive)
  (let ((my-proxy +emacs/proxy)) ; HACK this requires MY_PROXY to be set in system-wide
    (when my-proxy
      ;; Set proxy
      (+eshell/set-proxy my-proxy)
      ;; Show proxy settings
      (eshell/show-proxy))))

(defun eshell/unset-proxy ()
  "Set proxy environment variables and git proxy configuration."
  (interactive)
  (+eshell/set-proxy nil)
  ;; Show proxy settings
  (eshell/show-proxy))

(defun +eshell/format-shell-command (command)
  (let* ((str 
          (replace-regexp-in-string "\n$" "" 
                                    (shell-command-to-string command))))
    (if (string-empty-p str) nil str)))

(defun eshell/show-proxy ()
  "Display current proxy settings."
  (interactive)
  (eshell-printn (format "[env] http_proxy  : %s" (getenv "http_proxy")))
  (eshell-printn (format "[env] https_proxy : %s" (getenv "https_proxy")))
  (eshell-printn (format "[env] ftp_proxy   : %s" (getenv "ftp_proxy")))
  (eshell-printn (format "[git] http_proxy  : %s" (+eshell/format-shell-command "git config --global --get http.proxy")))
  (eshell-printn (format "[git] https_proxy : %s" (+eshell/format-shell-command "git config --global --get https.proxy"))))

;; HACK redefine eshell/clear function using advice
(defadvice eshell/clear (around clear-buffer activate)
  "Clear the eshell buffer by erasing its contents."
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; ------------------------------------------------------------------
;; TTY Configuration
;; ------------------------------------------------------------------

(when (not (display-graphic-p))
  ;; HACK for state-of-art emacs (30.1.05 maybe?)
  (when (featurep 'tty-child-frames)
    (add-hook 'tty-setup-hook #'tty-tip-mode))

  ;; Use OSC52 protocol, which is used by alacritty by default
  (defun +tty/copy-to-system-clipboard (text &optional push)
    (let ((encoded (base64-encode-string 
                    (encode-coding-string text 'binary) 
                    t)))
      (send-string-to-terminal (format "\e]52;c;%s\a" encoded))))

  (setq interprogram-cut-function '+tty/copy-to-system-clipboard)


  (xterm-mouse-mode 1)
  (setq x-stretch-cursor t)
  (setq frame-resize-pixelwise t)

  ;; Disable eldoc in terminal
  (eldoc-mode -1))

;; ------------------------------------------------------------------
;; Dev Configuration
;; ------------------------------------------------------------------

;; sibling files (for c/c++)
(add-to-list 'find-sibling-rules
             '("/\\([^/]+\\)\\.c\\(c\\|pp\\)?\\'" "\\1.h\\(h\\|pp\\)?\\'"))
(add-to-list 'find-sibling-rules
             '("/\\([^/]+\\)\\.h\\(h\\|pp\\)?\\'" "\\1.c\\(c\\|pp\\)?\\'"))

;; ------------------------------------------------------------------
;;; Modeline 
;; ------------------------------------------------------------------

;; evil
(setopt evil-mode-line-format '(before . mode-line-front-space))

;; turn on column-number on modeline
(column-number-mode 1)
;; (setopt mode-line-position-column-line-format '("%l:%c"))

;; remove trailing dashes
(setopt mode-line-end-spaces nil)
(setopt mode-line-front-space nil)

;; mode-line format
(setopt mode-line-format
        (list
         "%e" 
         'mode-line-front-space
         '(:eval (format-mode-line 
                  (propertized-buffer-identification 
                   (or (when-let* ((buffer-file-truename buffer-file-truename)
                                   (project (cdr-safe (project-current)))
                                   (project-parent (file-name-directory (directory-file-name (expand-file-name project)))))
                         (concat (file-relative-name (file-name-directory buffer-file-truename) project-parent)
                                 (file-name-nondirectory buffer-file-truename)))
                       "%b"))))
         "   "
         'mode-line-position
         "   "
         '(:eval (if (region-active-p)
                     (format " (Sel: %d)" (abs (- (point) (mark)))) ""))
         'mode-line-format-right-align
         'mode-line-misc-info
         " "
         '(:eval
           (propertize
            (if (tramp-tramp-file-p buffer-file-name)
                (format "[%s:%s]" 
                        (tramp-file-name-method (tramp-dissect-file-name buffer-file-name))
                        (persp-current-name))
              (format "[%s]" (persp-current-name)))
            'face 'font-lock-keyword-face))
         " "
         'mode-name
         " (+"
         '(:eval (format "%d" (length local-minor-modes)))
         ")"
         'mode-line-end-spaces
         " "
         )
        )

;; debug
(setopt gdb-show-main t)

;; compile
(defun +compile-with-no-preset ()
  (interactive)
  (let* ((compile-command (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) "")))
    (call-interactively 'compile)))

;; copy buffer file name
(defun +copy-buffer-file-name ()
  "Copy the current buffer file name to clipboard."
  (interactive)
  (kill-new (buffer-file-name))
  (message "Copied: %s" (buffer-file-name)))

(setopt duplicate-line-final-position 1) ; move point to the first newline

;; treesit font level set to 2
(setopt treesit-font-lock-level 2)

(provide 'init-misc)
