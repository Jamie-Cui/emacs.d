;;; init-misc.el --- native emacs tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set Lexical Binding Globally
(setq lexical-binding t)
(setq byte-compile-warnings '(not lexical))

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
        ;; Don't auto-disable auto-save after deleting big chunks. This defeats
        ;; the purpose of a failsafe. This adds the risk of losing the data we
        ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
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
(setq ring-bell-function 'ignore)

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
(use-package hideshow
  :config
  (add-hook 'prog-mode-hook #'hs-minor-mode))

;; Persist history over Emacs restarts.
(use-package savehist
  :init
  (savehist-mode))

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

;;; proced, show processes
(setopt proced-auto-update-flag t)
(setopt proced-goal-attribute nil)
(setopt proced-show-remote-processes t)
(setopt proced-enable-color-flag t)
(setopt proced-format 'custom)
(setopt proced-auto-update-interval 1)
(setopt proced-format-alist
        '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm)))

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
          (let* ((cwd (abbreviate-file-name (eshell/pwd)))
                 (x-stat eshell-last-command-status))
            (propertize
             (format "%s %s $ "
                     (if (< 0 x-stat)
                         (format (propertize "!%s" 'font-lock-face 
                                             '(:foreground "red")) x-stat)
                       (propertize "➤" 'font-lock-face 
                                   (list :foreground (if (< 0 x-stat) "red" "green"))))
                     (propertize cwd 'font-lock-face '(:foreground "#45babf")))
             'front-sticky   '(font-lock-face read-only)
             'rear-nonsticky '(font-lock-face read-only)))))

(setopt eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face)))

;; HACK always get a new eshell
(defun +eshell/new ()
  (interactive)
  (eshell '(t)))

;; HACK redefine eshell/clear function
(use-package esh-mode
  :config
  ;; HACK redefine eshell/clear function
  (defun eshell/clear (&optional scrollback)
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))))


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
;; (column-number-mode 1)
;; (setopt mode-line-position-column-line-format '("%l:%c"))

;; remove trailing dashes
(setopt mode-line-end-spaces nil)
(setopt mode-line-front-space nil)

;; mode-line format
(setopt mode-line-format
        (list
         ;; ?
         "%e" 
         ;;
         'mode-line-front-space
         ;; 
         '(:eval (format "%s" (count-lines (point-min) (point-max))))
         ;;
         ;; 'mode-line-frame-identification
         ;;
         ;; 'mode-line-buffer-identification
         ;;
         ;; HACK
         ;;
         " "
         '(:propertize 
           (:eval (concat (projectile-project-name) "/" (file-relative-name buffer-file-name (projectile-project-root))))
           face mode-line-buffer-id)
         ;;
         ;;
         " "
         ;;
         'mode-line-position
         ;;
         ;; REVIEW do not display version info
         ;; '(vc-mode vc-mode)
         ;;
         ;; right align start
         'mode-line-format-right-align
         ;;
         'mode-line-misc-info
         ;;
         " "
         ;; mode-line-modes 
         'mode-name
         ;;
         '(:propertize
           ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
            mode-line-window-dedicated)
           display (min-width (6.0)))
         ;;
         'mode-line-end-spaces
         ;;
         " "
         )
        )

(provide 'init-misc)
