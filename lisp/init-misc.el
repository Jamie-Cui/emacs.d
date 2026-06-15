;;; init-misc.el --- native emacs tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; fix frame display issues
(setq frame-resize-pixelwise t)

;; smooth scrolling
(pixel-scroll-mode +1)

;; Optional configurations for fine-tuning
(setq pixel-dead-time 0) ; Never revert to line-based scrolling behavior
(setq pixel-resolution-fine-flag t) ; Scroll by actual pixels
(setq mouse-wheel-scroll-amount '(1)) ; Distance to scroll per mouse wheel event
(setq mouse-wheel-progressive-speed nil) ; Disable progressive speed if it feels too fast

;; (setq tramp-verbose 6)
;; Optional: display debug info in a separate buffer
;; (setq tramp-debug-buffer t)

;; Delete trailing before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Supress warnings about file
(setopt warning-suppress-log-types '((files)))

(setq byte-compile-warnings '(not lexical))

;; make minimal margin
(set-fringe-mode 0)

;; stop the BELL!
(setq ring-bell-function 'ignore)

;; Emacs 30 and newer: Disable Ispell completion function.
;; Try `cape-dict' as an alternative.
(setopt text-mode-ispell-word-completion nil)

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
(setopt auto-save-default nil ;; HACK DO NOT auto-save
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

;; Silently report native-comp warnings/errors (we handle them in init.el)
;; This prevents spurious warnings when native compilation is disabled
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

(dolist (mode '(pdf-view-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                imenu-list-minor-mode-hook
                imenu-list-major-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))
(setq-default display-line-numbers-type 'visual)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq tab-always-indent t) ;  TAB just indents the current line

;; allow use minibuffer inside minibuffer
(setq enable-recursive-minibuffers t)

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
(defvar +compile/rust-compilation-error
  (let ((err "^error[^:]*:[^\n]*\n\s*-->\s")
        (file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat err file ":" start-line ":" start-col)))
      (cons re '(1 2 3))))
  "Create hyperlink in compilation buffers for rust errors.")

(defvar +compile/rust-compilation-warning
  (let ((warning "^warning:[^\n]*\n\s*-->\s")
        (file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat warning file ":" start-line ":" start-col)))
      (cons re '(1 2 3 1)))) ;; 1 for warning
  "Create hyperlink in compilation buffers for rust warnings.")

(defvar +compile/rust-compilation-info
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *::: " file ":" start-line ":" start-col)))
      (cons re '(1 2 3 0)))) ;; 0 for info type
  "Create hyperlink in compilation buffers for file paths preceded by ':::'.")

(defvar +compile/rust-compilation-panic
  (let ((panic "thread '[^']+' panicked at '[^']+', ")
        (file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat panic file ":" start-line ":" start-col)))
      (cons re '(1 2 3))))
  "Match thread panics.")

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'rustic-error +compile/rust-compilation-error))
(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'rustic-warning +compile/rust-compilation-warning))
(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'rustic-info +compile/rust-compilation-info))
(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'rustic-panic +compile/rust-compilation-panic))

(add-to-list 'compilation-error-regexp-alist 'rustic-error)
(add-to-list 'compilation-error-regexp-alist 'rustic-warning)
(add-to-list 'compilation-error-regexp-alist 'rustic-info)
(add-to-list 'compilation-error-regexp-alist 'rustic-panic)

;;; proced, show processes
(setopt proced-auto-update-flag t)
(setopt proced-goal-attribute nil)
(setopt proced-show-remote-processes t)
(setopt proced-enable-color-flag t)
(setopt proced-format 'short)
(setopt proced-auto-update-interval 1)
(add-hook 'proced-post-display-hook
          (lambda ()
            (setq-local truncate-lines t)))

;; emacs minibufer completion
(setopt minibuffer-completion-auto-choose nil)

(require 'init-config-tramp)

(setq +eshell/proxy +emacs/proxy)
(require 'init-config-eshell)

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

;; turn on size in bytes indicator on modeline
;; (size-indication-mode 1)

(defun +modeline/project-buffer-name ()
  "Return the current buffer name relative to the project root."
  (if-let* ((file buffer-file-name)
            (project (project-current nil (file-name-directory file)))
            (root (project-root project))
            (name (or (project-name project)
                      (file-name-nondirectory
                       (directory-file-name root))))
            (relative (file-relative-name file root)))
      (concat name "/" relative)
    (buffer-name)))

(defun +modeline/buffer-identification ()
  "Return the propertized current buffer identifier for the mode line."
  (propertize (+modeline/project-buffer-name)
              'face 'mode-line-buffer-id
              'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
              'mouse-face 'mode-line-highlight
              'local-map mode-line-buffer-identification-keymap))

;; mode-line format
(setopt mode-line-format
        (list
         "%e"
         'mode-line-front-space
         '(:eval (+modeline/buffer-identification))
         "   "
         'mode-line-position
         "   "
         '(:eval (if (region-active-p)
                     (format " (Sel: %d)" (abs (- (point) (mark)))) ""))
         'mode-line-format-right-align
         'mode-line-misc-info
         "   "
         '(:eval (format "%.1fk" (/ (count-lines (point-min) (point-max)) 1000.0)))
         "   "
         'mode-name
         " (+"
         '(:eval (format "%d" (length local-minor-modes)))
         ")"
         'mode-line-end-spaces
         "   "
         )
        )

;; debug
(setopt gdb-show-main t)

;; save all buffers

(defun +save-all-buffers ()
  (interactive)
  (let* ((current-prefix-arg '(4)))
    (call-interactively 'save-some-buffers)))

;; compile
(defun +compile-with-no-preset ()
  (interactive)
  (let* ((compile-command (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) "")))
    (call-interactively 'compile)))

(defun +compile-with-comint ()
  (interactive)
  (let* ((compile-command (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) ""))
         (current-prefix-arg '(4)))
    (call-interactively 'compile)))

(defun +copy-ref-dwim ()
  "Copy the current file location and text at point to the kill ring.

When a region is active, copy the file with the selected line range.
Otherwise, copy the current line."
  (interactive)
  (let* ((file (or buffer-file-name
                   (user-error "Buffer is not associated with a file")))
         (has-region (use-region-p))
         (beg (if has-region (region-beginning) (line-beginning-position)))
         (raw-end (if has-region (region-end) (line-end-position)))
         (end (if (> raw-end beg) (1- raw-end) raw-end))
         (start-line (line-number-at-pos beg t))
         (end-line (line-number-at-pos end t))
         (line-range
          (format "%s:%d-%d" file start-line end-line))
         (content
          (if (= start-line end-line)
              (let ((content-end (if (and (> raw-end beg)
                                          (eq (char-before raw-end) ?\n))
                                     (1- raw-end)
                                   raw-end)))
                (buffer-substring-no-properties beg content-end))
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- start-line))
              (let ((line start-line)
                    lines)
                (while (<= line end-line)
                  (push (format "   %d: %s"
                                line
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                        lines)
                  (forward-line 1)
                  (setq line (1+ line)))
                (mapconcat #'identity (nreverse lines) "\n")))))
         (text (format "%s\n\n%s" line-range content)))
    (kill-new text)
    (setq deactivate-mark t)
    (message "Yanked: %s" line-range)))

;; copy buffer file name
(defun +copy-buffer-file-name ()
  "Copy the current buffer file name to clipboard."
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message "Copied: %s" name)))

(defun +wc/non-ascii (&optional start end)
  "Count lines, non-ASCII characters, and characters in region or buffer."
  (interactive)
  (let ((start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char start)
        (message "lines: %3d non ascii words: %3d chars: %3d"
                 (count-lines start end)
                 (count-matches "[^[:ascii:]]")
                 (- end start))))))

(setopt duplicate-line-final-position 1) ; move point to the first newline

;; Use full tree-sitter fontification by default.
(setopt treesit-font-lock-level 4)

;; enable narrow-to-region function without asking
;; see: https://www.gnu.org/software/emacs/manual/html_node/elisp/Disabling-Commands.html
(put 'narrow-to-region 'disabled nil)

(defun +emacs--compiled-artifact-roots ()
  "Return directories searched by `+emacs/clear-compiled-artifacts'."
  (let* ((roots (list user-emacs-directory
                      (and (boundp '+emacs/repo-directory)
                           +emacs/repo-directory)))
         (normalized
          (delq nil
                (mapcar (lambda (dir)
                          (when (and (stringp dir)
                                     (file-directory-p dir))
                            (file-name-as-directory
                             (file-truename
                              (directory-file-name dir)))))
                        roots))))
    (delete-dups normalized)))

(defun +emacs--delete-compiled-artifacts-in-directory (dir)
  "Delete .elc and .eln files below DIR.
Return a cons cell (DELETED . FAILED), where FAILED is an alist of
file names and error data."
  (let ((deleted 0)
        failed)
    (dolist (file (directory-files-recursively dir "\\.el[cn]\\'"))
      (condition-case err
          (progn
            (delete-file file)
            (setq deleted (1+ deleted)))
        (error
         (push (cons file err) failed))))
    (cons deleted failed)))

(defun +emacs/clear-compiled-artifacts (&optional no-restart)
  "Delete Emacs .elc and .eln artifacts from config and package caches.

With prefix argument NO-RESTART, do not offer to restart Emacs after cleanup."
  (interactive "P")
  (let ((roots (+emacs--compiled-artifact-roots))
        (deleted 0)
        failed)
    (unless roots
      (user-error "No compiled artifact directories found"))
    (when (yes-or-no-p
           (format "Delete .elc/.eln files under %d directories? "
                   (length roots)))
      (dolist (dir roots)
        (let ((result (+emacs--delete-compiled-artifacts-in-directory dir)))
          (setq deleted (+ deleted (car result)))
          (setq failed (append (cdr result) failed))))
      (message "Deleted %d compiled artifacts%s"
               deleted
               (if failed
                   (format "; %d failed, see *Messages*" (length failed))
                 ""))
      (dolist (failure failed)
        (message "Failed to delete %s: %S" (car failure) (cdr failure)))
      (when (and (not no-restart)
                 (yes-or-no-p "Restart Emacs now? "))
        (restart-emacs)))))

(defun +emacs/clear-native-compile-cache ()
  "Delete Emacs compiled artifacts and offer to restart Emacs.
This command is kept as a compatibility alias for the broader cleanup command."
  (interactive)
  (+emacs/clear-compiled-artifacts))

;;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'init-misc)
