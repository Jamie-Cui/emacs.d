;;; init-misc.el --- native emacs tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)

;; stop the BELL!
(setq ring-bell-function 'ignore)

;; stop makding ~ files!
(setq make-backup-files nil) 

;; disable electric-indent-mode, forever
(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; scrolling
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered. The default (0) triggers this too
      ;; aggressively, so I've set it to 10 to recenter if scrolling too far
      ;; off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
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

;; enable hideshow in all programming modes
(use-package hideshow
  :config
  (add-hook 'prog-mode-hook #'hs-minor-mode))

;; Persist history over Emacs restarts.
(use-package savehist
  :init
  (savehist-mode))

;; commit mode scroll to bottom
(setopt comint-scroll-to-bottom-on-output t)

;; allow to use .dir_locals on remote files
(setopt enable-remote-dir-locals t)

;; compress warning at start-up
;; (setopt warning-minimum-level :emergency)

;; Imporove tramp speed
;; see: https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq tramp-allow-unsafe-temporary-files t ; do not warn me, please
      remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;; forgot why I add this ...
(setq magit-tramp-pipe-stty-settings 'pty)

;; don't show the diff by default in the commit buffer. Use `C-c C-d' to display it
(setq magit-commit-show-diff nil)
;; don't show git variables in magit branch
(setq magit-branch-direct-configure nil)
;; don't automatically refresh the status buffer after running a git command
(setq magit-refresh-status-buffer nil)

;;; eshell
(setopt eshell-scroll-show-maximum-output nil)

;; fancy eshell 
;; see: https://lambdaland.org/posts/2024-08-19_fancy_eshell_prompt/
(setopt eshell-prompt-function '+eshell/fancy-shell)
(setopt eshell-prompt-regexp "^[^#$\n]* [$#] ")
(setopt eshell-highlight-prompt nil)

(defun +eshell/fancy-shell ()
  "A pretty shell with git status"
  (let* ((cwd (abbreviate-file-name (eshell/pwd)))
         (x-stat eshell-last-command-status))
    (propertize
     (format "%s %s $ "
             (if (< 0 x-stat) (format (propertize "!%s" 'font-lock-face '(:foreground "red")) x-stat)
               (propertize "➤" 'font-lock-face (list :foreground (if (< 0 x-stat) "red" "green"))))
             (propertize cwd 'font-lock-face '(:foreground "#45babf")))
     ;; 'read-only t
     'front-sticky   '(font-lock-face read-only)
     'rear-nonsticky '(font-lock-face read-only))))

(use-package proced
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  (proced-auto-update-interval 1)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))

(provide 'init-misc)
