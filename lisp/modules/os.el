;;; os.el --- operating-system specific tweaks -*- lexical-binding: t -*-
;;; Commentary:
;; Operating-system specific tweaks (Linux/macOS/Windows/WSL) and shell
;; environment import.
;;; Code:


(unless (eq system-type 'windows-nt)
  (use-package exec-path-from-shell
    :ensure t
    :custom
    (exec-path-from-shell-variables '("PATH" "MANPATH" "SSS_API_KEY"))
    :config
    (exec-path-from-shell-initialize)))

(use-package rime
  :ensure t
  :if (eq system-type 'gnu/linux)
  :custom
  (default-input-method "rime")
  (rime-user-data-dir "~/opt/dotfiles/rime")
  (rime-show-candidate 'posframe))

;; Stop respecting the system input method (fcitx) on PGTK.
;;
;; A PGTK build (Linux Wayland/X11) forwards keys to GtkIMContext, which hands
;; them to the system IME.  That bypasses Emacs's own `current-input-method' /
;; `evil-input-method', so packages that toggle the input method (rime above,
;; magent-evil's post-submit reset) cannot take effect.  Disabling GtkIMContext
;; lets Emacs own input again, driven by `rime'.
;;
;; PGTK-only: `pgtk-use-im-context' does not exist on the macOS (ns) build or in
;; the terminal, so guard on both the frame type and the function.  The macOS
;; equivalent, if ever needed, goes in the Darwin section below.
(when (and (eq system-type 'gnu/linux)
           (eq window-system 'pgtk)
           (fboundp 'pgtk-use-im-context))
  (pgtk-use-im-context nil))

;;; --------------------------------------
;;; Darwin (MacOs)
;;; --------------------------------------

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  ;; (add-to-list 'default-frame-alist '(undecorated . t))
  ;; Fix, macos dired permission
  (setq insert-directory-program "gls" dired-use-ls-dired t)

  ;; --- Input method (placeholder) ---------------------------------------
  ;; macOS uses the NS (Cocoa) build, which talks to the system IME through
  ;; NSTextInputClient.  There is NO `pgtk-use-im-context' equivalent here and
  ;; no official switch to make Emacs stop respecting the system input method.
  ;; If you want Emacs-owned input on macOS, the practical route is:
  ;;   1. Install rime for macOS and set `default-input-method' to "rime"
  ;;      (rime has a macOS backend; the LADDER config differs from Linux).
  ;;   2. Keep the system IME in ASCII/English while Emacs is focused.  On the
  ;;      `emacs-mac' (Yamamoto) build, `mac-auto-ascii-mode' forces the system
  ;;      IME back to Roman input; the stock `ns' build has no such helper.
  ;; Left unimplemented on purpose — fill in once the macOS box is set up.
  ;; (when (fboundp 'mac-auto-ascii-mode) (mac-auto-ascii-mode 1))

  ;; site-lisp
  (use-package ultra-scroll
    :ensure t
    :init
    (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
          scroll-margin 0)        ; important: scroll-margin>0 not yet supported
    :config
    (ultra-scroll-mode 1)))

;;; --------------------------------------
;;; Windows-NT
;;; --------------------------------------

(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plink")
  (setq tramp-use-connection-share t)
  ;; (setq inhibit-eol-conversion t)

  (setq buffer-file-coding-system 'utf-8-unix)
  (setq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)

  ;; HACK see: https://github.com/magit/magit/issues/2219#issuecomment-157219646
  (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
    "Mode for showing staged and unstaged changes."
    :group 'magit-status)

  (defun magit-staging-refresh-buffer ()
    (magit-insert-section (status)
      (magit-insert-unstaged-changes)
      (magit-insert-staged-changes)))

  (defun magit-staging ()
    (interactive)
    (magit-mode-setup #'magit-staging-mode))

  (defun w32explore (file)
    "Open Windows Explorer to FILE (a file or a folder)."
    (interactive "fFile: ")
    (let ((w32file (subst-char-in-string ?/ ?\\ (expand-file-name file))))
      (if (file-directory-p w32file)
          (w32-shell-execute "explore" w32file "/e,/select,")
        (w32-shell-execute "open" "explorer" (concat "/e,/select," w32file)))))

  (require 'eshell)
  (defun +eshell/fix-crlf (output)
    "Remove CRLF from Eshell output."
    (replace-regexp-in-string "\r\n" "\n" output))
  (add-to-list 'eshell-preoutput-filter-functions '+eshell/fix-crlf))

;;; --------------------------------------
;;; Windows-WSL
;;; --------------------------------------

(when (getenv "WSLENV")
  ;; from: https://gist.github.com/minorugh/1770a6aa93df5fe55f70b4d72091ff76
  ;; Emacs on WSL open links in Windows web browser
  ;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic)))

  ;; org-download from windows clipboard
  (use-package org-download
    :ensure t
    :custom
    (org-download-screenshot-method
     "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")))

;;; --------------------------------------
;;; Utilities work for all systems
;;; --------------------------------------

(defun +os/explorer-dwim ()
  "Open Windows Explorer to current file or folder.w"
  (interactive)
  (require 'dired)
  (let ((target-file (if (eq major-mode 'dired-mode)
                         (dired-get-filename nil t)
                       (buffer-file-name))))
    (message "%s" target-file)
    (cond
     ((eq system-type 'windows-nt)
      (w32explore target-file))
     ((eq system-type 'gnu/linux)
      (start-process "xdg-open" nil "xdg-open"
                     (if (file-directory-p target-file)
                         target-file
                       (file-name-directory target-file))))
     ((eq system-type 'darwin)
      (start-process "finder" nil "open" "-R" target-file))
     (t (message "Unimplemented!")))))


(provide 'init-os)
;;; os.el ends here
