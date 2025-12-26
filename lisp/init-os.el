;;; init-os.el --- different os tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; --------------------------------------
;;; Darwin (MacOs)
;;; --------------------------------------

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  ;; (add-to-list 'default-frame-alist '(undecorated . t))
  ;; Fix, macos dired permission
  (setq insert-directory-program "gls" dired-use-ls-dired t)

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
  ;; (prefer-coding-system 'utf-8)
  ;; (setq buffer-file-coding-system 'utf-8-unix)
  ;; (set-terminal-coding-system 'utf-8)
  ;; (set-keyboard-coding-system 'utf-8)
  ;; (set-language-environment "UTF-8")

  ;; WORKAROUND https://github.com/magit/magit/issues/2395
  (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
    "Mode for showing staged and unstaged changes."
    :group 'magit-status)
  (defun magit-staging-refresh-buffer ()
    (magit-insert-section (status)
      (magit-insert-untracked-files)
      (magit-insert-unstaged-changes)
      (magit-insert-staged-changes)))
  (defun magit-staging ()
    (interactive)
    (magit-mode-setup #'magit-staging-mode))
  
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

(defun +os-explorer/dwim ()
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
