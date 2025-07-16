;;; init-os.el --- different os tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)

;;; --------------------------------------
;;; WLS2
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
  
  ;; declare new functions
  (defun cp-current-file-to-windows()
    "Copy the current file to windows"
    (interactive)
    (let ((dest-path (concat "~/Desktop/tmp/" (format-time-string "%Y-%m-%d") "/")))
      (when buffer-file-name
        (make-directory dest-path 'parents)
        (message (concat "cp -r " buffer-file-name " " dest-path))
        (shell-command (concat "cp -r " buffer-file-name " " dest-path)))))
  
  ;; org-download from windows clipboard
  (use-package org-download
    :ensure t
    :custom
    (org-download-screenshot-method
     "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\""))
  )

;;; --------------------------------------
;;; Darwin (MacOs)
;;; --------------------------------------

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (add-to-list 'default-frame-alist '(undecorated . t))
  ;; Fix, macos dired permission
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  )

;;; --------------------------------------
;;; Windows
;;; --------------------------------------

(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plink")
  (setq tramp-use-connection-share t)
  (setq inhibit-eol-conversion t)
  (prefer-coding-system 'utf-8)
  (setq buffer-file-coding-system 'utf-8-unix)
  (set-terminal-coding-system 'utf-8)
  ;;(set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  )

(provide 'init-os)
