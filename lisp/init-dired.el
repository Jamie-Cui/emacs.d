;;; init-dired.el --- dired support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)

(+ensure-packages-installed
 '(
   ;; better dired
   dirvish
   ))

;; dired hide .. and .
(add-hook 'dired-mode-hook 'dired-omit-mode)

(use-package dired-x
  :custom
  (dired-dwim-target t)
  (dired-omit-extensions nil)
  )

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :after (:and general nerd-icons)
  :custom
  (dired-listing-switches (purecopy "-alh --human-readable --group-directories-first --no-group"))
  (dired-kill-when-opening-new-dired-buffer t)
  (dirvish-subtree-state-style 'plus)
  :config
  ;; HACK for macos, see: https://github.com/d12frosted/homebrew-emacs-plus/issues/383#issuecomment-899157143
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls" dired-use-ls-dired t))
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  (general-define-key
   :states 'normal
   :keymaps 'dirvish-mode-map
   "TAB" #'dirvish-subtree-toggle
   "h"   #'dired-up-directory
   "l"   #'dired-find-file)
  )

(use-package tramp
  :config
  ;; Enable full-featured Dirvish over TRAMP on ssh connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  ;; Tips to speed up connections
  (setq tramp-verbose 0)
  (setq tramp-chunksize 2000)
  (setq tramp-ssh-controlmaster-options nil))

(provide 'init-dired)
