;;; init-dired.el --- dired support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-funs)

(+ensure-packages-installed
 '(
   ;; better dired
   ;; dirvish
   dired-subtree
   diredfl ;; color
   ))

;; dired hide .. and .
(add-hook 'dired-mode-hook 'dired-omit-mode)

(use-package dired
  :custom
  (dired-listing-switches (purecopy "-alh --human-readable --group-directories-first --no-group"))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-omit-extensions nil)
  (dired-dwim-target t)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "h"   #'dired-up-directory
   "l"   #'dired-find-file)
  )

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "TAB" #'dired-subtree-toggle)
  )

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode)
  )


;; (use-package dirvish
;;   :ensure t
;;   :init
;;   (dirvish-override-dired-mode)
;;   :after (:and general nerd-icons)
;;   :custom
;;   (dired-listing-switches (purecopy "-alh --human-readable --group-directories-first --no-group"))
;;   (dired-kill-when-opening-new-dired-buffer t)
;;   (dirvish-subtree-state-style 'plus)
;;   :config
;;   ;; HACK for macos, see: https://github.com/d12frosted/homebrew-emacs-plus/issues/383#issuecomment-899157143
;;   (when (eq system-type 'darwin)
;;     (setq insert-directory-program "gls" dired-use-ls-dired t))
;;   (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
;;   (setq dirvish-attributes '(vc-state subtree-state file-time file-size))
;;   (general-define-key
;;    :states 'normal
;;    :keymaps 'dirvish-mode-map
;;    "TAB" #'dirvish-subtree-toggle
;;    "h"   #'dired-up-directory
;;    "l"   #'dired-find-file)
;;   )

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
