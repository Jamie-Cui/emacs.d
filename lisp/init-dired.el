;;; init-dired.el --- dired support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)

(+ensure-packages-installed
 '(
   ;; better dired
   ;; dirvish
   dired-subtree
   dired-filter
   diredfl ;; color
   nerd-icons
   nerd-icons-dired
   ))

;; dired hide .. and .
(add-hook 'dired-mode-hook 'dired-omit-mode)

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :after dired
  :config
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode))

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

(use-package dired-filter
  :ensure t
  :after dired
  :config
  (add-hook 'dired-mode-hook 'dired-filter-mode)
  )

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode)
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

