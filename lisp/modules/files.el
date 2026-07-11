;;; files.el --- files, dired, terminals and remote access -*- lexical-binding: t -*-
;;; Commentary:
;; Files, dired, terminals and remote access: dired, tramp, eshell, eat,
;; docker, backups, auto-save and proced.
;;; Code:


;; -----------------------------------------------------------
;; DONE Tramp
;;
;; tramp-hlo
;; -----------------------------------------------------------

(with-eval-after-load 'tramp
  (unless (fboundp 'tramp-add-external-operation)
    (defun +tramp-external-operation-handler-alist (backend)
      "Return the file-name handler alist symbol for TRAMP BACKEND."
      (intern (format "%s-file-name-handler-alist" backend)))

    (defun +tramp-external-operation-handler (backend)
      "Return the file-name handler symbol for TRAMP BACKEND."
      (intern (format "%s-file-name-handler" backend)))

    (defun +tramp-refresh-external-operation-handlers (backend)
      "Refresh file-name handler operation metadata for TRAMP BACKEND."
      (let* ((handler (+tramp-external-operation-handler backend))
             (operations
              (and (boundp (+tramp-external-operation-handler-alist backend))
                   (mapcar #'car
                           (symbol-value
                            (+tramp-external-operation-handler-alist backend))))))
        (put handler 'operations operations)
        (put #'tramp-file-name-handler 'operations
             (delete-dups
              (append (get #'tramp-file-name-handler 'operations)
                      operations)))))

    (defun tramp-add-external-operation (operation handler backend)
      "Register OPERATION with HANDLER for TRAMP BACKEND.
This is a compatibility implementation for bundled TRAMP versions that
pre-date the external-operation helper API."
      (let ((handler-alist (+tramp-external-operation-handler-alist backend)))
        (unless (boundp handler-alist)
          (error "Unknown TRAMP handler alist: %s" handler-alist))
        (set handler-alist
             (cons (cons operation handler)
                   (assq-delete-all operation (symbol-value handler-alist))))
        (+tramp-refresh-external-operation-handlers backend)))

    (defun tramp-remove-external-operation (operation backend)
      "Remove OPERATION from TRAMP BACKEND.
This is a compatibility implementation for bundled TRAMP versions that
pre-date the external-operation helper API."
      (let ((handler-alist (+tramp-external-operation-handler-alist backend)))
        (when (boundp handler-alist)
          (set handler-alist
               (assq-delete-all operation (symbol-value handler-alist)))
          (+tramp-refresh-external-operation-handlers backend))))))

(use-package tramp-hlo
  :ensure t
  :config
  (tramp-hlo-setup))

;; -----------------------------------------------------------
;; DONE Dired
;;
;; dired-du
;; dired
;; dired-subtree
;; diredfl
;; dired-sidebar
;; -----------------------------------------------------------

;; dired hide .. and .
(add-hook 'dired-mode-hook 'dired-omit-mode)

(use-package dired-du
  :ensure t
  :custom
  (dired-du-size-format t))

(use-package dired
  :custom
  (dired-listing-switches
   (purecopy "-ahl -v --group-directories-first"))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-omit-extensions nil)
  (dired-dwim-target t)
  :config
  (general-define-key
   :states '(insert normal visual motion) ;; all modes
   :keymaps 'dired-mode-map
   "h"   #'dired-up-directory
   "l"   #'dired-find-file
   "T"   #'dired-create-empty-file
   "TAB" #'dired-subtree-toggle
   )
  )

(use-package dired-subtree
  :ensure t
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode))

(use-package dired-sidebar
  :ensure t
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :custom
  (dired-sidebar-theme 'ascii))

(use-package sudo-edit
  :ensure t)

(use-package docker
  :ensure t
  :custom
  (docker-show-messages nil)
  (docker-container-shell-file-name "/bin/bash"))

(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  (eat-enable-yank-to-terminal t)
  (eat-eshell-fallback-if-stty-not-available t)
  :config
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  (defun +eat/new ()
    (interactive)
    (let ((current-prefix-arg ""))
      (call-interactively 'eat)))
  )

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

(require 'init-config-tramp)
(require 'consult-tramp)

;; consult-tramp only use thses methods
(setopt consult-tramp-methods '(sshx docker sudo))

(setq +eshell/proxy +emacs/proxy)
(require 'init-config-eshell)


(provide 'init-files)
;;; files.el ends here
