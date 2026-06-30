;;; core-util.el --- Cross-cutting utilities -*- lexical-binding: t -*-
;;; Commentary:
;; Genuinely cross-domain commands and startup/maintenance helpers.  Most
;; utility commands live with the module that owns their domain; only things
;; used across modules belong here.  Populated in later refactor phases.
;;; Code:

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

(provide 'core-util)
;;; core-util.el ends here
