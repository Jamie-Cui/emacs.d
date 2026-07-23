;;; notes.el --- personal knowledge base on top of Org -*- lexical-binding: t -*-
;;; Commentary:
;; Personal knowledge base on top of Org: org-project, journal, roam, Denote,
;; agenda hygiene and consult integration.
;;; Code:


(add-to-list 'load-path (expand-file-name "site-lisp" +emacs/repo-directory))
(setq +org-project-root-dir +emacs/org-root-dir)
(require 'org-project)
(require 'dashboard-org-project)
(dashboard-org-project-setup)

;; -----------------------------------------------------------
;; DONE hack: filter out sync-conflict and *-beorg
;; -----------------------------------------------------------

(defgroup +org-agenda nil
  "Local agenda hygiene helpers."
  :group 'org)

(defcustom +org-agenda-ignored-file-regexps
  '("\\.sync-conflict-[^/]*\\.org\\'"
    "/[^/]+-beorg\\.org\\'")
  "Regexps for agenda files that should be ignored locally."
  :type '(repeat string)
  :group '+org-agenda)

(defun +org-agenda-ignored-file-p (file)
  "Return non-nil when FILE should be excluded from `org-agenda-files'."
  (when (stringp file)
    (let ((path (expand-file-name file))
          ignored)
      (dolist (regexp +org-agenda-ignored-file-regexps ignored)
        (when (string-match-p regexp path)
          (setq ignored t))))))

(defun +org-agenda-prune-files (&optional files)
  "Remove ignored entries from FILES or `org-agenda-files'."
  (interactive)
  (let* ((targets (or files org-agenda-files))
         (normalized (delete-dups
                      (delq nil
                            (mapcar (lambda (file)
                                      (when (stringp file)
                                        (expand-file-name file)))
                                    (copy-sequence targets)))))
         (filtered (delq nil
                         (mapcar (lambda (file)
                                   (unless (+org-agenda-ignored-file-p file)
                                     file))
                                 normalized))))
    (when (or (null files)
              (called-interactively-p 'interactive))
      (setq org-agenda-files filtered))
    filtered))

(defun +org-agenda--prune-after-journal-update (&rest _)
  "Keep generated journal side files out of `org-agenda-files'."
  (+org-agenda-prune-files))

(use-package org-journal
  :ensure t
  :custom
  (org-journal-dir (concat +emacs/org-root-dir "/journal"))
  (org-journal-find-file-fn 'find-file)
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-file-type 'monthly)
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"WAIT\"|TODO=\"PROJ\"")
  (org-journal-enable-agenda-integration t)
  :config
  (setq org-element-use-cache nil)
  (add-to-list 'org-agenda-files org-journal-dir)
  (+org-project-sync-agenda-files)
  (+org-agenda-prune-files)
  (advice-add 'org-journal--update-org-agenda-files
              :after
              #'+org-agenda--prune-after-journal-update))

(defvar-local +notes/denote--syncing-file-name nil
  "Non-nil while synchronizing a Denote file name after saving.")

(declare-function denote-menu-get-path-by-id "denote-menu" (id file-type))
(declare-function denote-menu-update-entries "denote-menu" ())

(defun +notes/denote-sync-file-name-after-save-h ()
  "Synchronize the current Denote file name with its front matter."
  (when (and (not +notes/denote--syncing-file-name)
             buffer-file-name
             (denote-file-is-in-denote-directory-p buffer-file-name)
             (denote-file-has-denoted-filename-p buffer-file-name))
    (let ((+notes/denote--syncing-file-name t)
          (denote-rename-confirmations nil)
          (denote-save-buffers t))
      (condition-case err
          (denote-rename-file-using-front-matter buffer-file-name)
        (error
         (display-warning
          'denote
          (format "Could not synchronize `%s': %s"
                  (file-name-nondirectory buffer-file-name)
                  (error-message-string err))
          :warning))))))

(defun +notes/denote-menu-file-at-point ()
  "Return the Denote file represented by the menu row at point."
  (unless (derived-mode-p 'denote-menu-mode)
    (user-error "Not in a Denote menu"))
  (let* ((entry-id (tabulated-list-get-id))
         (parts (and (stringp entry-id) (split-string entry-id "-")))
         (identifier (car parts))
         (file-type (cadr parts))
         (file (and identifier
                    file-type
                    (denote-menu-get-path-by-id identifier file-type))))
    (unless (and file (file-exists-p file))
      (user-error "No Denote file on this row"))
    file))

(defun +notes/denote-menu-refresh ()
  "Refresh the current Denote menu without retaining stale paths."
  (setq tabulated-list-entries nil)
  (denote-menu-update-entries))

(defun +notes/denote-menu-rename-title ()
  "Rename the Denote file title represented by the menu row at point."
  (interactive)
  (let* ((file (+notes/denote-menu-file-at-point))
         (existing-buffer (find-buffer-visiting file))
         (note-buffer (or existing-buffer (find-file-noselect file))))
    (when (and existing-buffer (buffer-modified-p existing-buffer))
      (user-error "Save the note before renaming it"))
    (unwind-protect
        (with-current-buffer note-buffer
          (let ((+notes/denote--syncing-file-name t)
                (denote-save-buffers t))
            (call-interactively #'denote-rename-file-title)))
      (when (and (not existing-buffer)
                 (buffer-live-p note-buffer)
                 (not (buffer-modified-p note-buffer)))
        (kill-buffer note-buffer))))
  (+notes/denote-menu-refresh))

(defun +notes/denote-menu-new ()
  "Create a Denote note and refresh the menu that launched the command."
  (interactive)
  (let ((menu-buffer (current-buffer)))
    (call-interactively #'denote)
    (when (buffer-live-p menu-buffer)
      (with-current-buffer menu-buffer
        (+notes/denote-menu-refresh)))))

(defun +notes/denote-menu-archive ()
  "Archive the Denote file represented by the menu row at point."
  (interactive)
  (let* ((file (+notes/denote-menu-file-at-point))
         (root (seq-find (lambda (directory)
                           (file-in-directory-p file directory))
                         (denote-directories)))
         (relative-file (and root (file-relative-name file root)))
         (archive-root (and root (expand-file-name "archive" root)))
         (destination (and archive-root
                           relative-file
                           (expand-file-name relative-file archive-root)))
         (note-buffer (find-buffer-visiting file)))
    (unless root
      (user-error "The note is outside `denote-directory'"))
    (when (and note-buffer (buffer-modified-p note-buffer))
      (user-error "Save the note before archiving it"))
    (when (file-exists-p destination)
      (user-error "Archive destination already exists: %s" destination))
    (when (yes-or-no-p
           (format "Archive `%s'? " (file-name-nondirectory file)))
      (make-directory (file-name-directory destination) t)
      (rename-file file destination)
      (when note-buffer
        (with-current-buffer note-buffer
          (set-visited-file-name destination t)
          (set-buffer-modified-p nil)))
      (denote-update-dired-buffers)
      (+notes/denote-menu-refresh)
      (message "Archived to %s" destination))))

(use-package denote
  :ensure t
  :demand t
  :custom
  (denote-directory
   (list (+emacs/org-subdir "deft")
         (+emacs/org-subdir "projects")))
  (denote-file-type 'org)
  (denote-prompts '(title keywords))
  (denote-save-buffers nil)
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (denote-excluded-directories-regexp
   "\\`\\(?:archive\\|img\\|sty\\)\\'")
  (denote-excluded-files-regexp
   "\\(?:\\.sync-conflict-[^/]*\\.org\\'\\|/[^/]+-beorg\\.org\\'\\)")
  (denote-dired-directories
   (list (+emacs/org-subdir "deft")
         (+emacs/org-subdir "projects")))
  (denote-dired-directories-include-subdirectories t)
  :hook
  (dired-mode . denote-dired-mode-in-directories)
  :config
  (denote-rename-buffer-mode +1)
  (add-hook 'after-save-hook #'+notes/denote-sync-file-name-after-save-h))

(use-package denote-menu
  :ensure t
  :commands (denote-menu-list-notes list-denotes)
  :bind
  (:map denote-menu-mode-map
        ("C-c C-r" . +notes/denote-menu-rename-title)
        ("C-c C-n" . +notes/denote-menu-new)
        ("C-c C-a" . +notes/denote-menu-archive))
  :config
  (evil-define-key 'normal denote-menu-mode-map
    (kbd "R") #'+notes/denote-menu-rename-title
    (kbd "N") #'+notes/denote-menu-new
    (kbd "A") #'+notes/denote-menu-archive))

(use-package consult-denote
  :ensure t
  :after (denote consult)
  :config
  (consult-denote-mode +1))

(use-package org-roam
  :ensure t
  :after evil
  :custom
  (org-roam-directory (+emacs/org-subdir "roam"))
  :config
  ;; 例如改成只用时间戳：
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y-%m-%dt%H%M>.org" "#+title: ${title}\n")
           :unnarrowed t)))

  ;; If you're using a vertical completion framework, you might want
  ;; a more informative completion interface
  (setq org-roam-node-display-template
        (format "${title:50}%s"
                (propertize "${tags:25}" 'face 'org-tag)))
  (require 'org-roam-db)
  (org-roam-db-autosync-mode +1)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)

  ;; NOTE Make org-roam case insensitve
  ;; from: https://emacs.stackexchange.com/a/77296
  (defun +org-roam/case-insensitive-org-roam-node-read (orig-fn &rest args)
    (let ((completion-ignore-case t))
      (apply orig-fn args)))
  (advice-add 'org-roam-node-read :around #'+org-roam/case-insensitive-org-roam-node-read)
  (advice-add 'org-roam-node-insert :before #'+evil/smart-insert))

(require 'init-config-consult)


(provide 'init-notes)
;;; notes.el ends here
