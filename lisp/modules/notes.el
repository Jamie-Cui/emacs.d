;;; notes.el --- personal knowledge base on top of Org -*- lexical-binding: t -*-
;;; Commentary:
;; Personal knowledge base on top of Org: org-project, journal, roam, deft,
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

(setq +deft/org-root-dir +emacs/org-root-dir)
(require 'init-config-deft)

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
