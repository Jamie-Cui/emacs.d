;;; dashboard-org-project.el --- Org Project section for dashboard -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jamie Cui - MIT License
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Description: Show Org Project action items in dashboard
;; Package-Requires: ((emacs "30.1") (dashboard "1.8.0"))

;;; Commentary:

;; Dashboard widget integration for org-project.  This package is intentionally
;; self-contained so it can be loaded from an init module without depending on
;; any init-* feature.

;;; Code:

(require 'cl-lib)
(require 'dashboard)
(require 'subr-x)

(declare-function +org-project--collect-action-items "org-project" (&optional filter))
(declare-function +org-project--goto-source "org-project" (source))
(declare-function +org-project--marker-source "org-project" (marker))
(declare-function +org-project--render-todo-list "org-project" (&optional ignore-auto noconfirm))
(declare-function +org-project--time-face "org-project" (item))
(declare-function org-get-todo-face "org" (kwd))
(declare-function org-project-todo-list-mode "org-project")
(declare-function widget-get "wid-edit" (widget property))
(defvar org-project-todo-list--keyword-filter)
(defvar org-project-todo-list-buffer-name)

(defgroup dashboard-org-project nil
  "Show Org Project action items in dashboard."
  :group 'dashboard)

(defcustom dashboard-org-project-keyword-filter nil
  "TODO keyword filter for dashboard Org Project action items.

When this is a list, any listed TODO keyword matches."
  :type '(choice (const :tag "All active items" nil)
                 (string :tag "TODO keyword")
                 (repeat :tag "TODO keywords" string))
  :group 'dashboard-org-project)

(defface dashboard-org-project-state-face
  '((t (:inherit font-lock-keyword-face :weight semibold)))
  "Fallback face for Org Project TODO states in dashboard."
  :group 'dashboard-org-project)

(defface dashboard-org-project-project-face
  '((t (:inherit font-lock-doc-face)))
  "Face for Org Project project names in dashboard."
  :group 'dashboard-org-project)

(defface dashboard-org-project-context-face
  '((t (:inherit shadow)))
  "Face for Org Project hierarchy context in dashboard."
  :group 'dashboard-org-project)

(defface dashboard-org-project-time-face
  '((t (:inherit shadow)))
  "Fallback face for Org Project planning time in dashboard."
  :group 'dashboard-org-project)

(defcustom dashboard-org-project-action-max-width 64
  "Maximum action title width for entries displayed in dashboard."
  :type 'integer
  :group 'dashboard-org-project)

(defcustom dashboard-org-project-project-max-width 18
  "Maximum project title width for entries displayed in dashboard."
  :type 'integer
  :group 'dashboard-org-project)

(defcustom dashboard-org-project-context-max-width 28
  "Maximum hierarchy width for entries displayed in dashboard."
  :type 'integer
  :group 'dashboard-org-project)

(defvar dashboard-org-project--items nil
  "Alist mapping dashboard entry keys to Org Project action items.")

(defvar dashboard-org-project--installed nil
  "Non-nil when dashboard-org-project has installed its dashboard integration.")

(defun dashboard-org-project--feature-available-p (&optional feature)
  "Return non-nil when FEATURE can be required.

FEATURE defaults to `org-project'."
  (require (or feature 'org-project) nil t))

(defun dashboard-org-project--truncate (string width)
  "Return STRING truncated to WIDTH characters."
  (truncate-string-to-width (or string "") width nil nil t))

(defun dashboard-org-project--keyword-filter ()
  "Return normalized TODO keyword filter for Org Project."
  (cond
   ((null dashboard-org-project-keyword-filter)
    nil)
   ((stringp dashboard-org-project-keyword-filter)
    (unless (string-blank-p dashboard-org-project-keyword-filter)
      dashboard-org-project-keyword-filter))
   ((listp dashboard-org-project-keyword-filter)
    (when dashboard-org-project-keyword-filter
      (string-join dashboard-org-project-keyword-filter "|")))))

(defun dashboard-org-project--item-key (index item)
  "Return a stable dashboard display key for ITEM at INDEX."
  (format "%03d:%S" index
          (+org-project--marker-source (plist-get item :marker))))

(defun dashboard-org-project--item-display (item)
  "Return dashboard display text for ITEM."
  (let* ((state (or (plist-get item :state) ""))
         (project (dashboard-org-project--truncate
                   (or (plist-get item :project) "")
                   dashboard-org-project-project-max-width))
         (context (dashboard-org-project--truncate
                   (or (plist-get item :hierarchy) "")
                   dashboard-org-project-context-max-width))
         (action (dashboard-org-project--truncate
                  (or (plist-get item :action) "")
                  dashboard-org-project-action-max-width))
         (time (or (plist-get item :time) ""))
         (meta (string-join (delq nil
                                  (list (unless (string-empty-p project)
                                          project)
                                        (unless (string-empty-p context)
                                          context)))
                            " / "))
         (help (string-join (delq nil
                                  (list (plist-get item :project)
                                        (plist-get item :hierarchy)
                                        (plist-get item :action)
                                        (unless (string-empty-p time)
                                          time)))
                            " / ")))
    (concat
     (propertize (format "%-5s" state)
                 'face (or (and (not (string-empty-p state))
                                (org-get-todo-face state))
                           'dashboard-org-project-state-face)
                 'help-echo help)
     " "
     (propertize action
                 'face 'dashboard-items-face
                 'help-echo help)
     (unless (string-empty-p meta)
       (concat
        (propertize "  " 'help-echo help)
        (propertize meta
                    'face (if (string-empty-p project)
                              'dashboard-org-project-context-face
                            'dashboard-org-project-project-face)
                    'help-echo help)))
     (unless (string-empty-p time)
       (concat
        (propertize "  " 'help-echo help)
        (propertize time
                    'face (or (ignore-errors (+org-project--time-face item))
                              'dashboard-org-project-time-face)
                    'help-echo help))))))

(defun dashboard-org-project--collect-items (limit)
  "Return up to LIMIT Org Project action items."
  (when (dashboard-org-project--feature-available-p)
    (dashboard-org-project--feature-available-p 'org)
    (dashboard-org-project--feature-available-p 'tabulated-list)
    (dashboard-org-project--feature-available-p 'org-agenda)
    (dashboard-org-project--feature-available-p 'org-id)
    (let ((items (+org-project--collect-action-items
                  (dashboard-org-project--keyword-filter))))
      (cl-subseq items 0 (min limit (length items))))))

(defun dashboard-org-project--dashboard-entries (limit)
  "Return dashboard item keys for up to LIMIT Org Project action items."
  (setq dashboard-org-project--items nil)
  (cl-loop for item in (dashboard-org-project--collect-items limit)
           for index from 0
           for key = (dashboard-org-project--item-key index item)
           do (push (cons key item) dashboard-org-project--items)
           collect key into keys
           finally
           (setq dashboard-org-project--items
                 (nreverse dashboard-org-project--items))
           (cl-return keys)))

(defun dashboard-org-project--lookup-item (key)
  "Return the Org Project action item represented by dashboard KEY."
  (cdr (assoc key dashboard-org-project--items)))

(defun dashboard-org-project--widget-key (widget)
  "Return dashboard Org Project key stored in WIDGET."
  (get-text-property 0 'dashboard-org-project-key (widget-get widget :tag)))

(defun dashboard-org-project--goto-todo-source (source)
  "Open Org Project TODO list and move point to SOURCE."
  (let ((buffer (get-buffer-create org-project-todo-list-buffer-name)))
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (org-project-todo-list-mode)
      (setq-local org-project-todo-list--keyword-filter
                  (dashboard-org-project--keyword-filter))
      (+org-project--render-todo-list)
      (unless (+org-project--goto-source source)
        (goto-char (point-min)))
      (when (get-buffer-window (current-buffer) t)
        (recenter)))))

(defun dashboard-org-project-jump-to-item (widget &rest _ignore)
  "Open Org Project TODO list and move point to the item represented by WIDGET."
  (interactive)
  (when (dashboard-org-project--feature-available-p)
    (when-let* ((key (dashboard-org-project--widget-key widget))
                (item (dashboard-org-project--lookup-item key))
                (source (+org-project--marker-source
                         (plist-get item :marker))))
      (dashboard-org-project--goto-todo-source source))))

(defun dashboard-org-project-insert (list-size)
  "Add Org Project action items to dashboard with LIST-SIZE items."
  (let ((items (dashboard-org-project--dashboard-entries list-size)))
    (dashboard-insert-section
     "Org Project:"
     items
     list-size
     'org-project
     (dashboard-get-shortcut 'org-project)
     #'dashboard-org-project-jump-to-item
     (let* ((item (dashboard-org-project--lookup-item el))
            (display (if item
                         (dashboard-org-project--item-display item)
                       el)))
       (propertize display 'dashboard-org-project-key el)))))

(defun dashboard-org-project-setup ()
  "Install the dashboard Org Project section."
  (interactive)
  (unless dashboard-org-project--installed
    (add-to-list 'dashboard-item-generators
                 '(org-project . dashboard-org-project-insert)
                 t)
    (setq dashboard-org-project--installed t)))

(defun dashboard-org-project-teardown ()
  "Remove dashboard Org Project integration."
  (interactive)
  (setq dashboard-item-generators
        (assq-delete-all 'org-project dashboard-item-generators))
  (setq dashboard-item-shortcuts
        (assq-delete-all 'org-project dashboard-item-shortcuts))
  (setq dashboard-org-project--installed nil))

(provide 'dashboard-org-project)
;;; dashboard-org-project.el ends here
