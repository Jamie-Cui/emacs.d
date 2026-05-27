;;; dashboard-elfeed.el --- Elfeed section for dashboard -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jamie Cui - MIT License
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Description: Show recent Elfeed entries in dashboard
;; Package-Requires: ((emacs "30.1") (dashboard "1.8.0") (elfeed "3.4.1"))

;;; Commentary:

;; Dashboard widget integration for Elfeed.  This package is intentionally
;; self-contained so it can be loaded from an init module without depending on
;; any init-* feature.

;;; Code:

(require 'cl-lib)
(require 'dashboard)
(require 'subr-x)

(eval-when-compile
  (require 'elfeed-db))

(declare-function elfeed-db-ensure "elfeed-db")
(declare-function elfeed-db-get-entry "elfeed-db" (id))
(declare-function elfeed-db-return "elfeed-db")
(declare-function elfeed "elfeed")
(declare-function elfeed-entry-date "elfeed-db" (entry))
(declare-function elfeed-entry-feed "elfeed-db" (entry))
(declare-function elfeed-entry-id "elfeed-db" (entry))
(declare-function elfeed-entry-link "elfeed-db" (entry))
(declare-function elfeed-entry-p "elfeed-db" (entry))
(declare-function elfeed-entry-tags "elfeed-db" (entry))
(declare-function elfeed-entry-title "elfeed-db" (entry))
(declare-function elfeed-feed-title "elfeed-db" (feed))
(declare-function elfeed-feed-list "elfeed")
(declare-function elfeed-goto-line "elfeed-lib" (n))
(declare-function elfeed-search-buffer "elfeed-search")
(declare-function elfeed-search-set-filter "elfeed-search" (new-filter))
(declare-function elfeed-search-update "elfeed-search" (&optional force))
(declare-function elfeed-tagged-p "elfeed-db" (tag entry))
(declare-function elfeed-update "elfeed")
(declare-function avl-tree-mapc "avl-tree" (function tree))
(declare-function widget-get "wid-edit" (widget property))
(defvar elfeed-search--offset)
(defvar elfeed-search-entries)
(defvar elfeed-search-filter)
(defvar elfeed-update-hooks)

(defgroup dashboard-elfeed nil
  "Show Elfeed entries in dashboard."
  :group 'dashboard)

(defcustom dashboard-elfeed-update-on-open t
  "Whether to update Elfeed when opening dashboard."
  :type 'boolean
  :group 'dashboard-elfeed)

(defcustom dashboard-elfeed-update-interval nil
  "Minimum seconds between automatic dashboard-triggered Elfeed updates.

Set this to nil to update on every dashboard open."
  :type '(choice (const :tag "Every open" nil)
                 (integer :tag "Seconds"))
  :group 'dashboard-elfeed)

(defcustom dashboard-elfeed-unread-only nil
  "Whether the dashboard section should only show unread entries."
  :type 'boolean
  :group 'dashboard-elfeed)

(defcustom dashboard-elfeed-title-max-width 80
  "Maximum title width for entries displayed in dashboard."
  :type 'integer
  :group 'dashboard-elfeed)

(defcustom dashboard-elfeed-feed-title-max-width 24
  "Maximum feed title width for entries displayed in dashboard."
  :type 'integer
  :group 'dashboard-elfeed)

(defvar dashboard-elfeed--entries nil
  "Alist mapping dashboard entry keys to Elfeed entry objects.")

(defvar dashboard-elfeed--last-update-time nil
  "Last time `dashboard-elfeed-update-maybe' started an update.")

(defvar dashboard-elfeed--updating nil
  "Non-nil while an automatic Elfeed update is in progress.")

(defvar dashboard-elfeed--pending-feeds nil
  "Number of feeds still pending from the current automatic update.")

(defvar dashboard-elfeed--inhibit-update nil
  "Non-nil while dashboard-elfeed refreshes dashboard internally.")

(defvar dashboard-elfeed--installed nil
  "Non-nil when dashboard-elfeed has installed its dashboard integration.")

(defun dashboard-elfeed--feature-available-p (&optional feature)
  "Return non-nil when FEATURE can be required.

FEATURE defaults to `elfeed'."
  (require (or feature 'elfeed) nil t))

(defun dashboard-elfeed--truncate (string width)
  "Return STRING truncated to WIDTH characters."
  (truncate-string-to-width (or string "") width nil nil t))

(defun dashboard-elfeed--entry-title (entry)
  "Return display title for ENTRY."
  (let ((title (or (elfeed-entry-title entry) "")))
    (if (string-empty-p title) "Untitled" title)))

(defun dashboard-elfeed--entry-feed-title (entry)
  "Return display feed title for ENTRY."
  (let* ((feed (elfeed-entry-feed entry))
         (title (and feed (elfeed-feed-title feed))))
    (if (and title (not (string-empty-p title)))
        title
      "Unknown Feed")))

(defun dashboard-elfeed--entry-key (index entry)
  "Return a stable dashboard display key for ENTRY at INDEX."
  (format "%03d:%s" index (or (elfeed-entry-link entry)
                              (dashboard-elfeed--entry-title entry))))

(defun dashboard-elfeed--entry-display (entry)
  "Return dashboard display text for ENTRY."
  (let* ((date (format-time-string "%m-%d" (seconds-to-time (elfeed-entry-date entry))))
         (feed-title (dashboard-elfeed--truncate
                      (dashboard-elfeed--entry-feed-title entry)
                      dashboard-elfeed-feed-title-max-width))
         (title (dashboard-elfeed--truncate
                 (dashboard-elfeed--entry-title entry)
                 dashboard-elfeed-title-max-width))
         (unread-mark (if (elfeed-tagged-p 'unread entry) "*" " "))
         (link (elfeed-entry-link entry))
         (display (format (format "%%s %%s  %%-%ds  %%s"
                                  dashboard-elfeed-feed-title-max-width)
                          unread-mark date feed-title title)))
    (propertize display
                'help-echo (or link title)
                'face (if (elfeed-tagged-p 'unread entry)
                          'dashboard-items-face
                        'shadow))))

(defun dashboard-elfeed--collect-entries (limit)
  "Return up to LIMIT latest Elfeed entries."
  (when (dashboard-elfeed--feature-available-p 'elfeed-db)
    (elfeed-db-ensure)
    (let (entries)
      (with-elfeed-db-visit (entry _feed)
        (when (and (elfeed-entry-p entry)
                   (or (not dashboard-elfeed-unread-only)
                       (elfeed-tagged-p 'unread entry)))
          (push entry entries)
          (when (>= (length entries) limit)
            (elfeed-db-return))))
      (nreverse entries))))

(defun dashboard-elfeed--dashboard-entries (limit)
  "Return dashboard item keys for up to LIMIT Elfeed entries."
  (setq dashboard-elfeed--entries nil)
  (cl-loop for entry in (dashboard-elfeed--collect-entries limit)
           for index from 0
           for key = (dashboard-elfeed--entry-key index entry)
           do (push (cons key entry) dashboard-elfeed--entries)
           collect key into keys
           finally
           (setq dashboard-elfeed--entries (nreverse dashboard-elfeed--entries))
           (cl-return keys)))

(defun dashboard-elfeed--lookup-entry (key)
  "Return the Elfeed entry represented by dashboard KEY."
  (cdr (assoc key dashboard-elfeed--entries)))

(defun dashboard-elfeed--widget-key (widget)
  "Return dashboard Elfeed key stored in WIDGET."
  (get-text-property 0 'dashboard-elfeed-key (widget-get widget :tag)))

(defun dashboard-elfeed--current-entry (entry)
  "Return the current database object for ENTRY."
  (and entry
       (or (and (elfeed-entry-id entry)
                (elfeed-db-get-entry (elfeed-entry-id entry)))
           entry)))

(defun dashboard-elfeed--entry-position (entry)
  "Return ENTRY's visible position in the Elfeed search buffer."
  (cl-position entry elfeed-search-entries :test #'eq))

(defun dashboard-elfeed--goto-search-entry (entry)
  "Move point to ENTRY in the Elfeed search buffer.

Return non-nil when ENTRY is visible in the current search buffer."
  (when-let* ((position (dashboard-elfeed--entry-position entry)))
    (elfeed-goto-line (+ elfeed-search--offset position))
    (when (get-buffer-window (current-buffer) t)
      (recenter))
    t))

(defun dashboard-elfeed-jump-to-entry (widget &rest _ignore)
  "Open Elfeed search and move point to the entry represented by WIDGET."
  (interactive)
  (when (and (dashboard-elfeed--feature-available-p 'elfeed)
             (dashboard-elfeed--feature-available-p 'elfeed-search)
             (dashboard-elfeed--feature-available-p 'elfeed-db))
    (when-let* ((key (dashboard-elfeed--widget-key widget))
                (entry (dashboard-elfeed--current-entry
                        (dashboard-elfeed--lookup-entry key))))
      (elfeed)
      (with-current-buffer (elfeed-search-buffer)
        (elfeed-search-update :force)
        (unless (dashboard-elfeed--goto-search-entry entry)
          (unless (string-empty-p (or elfeed-search-filter ""))
            (elfeed-search-set-filter "")
            (dashboard-elfeed--goto-search-entry entry)))
        (unless (dashboard-elfeed--entry-position entry)
          (message "Could not find Elfeed entry in search buffer: %s"
                   (dashboard-elfeed--entry-title entry)))))))

(defun dashboard-elfeed-insert (list-size)
  "Add latest Elfeed entries to dashboard with LIST-SIZE items."
  (let ((entries (dashboard-elfeed--dashboard-entries list-size)))
    (dashboard-insert-section
     "RSS Feed:"
     entries
     list-size
     'elfeed
     (dashboard-get-shortcut 'elfeed)
     #'dashboard-elfeed-jump-to-entry
     (let* ((entry (dashboard-elfeed--lookup-entry el))
            (display (if entry
                         (dashboard-elfeed--entry-display entry)
                       el)))
       (propertize display 'dashboard-elfeed-key el)))))

(defun dashboard-elfeed--dashboard-window ()
  "Return a visible dashboard window."
  (and (boundp 'dashboard-buffer-name)
       (get-buffer-window dashboard-buffer-name t)))

(defun dashboard-elfeed-refresh-dashboard (&rest _ignore)
  "Refresh dashboard when it is currently visible."
  (when-let* ((window (dashboard-elfeed--dashboard-window)))
    (run-at-time
     0 nil
     (lambda (window)
       (when (window-live-p window)
         (with-selected-window window
           (let ((dashboard-elfeed--inhibit-update t))
             (dashboard-open)))))
     window)))

(defun dashboard-elfeed--update-due-p ()
  "Return non-nil when an automatic Elfeed update should be started."
  (and dashboard-elfeed-update-on-open
       (not dashboard-elfeed--updating)
       (or (null dashboard-elfeed-update-interval)
           (null dashboard-elfeed--last-update-time)
           (>= (float-time (time-subtract (current-time)
                                          dashboard-elfeed--last-update-time))
               dashboard-elfeed-update-interval))))

(defun dashboard-elfeed-update-maybe (&rest _ignore)
  "Update Elfeed if dashboard automatic updates are enabled and due."
  (when (and (not dashboard-elfeed--inhibit-update)
             (dashboard-elfeed--update-due-p)
             (dashboard-elfeed--feature-available-p 'elfeed))
    (setq dashboard-elfeed--last-update-time (current-time)
          dashboard-elfeed--updating t
          dashboard-elfeed--pending-feeds nil)
    (condition-case err
        (progn
          (setq dashboard-elfeed--pending-feeds (length (elfeed-feed-list)))
          (elfeed-update)
          (when (zerop dashboard-elfeed--pending-feeds)
            (setq dashboard-elfeed--updating nil
                  dashboard-elfeed--pending-feeds nil)
            (dashboard-elfeed-refresh-dashboard)))
      (error
       (setq dashboard-elfeed--updating nil
             dashboard-elfeed--pending-feeds nil)
       (message "dashboard-elfeed update failed: %s"
                (error-message-string err))))))

(defun dashboard-elfeed--finish-update (&rest _ignore)
  "Mark one automatic dashboard Elfeed feed update as finished.

Refresh dashboard once when all feeds in the current update are done."
  (when dashboard-elfeed--updating
    (setq dashboard-elfeed--pending-feeds
          (max 0 (1- (or dashboard-elfeed--pending-feeds 1))))
    (when (zerop dashboard-elfeed--pending-feeds)
      (setq dashboard-elfeed--updating nil
            dashboard-elfeed--pending-feeds nil)
      (dashboard-elfeed-refresh-dashboard))))

(defun dashboard-elfeed-setup ()
  "Install the dashboard Elfeed section and automatic update hooks."
  (interactive)
  (unless dashboard-elfeed--installed
    (add-to-list 'dashboard-item-generators
                 '(elfeed . dashboard-elfeed-insert)
                 t)
    (advice-add 'dashboard-open :before #'dashboard-elfeed-update-maybe)
    (add-hook 'elfeed-update-hooks #'dashboard-elfeed--finish-update)
    (setq dashboard-elfeed--installed t)))

(defun dashboard-elfeed-teardown ()
  "Remove dashboard Elfeed integration."
  (interactive)
  (setq dashboard-item-generators
        (assq-delete-all 'elfeed dashboard-item-generators))
  (setq dashboard-item-shortcuts
        (assq-delete-all 'elfeed dashboard-item-shortcuts))
  (advice-remove 'dashboard-open #'dashboard-elfeed-update-maybe)
  (remove-hook 'elfeed-update-hooks #'dashboard-elfeed--finish-update)
  (setq dashboard-elfeed--installed nil
        dashboard-elfeed--updating nil
        dashboard-elfeed--pending-feeds nil))

(provide 'dashboard-elfeed)
;;; dashboard-elfeed.el ends here
