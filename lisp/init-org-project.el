;;; init-org-project.el --- central project org workflow -*- lexical-binding: t -*-
;;; Commentary:
;;; Helpers for central project task capture, journal audit logs, and
;;; project-local archive handling.
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-capture)
(require 'org-element)
(require 'org-id)
(require 'outline)
(require 'subr-x)

(declare-function org-journal--get-entry-path "org-journal" (&optional time))
(declare-function org-journal-new-entry "org-journal" (prefix &optional time no-timestamp))

(defgroup +org-project nil
  "Central project org workflow."
  :group 'org)

(defcustom +org-projects-dir (+emacs/org-subdir "projects")
  "Directory containing central project org files."
  :type 'directory
  :group '+org-project)

(defcustom +org-project-registry nil
  "Alist mapping absolute project roots to stable project slugs."
  :type '(alist :key-type string :value-type string)
  :group '+org-project)

(defcustom +org-capture-audit-log-enabled t
  "When non-nil, log project captures to the current journal day."
  :type 'boolean
  :group '+org-project)

(defcustom +org-project-default-collapse-archive t
  "When non-nil, collapse the Archive subtree when opening project files."
  :type 'boolean
  :group '+org-project)

(defcustom +org-project-audit-refresh-delay 0.25
  "Idle delay before refreshing journal capture audit state."
  :type 'number
  :group '+org-project)

(defcustom +org-project-audit-refresh-interval 60
  "Minimum seconds between automatic audit refreshes in one journal buffer."
  :type 'number
  :group '+org-project)

(defvar-local +org-project--audit-last-refresh 0
  "Last automatic audit refresh time for the current journal buffer.")

(defconst +org-project--capture-tags '("captured" "human")
  "Tags applied to human-captured project tasks.")

(defconst +org-project--plan-tags '("agent" "ai")
  "Tags applied to agent-generated plan content.")

(defconst +org-project--audit-base-tags '("capture-log" "journal")
  "Base tags applied to journal capture audit entries.")

(defconst +org-project--audit-status-tags
  '("active" "archived" "deleted" "done" "moved" "reverted")
  "Status tags used by journal capture audit entries.")

(defun +org-project--save-buffer-no-hooks ()
  "Save the current buffer without org-heavy save hooks."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (write-region (point-min) (point-max) buffer-file-name nil 0)
  (set-buffer-modified-p nil))

(defun +org-project--inactive-timestamp (&optional time)
  "Return TIME as an inactive org timestamp string."
  (format-time-string "[%Y-%m-%d %a %H:%M]" (or time (current-time))))

(defun +org-project--normalize-root (root)
  "Return normalized absolute project ROOT, or nil."
  (when (and (stringp root)
             (not (string-empty-p root)))
    (file-name-as-directory
     (file-truename (directory-file-name root)))))

(defun +org-project--slugify (name)
  "Return a stable project slug from NAME."
  (let* ((downcased (downcase (string-trim (or name ""))))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" downcased)))
    (setq slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug))
    (if (string-empty-p slug) "project" slug)))

(defun +org-project-known-files ()
  "Return known project org files."
  (when (file-directory-p +org-projects-dir)
    (sort (directory-files +org-projects-dir t "\\.org\\'")
          #'string<)))

(defun +org-project-known-slugs ()
  "Return known project slugs."
  (mapcar #'file-name-base (+org-project-known-files)))

(defun +org-project-file-p (&optional file)
  "Return non-nil when FILE is a central project org file."
  (when-let* ((target (or file (buffer-file-name)))
              (projects-dir (and (file-directory-p +org-projects-dir)
                                 (file-name-as-directory
                                  (file-truename +org-projects-dir)))))
    (string-prefix-p projects-dir (file-truename target))))

(defun +org-project--normalize-path (path)
  "Return PATH as an absolute normalized path, or nil."
  (when (and (stringp path)
             (not (string-empty-p path)))
    (expand-file-name path)))

(defun +org-project-sync-agenda-files ()
  "Ensure central project org files are visible to `org-agenda'.

This keeps the projects directory in `org-agenda-files', which lets
Org automatically include every `*.org' file under
`+org-projects-dir'."
  (interactive)
  (when (boundp 'org-agenda-files)
    (let* ((project-dir (+org-project--normalize-path
                         (+emacs/ensure-directory +org-projects-dir)))
           (known-paths (delq nil
                              (mapcar #'+org-project--normalize-path
                                      org-agenda-files))))
      (unless (member project-dir known-paths)
        (setq org-agenda-files
              (append org-agenda-files (list project-dir))))))
  org-agenda-files)

(defun +org-project-journal-file-p (&optional file)
  "Return non-nil when FILE is inside `org-journal-dir'."
  (when-let* ((target (or file (buffer-file-name)))
              ((boundp 'org-journal-dir))
              (journal-dir (and (stringp org-journal-dir)
                                (file-directory-p org-journal-dir)
                                (file-name-as-directory
                                 (file-truename org-journal-dir)))))
    (string-prefix-p journal-dir (file-truename target))))

(defun +org-project-current-root (&optional directory)
  "Return the active project root for DIRECTORY, or nil when unavailable."
  (let ((default-directory (or directory default-directory)))
    (+org-project--normalize-root
     (or (when (and (boundp 'magent-project-root-function)
                    (functionp magent-project-root-function))
           (ignore-errors (funcall magent-project-root-function)))
         (when (and (fboundp 'projectile-project-p)
                    (ignore-errors (projectile-project-p)))
           (ignore-errors (projectile-project-root)))
         (when (fboundp 'project-current)
           (ignore-errors
             (when-let ((project (project-current nil default-directory)))
               (if (fboundp 'project-root)
                   (project-root project)
                 (car (with-no-warnings (project-roots project)))))))))))

(defun +org-project-slug-for-root (root)
  "Return the central project slug for ROOT."
  (when-let ((normalized (+org-project--normalize-root root)))
    (or (cdr (assoc normalized +org-project-registry))
        (+org-project--slugify
         (file-name-nondirectory (directory-file-name normalized))))))

(defun +org-project-file-for-slug (slug)
  "Return the project org file path for SLUG."
  (expand-file-name (concat slug ".org") (+emacs/ensure-directory +org-projects-dir)))

(defun +org-project-file-for-root (root)
  "Return the project org file path for ROOT."
  (when-let ((slug (+org-project-slug-for-root root)))
    (+org-project-file-for-slug slug)))

(defun +org-project-current-file ()
  "Return the central project org file for the active project, or nil."
  (when-let ((root (+org-project-current-root)))
    (+org-project-file-for-root root)))

(defun +org-project-select-or-detect ()
  "Return a project context plist for capture or agent updates."
  (if-let* ((root (+org-project-current-root))
            (slug (+org-project-slug-for-root root)))
      (list :slug slug
            :root root
            :file (+org-project-file-for-slug slug)
            :title (file-name-nondirectory (directory-file-name root)))
    (let* ((slugs (+org-project-known-slugs))
           (slug (if slugs
                     (completing-read "Project: " slugs nil t)
                   (read-string "Project slug: "))))
      (list :slug slug
            :root nil
            :file (+org-project-file-for-slug slug)
            :title slug))))

(defun +org-project--set-tags (tags)
  "Replace current heading tags with TAGS."
  (org-set-tags tags))

(defun +org-project--ensure-tags (required-tags)
  "Ensure REQUIRED-TAGS are present on the current heading."
  (let ((tags (org-get-tags)))
    (dolist (tag required-tags)
      (cl-pushnew tag tags :test #'string=))
    (+org-project--set-tags (sort tags #'string<))))

(defun +org-project--set-property (property value)
  "Set PROPERTY to VALUE on the current heading."
  (org-entry-put (point) property value))

(defun +org-project--find-top-heading (title)
  "Return a marker for the top-level heading named TITLE."
  (save-excursion
    (goto-char (point-min))
    (let (marker)
      (while (and (not marker)
                  (re-search-forward org-outline-regexp-bol nil t))
        (when (and (= (org-outline-level) 1)
                   (string= (org-get-heading t t t t) title))
          (setq marker (point-marker))))
      marker)))

(defun +org-project--insert-top-heading (heading)
  "Insert top-level HEADING at end of current file and return its marker."
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  (insert heading)
  (unless (string-suffix-p "\n" heading)
    (insert "\n"))
  (point-marker))

(defun +org-project--ensure-current-plan ()
  "Ensure the Current Plan subtree exists and return its marker."
  (let ((marker (+org-project--find-top-heading "Current Plan")))
    (unless marker
      (setq marker
            (+org-project--insert-top-heading
             "* PROJ Current Plan :agent:ai:\n:PROPERTIES:\n:ORIGIN: agent\n:END:\nCurrent Status: not started\nNext Step: clarify scope\n")))
    (goto-char marker)
    (org-back-to-heading t)
    (+org-project--ensure-tags +org-project--plan-tags)
    (org-id-get-create)
    (+org-project--set-property "ORIGIN" "agent")
    marker))

(defun +org-project--ensure-section (title)
  "Ensure a top-level section TITLE exists and return its marker."
  (or (+org-project--find-top-heading title)
      (+org-project--insert-top-heading (format "* %s\n" title))))

(defun +org-project--ensure-project-buffer (file title root slug)
  "Ensure FILE has the central project skeleton.
TITLE, ROOT and SLUG seed the initial metadata."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (save-excursion
        (widen)
        (goto-char (point-min))
        (when (zerop (buffer-size))
          (insert (format "#+title: %s\n" title))
          (insert (format "#+category: %s\n" slug))
          (insert "#+filetags: :project:\n")
          (when root
            (insert (format "#+PROJECT_ROOT: %s\n" root))
            (insert (format "#+MACHINE: %s\n" system-name)))
          (insert "\n"))
        (+org-project--ensure-current-plan)
        (+org-project--ensure-section "Inbox")
        (+org-project--ensure-section "Backlog")
        (+org-project--ensure-section "Log")
        (+org-project--ensure-section "Archive")
        (+org-project--save-buffer-no-hooks)))
    buffer))

(defun +org-project-ensure-file (file &optional title root slug)
  "Ensure project FILE exists with the expected skeleton and return its path."
  (let* ((expanded (expand-file-name file))
         (resolved-slug (or slug (file-name-base expanded)))
         (resolved-title (or title resolved-slug)))
    (+emacs/ensure-directory (file-name-directory expanded))
    (unless (and (file-exists-p expanded)
                 (> (file-attribute-size (file-attributes expanded)) 0))
      (+org-project--ensure-project-buffer expanded resolved-title root resolved-slug))
    (+org-project-sync-agenda-files)
    expanded))

(defun +org-project-file-dwim ()
  "Return an ensured central project file for the current or selected project."
  (let* ((context (+org-project-select-or-detect))
         (file (plist-get context :file)))
    (+org-project-ensure-file
     file
     (plist-get context :title)
     (plist-get context :root)
     (plist-get context :slug))))

(defun +org-project--goto-inbox ()
  "Move point to the insertion location in the Inbox section."
  (goto-char (+org-project--ensure-section "Inbox"))
  (org-back-to-heading t)
  (org-end-of-subtree t t)
  (unless (bolp)
    (insert "\n")))

(defun +org-project--capture-entry-point ()
  "Return point at the current capture heading."
  (goto-char (org-capture-get :begin-marker 'local))
  (org-back-to-heading t)
  (point))

(defun +org-project--refresh-id-locations (&optional files)
  "Refresh Org ID locations for project FILES."
  (let ((targets (or files (+org-project-known-files))))
    (when targets
      (org-id-update-id-locations targets))))

(defun +org-project-find-task-by-id (task-id)
  "Return a marker for TASK-ID, or nil if it no longer exists."
  (when (and (stringp task-id)
             (not (string-empty-p task-id)))
    (or (org-id-find task-id 'marker)
        (progn
          (+org-project--refresh-id-locations)
          (org-id-find task-id 'marker)))))

(defun +org-project-in-archive-p (&optional marker)
  "Return non-nil when MARKER or point is inside the Archive subtree."
  (org-with-point-at (or marker (point))
    (member "Archive" (org-get-outline-path t t))))

(defun +org-project-capture-target ()
  "Visit the right project org file and move point to Inbox."
  (let* ((context (+org-project-select-or-detect))
         (file (+org-project-ensure-file
                (plist-get context :file)
                (plist-get context :title)
                (plist-get context :root)
                (plist-get context :slug))))
    (org-capture-put :project-context context)
    (set-buffer (find-file-noselect file))
    (widen)
    (+org-project--goto-inbox)))

(defun +org-project-capture-prepare-finalize ()
  "Normalize a captured project task before finalization."
  (when-let* ((context (org-capture-get :project-context 'local))
              (point (+org-project--capture-entry-point)))
    (save-excursion
      (goto-char point)
      (let ((task-id (org-id-get-create)))
        (+org-project--ensure-tags +org-project--capture-tags)
        (+org-project--set-property "ORIGIN" "human-capture")
        (+org-project--set-property "PROJECT" (plist-get context :slug))
        (+org-project--set-property "PROJECT_FILE" (plist-get context :file))
        (when-let ((root (plist-get context :root)))
          (+org-project--set-property "PROJECT_ROOT" root))
        (+org-project--set-property "MACHINE" system-name)
        (+org-project--set-property "CREATED_AT" (+org-project--inactive-timestamp))
        (+org-project--set-property "TASK_ID" task-id)
        (org-capture-put :project-task-id task-id)
        (org-capture-put :project-task-title (org-get-heading t t t t))))))

(defun +org-project--find-journal-day-heading (created)
  "Return a marker for the current journal day heading with CREATED property."
  (let (marker)
    (org-map-entries
     (lambda ()
       (when (and (= (org-outline-level) 1)
                  (string= (org-entry-get (point) "CREATED") created))
         (setq marker (point-marker))))
     nil 'file)
    marker))

(defun +org-project--audit-entry-body (status task-id &optional marker)
  "Return audit log body text for STATUS, TASK-ID, and optional MARKER."
  (pcase status
    ('active (format "- task: [[id:%s][open project task]]\n" task-id))
    ('done (format "- task completed: [[id:%s][open project task]]\n" task-id))
    ('archived (format "- task archived: [[id:%s][open project task]]\n" task-id))
    ('reverted (format "- task marked KILL: [[id:%s][open project task]]\n" task-id))
    ('moved (format "- task moved to %s\n"
                    (abbreviate-file-name
                     (or (and marker
                              (buffer-file-name (marker-buffer marker)))
                         ""))))
    (_ "- task removed from project file after capture\n")))

(defun +org-project-log-capture (context task-id task-title)
  "Write a journal audit entry for a captured project task."
  (require 'org-journal)
  (let* ((time (current-time))
         (created (format-time-string "%Y%m%d" time))
         (entry-path (org-journal--get-entry-path time))
         (slug (plist-get context :slug))
         (project-file (plist-get context :file)))
    (save-window-excursion
      (let ((org-journal-find-file-fn #'find-file))
        (org-journal-new-entry t time))
      (with-current-buffer (find-file-noselect entry-path)
        (save-excursion
          (widen)
          (goto-char (+org-project--find-journal-day-heading created))
          (org-back-to-heading t)
          (org-end-of-subtree t t)
          (unless (bolp)
            (insert "\n"))
          (insert (format "** CAPTURE [%s] %s\n:PROPERTIES:\n:TASK_ID: %s\n:PROJECT: %s\n:PROJECT_FILE: %s\n:AUDIT_STATUS: active\n:CREATED_AT: %s\n:END:\n%s"
                          slug
                          task-title
                          task-id
                          slug
                          project-file
                          (+org-project--inactive-timestamp time)
                          (+org-project--audit-entry-body 'active task-id)))
          (org-back-to-heading t)
          (+org-project--set-tags (append +org-project--audit-base-tags '("active")))
          (+org-project--save-buffer-no-hooks))))))

(defun +org-project-capture-after-finalize ()
  "Finalize project capture side effects."
  (when (and (not org-note-abort)
             (org-capture-get :project-context))
    (let* ((context (org-capture-get :project-context))
           (project-file (plist-get context :file))
           (task-id (org-capture-get :project-task-id))
           (task-title (or (org-capture-get :project-task-title) "Captured task")))
      (when-let ((buf (get-file-buffer project-file)))
        (with-current-buffer buf
          (+org-project--save-buffer-no-hooks)))
      (+org-project--refresh-id-locations (list project-file))
      (when +org-capture-audit-log-enabled
        (+org-project-log-capture context task-id task-title)))))

(defun +org-project-audit-status-for-task-id (task-id &optional expected-file)
  "Return audit status symbol for TASK-ID.
EXPECTED-FILE is the project file recorded in the journal audit entry."
  (when task-id
    (let ((marker (+org-project-find-task-by-id task-id)))
      (cond
       ((not marker) 'deleted)
       (t
        (org-with-point-at marker
          (cond
           ((+org-project-in-archive-p marker) 'archived)
           ((equal (org-get-todo-state) "KILL") 'reverted)
           ((equal (org-get-todo-state) "DONE") 'done)
           ((and expected-file
                 (buffer-file-name (marker-buffer marker))
                 (not (equal (file-truename expected-file)
                             (file-truename
                              (buffer-file-name (marker-buffer marker))))))
            'moved)
           (t 'active))))))))

(defun +org-project--replace-subtree-body (text)
  "Replace current top-level subtree body with TEXT."
  (org-back-to-heading t)
  (let ((beg (save-excursion
               (forward-line 1)
               (when (looking-at ":PROPERTIES:")
                 (re-search-forward "^:END:$" nil t)
                 (forward-line 1))
               (point)))
        (end (save-excursion
               (forward-line 1)
               (if (re-search-forward "^\\* " nil t)
                   (match-beginning 0)
                 (point-max)))))
    (delete-region beg end)
    (goto-char beg)
    (insert (string-trim-right text) "\n")))

(defun +org-project-audit-refresh-entry ()
  "Refresh the capture audit entry at point."
  (interactive)
  (org-back-to-heading t)
  (let* ((task-id (org-entry-get (point) "TASK_ID"))
           (expected-file (org-entry-get (point) "PROJECT_FILE"))
           (current-status (org-entry-get (point) "AUDIT_STATUS"))
           (marker (+org-project-find-task-by-id task-id))
           (status (or (+org-project-audit-status-for-task-id task-id expected-file)
                       'deleted))
           (status-name (symbol-name status)))
    (when (not (equal current-status status-name))
      (+org-project--set-property "REVIEWED_AT" (+org-project--inactive-timestamp)))
    (+org-project--set-property "AUDIT_STATUS" status-name)
    (+org-project--set-tags
     (append +org-project--audit-base-tags (list status-name)))
    (+org-project--replace-subtree-body
     (+org-project--audit-entry-body status task-id marker))))

(defun +org-project-audit-refresh-current-journal ()
  "Refresh capture audit entries in the current journal file."
  (interactive)
  (unless (+org-project-journal-file-p)
    (user-error "Current buffer is not an org-journal file"))
  (+org-project--refresh-id-locations)
  (save-excursion
    (widen)
    (org-map-entries
     #'+org-project-audit-refresh-entry
     "+capture-log"
     'file))
  (setq-local +org-project--audit-last-refresh (float-time))
  (+org-project--save-buffer-no-hooks))

(defun +org-project--configure-project-buffer-h ()
  "Disable heavyweight org extras in central project files."
  (when (+org-project-file-p)
    (remove-hook 'before-save-hook #'toc-org-insert-toc t)
    (remove-hook 'before-save-hook #'xenops-render-at-point t)
    (when (bound-and-true-p toc-org-mode)
      (toc-org-mode -1))
    (when (fboundp 'xenops-mode)
      (ignore-errors
        (when (bound-and-true-p xenops-mode)
          (xenops-mode -1))))))

(defun +org-project--maybe-refresh-journal-audit-h ()
  "Lazy refresh capture audit state when opening a journal buffer."
  (when (and (+org-project-journal-file-p)
             (> (- (float-time) +org-project--audit-last-refresh)
                +org-project-audit-refresh-interval)
             (save-excursion
               (goto-char (point-min))
               (search-forward ":capture-log:" nil t)))
    (let ((buffer (current-buffer)))
      (run-with-idle-timer
       +org-project-audit-refresh-delay nil
       (lambda (buf)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (+org-project-journal-file-p)
               (+org-project-audit-refresh-current-journal)))))
       buffer))))

(defun +org-project-collapse-archive ()
  "Collapse the Archive subtree in the current project file."
  (interactive)
  (when-let ((marker (+org-project--find-top-heading "Archive")))
    (save-excursion
      (goto-char marker)
      (outline-hide-subtree))))

(defun +org-project-toggle-archive-visibility ()
  "Toggle visibility of the Archive subtree in the current project file."
  (interactive)
  (if-let ((marker (+org-project--find-top-heading "Archive")))
      (save-excursion
        (goto-char marker)
        (org-cycle))
    (user-error "No Archive section in current project file")))

(defun +org-project--maybe-collapse-archive-h ()
  "Collapse Archive after opening a project file."
  (when (and +org-project-default-collapse-archive
             (+org-project-file-p))
    (let ((buffer (current-buffer)))
      (run-with-idle-timer
       0 nil
       (lambda (buf)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (+org-project-file-p)
               (+org-project-collapse-archive)))))
       buffer))))

(defun +org-project-archive-task (&optional reason)
  "Move the current subtree into the file-local Archive section.
REASON defaults to `manual-cleanup'."
  (interactive)
  (unless (+org-project-file-p)
    (user-error "Current buffer is not a central project file"))
  (org-back-to-heading t)
  (when (= (org-outline-level) 1)
    (user-error "Archive only applies to task headings, not top-level sections"))
  (let* ((reason (or reason "manual-cleanup"))
         (origin (point-marker))
         (beg (progn (org-back-to-heading t) (point)))
         (end (save-excursion (org-end-of-subtree t t) (point)))
         (contents (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (goto-char (+org-project--ensure-section "Archive"))
    (org-back-to-heading t)
    (org-end-of-subtree t t)
    (unless (bolp)
      (insert "\n"))
    (let ((inserted-root (point-marker)))
      (insert contents)
      (goto-char inserted-root)
      (org-back-to-heading t)
      (move-marker inserted-root nil))
    (when (looking-at "\\*+ ")
      (replace-match "** "))
    (+org-project--set-property "ARCHIVED_AT" (+org-project--inactive-timestamp))
    (+org-project--set-property "ARCHIVE_REASON" reason)
    (+org-project--save-buffer-no-hooks)
    (move-marker origin nil)))

(defun +org-project-archive-done-task ()
  "Archive the current task when it is DONE or KILL."
  (interactive)
  (let ((state (org-get-todo-state)))
    (unless (member state '("DONE" "KILL"))
      (user-error "Only DONE or KILL tasks can be archived"))
    (+org-project-archive-task
     (if (equal state "DONE") "done" "killed"))))

(defun +org-project-read-current-plan (&optional project-file)
  "Return the Current Plan subtree text from PROJECT-FILE."
  (let ((file (or project-file (+org-project-current-file))))
    (unless file
      (user-error "No current project file available"))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (widen)
        (goto-char (or (+org-project--find-top-heading "Current Plan")
                       (+org-project--ensure-current-plan)))
        (buffer-substring-no-properties
         (point)
         (save-excursion
           (forward-line 1)
           (if (re-search-forward "^\\* " nil t)
               (match-beginning 0)
             (point-max))))))))

(defun +org-project-upsert-current-plan (project-file body)
  "Replace PROJECT-FILE Current Plan body with BODY."
  (interactive "fProject file: \nsPlan body: ")
  (let ((file (+org-project-ensure-file project-file)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (widen)
        (goto-char (or (+org-project--find-top-heading "Current Plan")
                       (+org-project--ensure-current-plan)))
        (+org-project--replace-subtree-body body)
        (+org-project--save-buffer-no-hooks))))
  project-file)

(defun +org-project-append-log (project-file text)
  "Append TEXT to the Log section of PROJECT-FILE."
  (interactive "fProject file: \nsLog text: ")
  (let ((file (+org-project-ensure-file project-file)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (widen)
        (goto-char (+org-project--ensure-section "Log"))
        (forward-line 1)
        (if (re-search-forward "^\\* " nil t)
            (goto-char (match-beginning 0))
          (goto-char (point-max)))
        (insert (format "** %s :agent:ai:\n:PROPERTIES:\n:ORIGIN: agent\n:UPDATED_AT: %s\n:END:\n%s\n"
                        (+org-project--inactive-timestamp)
                        (+org-project--inactive-timestamp)
                        (string-trim-right text)))
        (+org-project--save-buffer-no-hooks))))
  project-file)

(defun +org-project-capture ()
  "Run the central project capture template."
  (interactive)
  (org-capture nil "p"))

(with-eval-after-load 'org-capture
  (setq org-capture-templates
        (cl-remove-if (lambda (template)
                        (equal (car-safe template) "p"))
                      org-capture-templates))
  (add-to-list 'org-capture-templates
               '("p" "Project task" entry
                 (function +org-project-capture-target)
                 "** TODO %?\n"
                 :empty-lines-before 1
                 :empty-lines-after 1
                 :no-save t
                 :unnarrowed t
                 :prepare-finalize +org-project-capture-prepare-finalize
                 :after-finalize +org-project-capture-after-finalize)))

(add-hook 'org-mode-hook #'+org-project--configure-project-buffer-h)
(add-hook 'org-mode-hook #'+org-project--maybe-refresh-journal-audit-h)
(add-hook 'org-mode-hook #'+org-project--maybe-collapse-archive-h)

(with-eval-after-load 'org-agenda
  (+org-project-sync-agenda-files))

(provide 'init-org-project)
;;; init-org-project.el ends here
