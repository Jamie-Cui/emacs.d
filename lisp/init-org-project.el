;;; init-org-project.el --- central project org workflow -*- lexical-binding: t -*-
;;; Commentary:
;;; Helpers for central project task capture, journal audit logs, and
;;; project-local archive handling.
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-element)
(require 'org-id)
(require 'outline)
(require 'subr-x)
(require 'tabulated-list)

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

(defconst +org-project--audit-base-tags '("capture-log" "journal")
  "Base tags applied to journal capture audit entries.")

(defconst +org-project--audit-status-tags
  '("active" "archived" "deleted" "done" "moved" "reverted")
  "Status tags used by journal capture audit entries.")

(defconst org-project-todo-list-buffer-name "*Org Project TODO List*"
  "Buffer name used by `org-project-todo-list'.")

(defvar-local org-project-todo-list--keyword-filter nil
  "Current TODO keyword filter for `org-project-todo-list'.")

(defvar-local org-project-todo-list--edit-marker nil
  "Source marker for the row currently being edited.")

(defvar-local org-project-todo-list--edit-original nil
  "Original action item text before editing.")

(defvar-local org-project-todo-list--edit-overlay nil
  "Overlay covering the editable action item region.")

(defvar-local org-project-todo-list--edit-start nil
  "Marker at the start of the editable action item region.")

(defvar-local org-project-todo-list--edit-end nil
  "Marker at the end of the editable action item region.")

(defvar-local org-project-todo-list--edit-guards nil
  "Read-only guard overlays outside the editable region.")

(defvar-local org-project-todo-list--header-columns nil
  "Base tabulated-list header used by `org-project-todo-list'.")

(defvar-local org-project-todo-list--render-width nil
  "Last window width used to render `org-project-todo-list'.")

(defface org-project-todo-list-project-face
  '((t (:inherit font-lock-doc-face)))
  "Face used for the project column."
  :group '+org-project)

(defface org-project-todo-list-hierarchy-face
  '((t (:inherit shadow)))
  "Face used for the hierarchy column."
  :group '+org-project)

(defface org-project-todo-list-tag-face
  '((t (:inherit org-tag)))
  "Face used for the tag column."
  :group '+org-project)

(defface org-project-todo-list-action-face
  '((t (:inherit default :weight semibold)))
  "Base face used for action items."
  :group '+org-project)

(defface org-project-todo-list-action-wait-face
  '((t (:inherit (org-project-todo-list-action-face warning))))
  "Face used for WAIT action items."
  :group '+org-project)

(defface org-project-todo-list-action-project-face
  '((t (:inherit (org-project-todo-list-action-face font-lock-keyword-face))))
  "Face used for PROJ action items that are still leaf actions."
  :group '+org-project)

(defface org-project-todo-list-deadline-face
  '((t (:inherit shadow)))
  "Face used for deadlines that are not urgent."
  :group '+org-project)

(defface org-project-todo-list-deadline-soon-face
  '((t (:inherit warning :weight semibold)))
  "Face used for deadlines due soon."
  :group '+org-project)

(defface org-project-todo-list-deadline-overdue-face
  '((t (:inherit error :weight bold)))
  "Face used for overdue deadlines."
  :group '+org-project)

(defun +org-project--save-buffer-no-hooks ()
  "Save the current buffer without org-heavy save hooks."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (write-region (point-min) (point-max) buffer-file-name nil 0)
  (set-visited-file-modtime)
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

(defvar +org-project--capture-context-override nil
  "Dynamic override for the next project capture context.")

(defvar +org-project--capture-initial-override nil
  "Dynamic override for project capture initial text before `org-capture' starts.")

(defconst +org-project--capture-kinds '("task" "note")
  "Supported project capture kinds.")

(defun +org-project--prefer-candidate (preferred candidates)
  "Return CANDIDATES with PREFERRED moved to the front when present."
  (if (and preferred (member preferred candidates))
      (cons preferred (delete preferred (copy-sequence candidates)))
    candidates))

(defun +org-project-root-for-slug (slug)
  "Return the registered project root for SLUG, or nil."
  (when-let ((root (car (rassoc slug +org-project-registry))))
    (+org-project--normalize-root root)))

(defun +org-project--context (slug &optional root)
  "Return a project context plist for SLUG and optional ROOT."
  (list :slug slug
        :root root
        :file (+org-project-file-for-slug slug)
        :title (if root
                   (file-name-nondirectory (directory-file-name root))
                 slug)))

(defun +org-project-select (&optional prompt require-match initial)
  "Prompt for a project context.
PROMPT overrides the minibuffer prompt.
When REQUIRE-MATCH is non-nil, restrict selection to known project slugs.
INITIAL seeds the default slug."
  (let* ((current-root (+org-project-current-root))
         (current-slug (and current-root
                            (+org-project-slug-for-root current-root)))
         (slugs (+org-project-known-slugs))
         (default-slug (or initial current-slug))
         (ordered-slugs (+org-project--prefer-candidate
                         (or current-slug default-slug)
                         slugs))
         (prompt (or prompt "Project: "))
         (slug (if ordered-slugs
                   (completing-read prompt ordered-slugs nil require-match nil nil default-slug)
                 (read-string prompt nil nil default-slug))))
    (+org-project--context slug (+org-project-root-for-slug slug))))

(defun +org-project-select-or-detect ()
  "Return a project context plist for capture or agent updates."
  (if-let* ((root (+org-project-current-root))
            (slug (+org-project-slug-for-root root)))
      (+org-project--context slug root)
    (+org-project-select "Project: " t)))

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
            (insert (format "#+MACHINE: %s\n" (system-name))))
          (insert "\n"))
        (+org-project--ensure-section "Inbox")
        (+org-project--ensure-section "Note")
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

(defun +org-project-open-file ()
  "Open the central project file for the current or selected project."
  (interactive)
  (find-file (+org-project-file-dwim)))

(defun +org-project-consult-notes ()
  "Open one of the known central project notes via Consult.
Pin the current project's central file to the front when available."
  (interactive)
  (let* ((known-files (+org-project-known-files))
         (current-file (+org-project-current-file))
         (files (if (member current-file known-files)
                    (cons current-file
                          (cl-remove current-file known-files :test #'equal))
                  known-files))
         (candidates (mapcar #'abbreviate-file-name files)))
    (unless candidates
      (user-error "No project notes available"))
    (find-file
     (consult--read
      candidates
      :prompt "Project note: "
      :sort nil
      :require-match t
      :category 'file
      :state (consult--file-preview)
      :history 'file-name-history))))

(defun +org-project--todo-list-window-width ()
  "Return the body width of the window showing the current todo list buffer."
  (let ((window (or (and (eq (window-buffer (selected-window)) (current-buffer))
                         (selected-window))
                    (get-buffer-window (current-buffer))
                    (get-buffer-window (current-buffer) t))))
    (max 1 (or (and (window-live-p window)
                    (window-body-width window))
               (frame-width)))))

(defun +org-project--todo-list-padding-width (column-count)
  "Return the horizontal padding used by `tabulated-list' for COLUMN-COUNT."
  (+ 2 (* column-count tabulated-list-padding)))

(defun +org-project--todo-list-format ()
  "Return the column format for `org-project-todo-list'."
  (let* ((total-width (+org-project--todo-list-window-width))
         (project-width 22)
         (hierarchy-width 28)
         (state-width 8)
         (compact-project-width
          (max 1
               (min 18
                    (- total-width
                       state-width
                       (+org-project--todo-list-padding-width 3)
                       1))))
         (tag-width 18)
         (time-width 24)
         (full-column-count 6)
         (compact-column-count 3)
         (full-fixed-width (+ project-width
                              hierarchy-width
                              state-width
                              tag-width
                              time-width))
         (full-min-width (+ full-fixed-width
                            28
                            (+org-project--todo-list-padding-width
                             full-column-count))))
    (if (< total-width full-min-width)
        (let ((compact-action-width
               (max 1
                    (- total-width
                       compact-project-width
                       state-width
                       (+org-project--todo-list-padding-width
                        compact-column-count)))))
          (vector
           `("Project" ,compact-project-width t)
           `("State" ,state-width t)
           `("Items" ,compact-action-width t)))
      (let ((action-width
             (- total-width
                full-fixed-width
                (+org-project--todo-list-padding-width
                 full-column-count))))
        (vector
         `("Project" ,project-width t)
         `("Hierarchy" ,hierarchy-width t)
         `("State" ,state-width t)
         `("Items" ,action-width t)
         `("Tag" ,tag-width t)
         `("Time" ,time-width t))))))

(defun +org-project--resolve-todo-filter (arg)
  "Resolve prefix ARG into an `org-project-todo-list' keyword filter."
  (when (and (stringp arg)
             (not (string-match-p "\\S-" arg)))
    (setq arg nil))
  (let* ((completion-ignore-case t)
         (todo-keywords (or (and (boundp 'org-todo-keywords-for-agenda)
                                 org-todo-keywords-for-agenda)
                            org-not-done-keywords))
         filter)
    (setq filter
          (cond ((stringp arg) arg)
                ((and (integerp arg) (> arg 0))
                 (nth (1- arg) todo-keywords))))
    (when (equal arg '(4))
      (setq filter
            (mapconcat
             #'identity
             (let ((crm-separator "|"))
               (completing-read-multiple
                "Keyword (or KWD1|KWD2|...): "
                (mapcar #'list todo-keywords)
                nil
                t))
             "|")))
    (when (equal arg 0)
      (setq filter nil))
    filter))

(defun +org-project--todo-filter-label (filter)
  "Return a display label for TODO FILTER."
  (if (stringp filter)
      filter
    "ALL"))

(defun +org-project--todo-matches-filter-p (todo-state filter)
  "Return non-nil when TODO-STATE matches FILTER."
  (or (null filter)
      (member todo-state (split-string filter "|" t "[[:space:]]*"))))

(defun +org-project--todo-heading-has-children-p ()
  "Return non-nil when the current heading has child headings."
  (save-excursion
    (let ((level (org-outline-level))
          (subtree-end (save-excursion (org-end-of-subtree t t))))
      (forward-line 1)
      (catch 'found
        (while (re-search-forward org-outline-regexp-bol subtree-end t)
          (when (> (org-outline-level) level)
            (throw 'found t)))
        nil))))

(defun +org-project--action-item-p (&optional filter bucket)
  "Return non-nil when the current heading is a leaf action item.
Optional FILTER limits the result to matching TODO keywords."
  (let ((todo-state (org-get-todo-state))
        (level (org-outline-level)))
    (and todo-state
         (or (> level 1)
             (eq bucket 'non-project))
         (not (member todo-state org-done-keywords))
         (not (+org-project-in-archive-p))
         (+org-project--todo-matches-filter-p todo-state filter)
         (not (+org-project--todo-heading-has-children-p)))))

(defun +org-project--project-title (&optional fallback-file)
  "Return the current project title, falling back to FALLBACK-FILE."
  (save-excursion
    (goto-char (point-min))
    (let ((title (when (re-search-forward "^#\\+title:[ \t]*\\(.+\\)$" nil t)
                   (string-trim (match-string-no-properties 1)))))
      (if (and (stringp title)
               (not (string-empty-p title)))
          title
        (file-name-base (or fallback-file
                            (buffer-file-name)
                            "project"))))))

(defun +org-project--parent-headings ()
  "Return parent headings for the current entry from top to bottom."
  (let (parents)
    (save-excursion
      (while (org-up-heading-safe)
        (push (substring-no-properties (org-get-heading t t t t)) parents)))
    parents))

(defun +org-project--parent-hierarchy ()
  "Return the parent heading path of the current entry."
  (string-join (+org-project--parent-headings) " / "))

(defun +org-project--entry-context (file bucket)
  "Return display context plist for the current entry in FILE and BUCKET."
  (let* ((parents (+org-project--parent-headings))
         (project (+org-project--project-title file))
         (hierarchy (string-join parents " / ")))
    (if (eq bucket 'non-project)
        (list :project ""
              :hierarchy hierarchy
              :sort-project (expand-file-name file)
              :sort-hierarchy (format "%020d" (point)))
      (list :project project
            :hierarchy hierarchy
            :sort-project project
            :sort-hierarchy hierarchy))))

(defun +org-project--entry-deadline ()
  "Return the current heading deadline as YYYY-MM-DD, or an empty string."
  (when-let ((deadline (org-entry-get (point) "DEADLINE")))
    (format-time-string "%Y-%m-%d" (org-time-string-to-time deadline))))

(defun +org-project--format-org-time-string (time-string)
  "Return TIME-STRING as a normalized display string."
  (when (and (stringp time-string)
             (not (string-empty-p time-string)))
    (let ((fmt (if (string-match-p "[0-9]\\{1,2\\}:[0-9]\\{2\\}" time-string)
                   "%Y-%m-%d %H:%M"
                 "%Y-%m-%d")))
      (format-time-string fmt (org-time-string-to-time time-string)))))

(defun +org-project--entry-time ()
  "Return the current heading planning info as a display string."
  (let (parts)
    (when-let ((deadline (+org-project--format-org-time-string
                          (org-entry-get (point) "DEADLINE"))))
      (push (concat "D:" deadline) parts))
    (when-let ((scheduled (+org-project--format-org-time-string
                           (org-entry-get (point) "SCHEDULED"))))
      (push (concat "S:" scheduled) parts))
    (when-let ((timestamp (+org-project--format-org-time-string
                           (or (org-entry-get (point) "TIMESTAMP")
                               (org-entry-get (point) "TIMESTAMP_IA")))))
      (push (concat "T:" timestamp) parts))
    (string-join (nreverse parts) "  ")))

(defun +org-project--entry-tags ()
  "Return the current heading tags as a display string."
  (let* ((origin (or (org-entry-get-with-inheritance "ORIGIN") ""))
         (tags (delete-dups
                (mapcar #'substring-no-properties
                        (org-get-tags)))))
    (cond
     ((or (string-match-p "human" origin)
          (member "human" tags)
          (member "captured" tags))
      "human")
     ((or (string-match-p "agent\\|ai" origin)
          (member "agent" tags)
          (member "ai" tags))
      "agent")
     (t ""))))

(defun +org-project--collect-file-action-items (file &optional filter bucket)
  "Collect leaf action items from FILE matching FILTER in BUCKET."
  (let ((buffer (find-file-noselect file))
        items)
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((bucket (or bucket 'project)))
            (while (re-search-forward org-outline-regexp-bol nil t)
              (goto-char (match-beginning 0))
              (when (+org-project--action-item-p filter bucket)
                (let ((context (+org-project--entry-context file bucket)))
                  (push (list :marker (copy-marker (point))
                              :bucket bucket
                              :bucket-rank (if (eq bucket 'non-project) 0 1)
                              :project (plist-get context :project)
                              :hierarchy (plist-get context :hierarchy)
                              :sort-project (plist-get context :sort-project)
                              :sort-hierarchy (plist-get context :sort-hierarchy)
                              :state (substring-no-properties
                                      (org-get-todo-state))
                              :action (substring-no-properties
                                       (org-get-heading t t t t))
                              :tags (+org-project--entry-tags)
                              :deadline (or (+org-project--entry-deadline) "")
                              :time (+org-project--entry-time))
                        items)))
              (forward-line 1))))))
    items))

(defun +org-project--todo-entry< (left right)
  "Return non-nil when LEFT should sort before RIGHT."
  (let ((left-rank (or (plist-get left :bucket-rank) 1))
        (right-rank (or (plist-get right :bucket-rank) 1)))
    (if (/= left-rank right-rank)
        (< left-rank right-rank)
      (catch 'result
        (dolist (key '(:sort-project :sort-hierarchy :state :action :tags :time))
          (let ((left-value (or (plist-get left key) ""))
                (right-value (or (plist-get right key) "")))
            (unless (string= left-value right-value)
              (throw 'result (string< left-value right-value)))))
        nil))))

(defun +org-project--action-face (state)
  "Return the face used for a leaf action item in STATE."
  (pcase state
    ("WAIT" 'org-project-todo-list-action-wait-face)
    ("PROJ" 'org-project-todo-list-action-project-face)
    (_ 'org-project-todo-list-action-face)))

(defun +org-project--deadline-face (deadline)
  "Return the face used for DEADLINE."
  (if (string-empty-p deadline)
      'org-project-todo-list-deadline-face
    (let ((days-left (floor (/ (float-time
                                (time-subtract
                                 (org-time-string-to-time deadline)
                                 (current-time)))
                               86400.0))))
      (cond ((< days-left 0) 'org-project-todo-list-deadline-overdue-face)
            ((<= days-left 7) 'org-project-todo-list-deadline-soon-face)
            (t 'org-project-todo-list-deadline-face)))))

(defun +org-project--time-face (item)
  "Return the face used for ITEM's time column."
  (+org-project--deadline-face (or (plist-get item :deadline) "")))

(defun +org-project--tabulated-cell (text width &optional face prefix &rest properties)
  "Return TEXT truncated to WIDTH, optionally propertized with FACE, PREFIX and PROPERTIES."
  (let* ((raw (concat (or prefix "") (or text "")))
         (cell (truncate-string-to-width raw width nil nil "…")))
    (apply #'propertize cell
           'face face
           'help-echo raw
           properties)))

(defun +org-project--todo-list-help-message ()
  "Return the current help string for `org-project-todo-list'."
  (if org-project-todo-list--edit-marker
      "Editing item: C-c C-c apply, C-c C-k cancel"
    "RET open, o other-window, i edit, C-c C-t todo, C-c C-q tags, C-c C-a archive, g refresh"))

(defun +org-project--update-header-line ()
  "Refresh the header line for `org-project-todo-list'."
  (setq header-line-format
        (list org-project-todo-list--header-columns
              (propertize
               (concat "    " (+org-project--todo-list-help-message))
               'face 'shadow))))

(defun +org-project--visit-marker (marker &optional other-window)
  "Visit MARKER in its original Org file.
When OTHER-WINDOW is non-nil, display it in another window."
  (when org-project-todo-list--edit-marker
    (user-error "Finish editing first with C-c C-c or C-c C-k"))
  (unless (markerp marker)
    (user-error "No project action item on this line"))
  (if other-window
      (pop-to-buffer (marker-buffer marker))
    (pop-to-buffer-same-window (marker-buffer marker)))
  (goto-char marker)
  (org-back-to-heading t)
  (org-fold-show-context 'agenda)
  (org-fold-show-entry)
  (org-reveal))

(defun org-project-todo-list-open-at-point ()
  "Open the original org heading for the current row."
  (interactive)
  (+org-project--visit-marker (tabulated-list-get-id)))

(defun org-project-todo-list-open-original-file ()
  "Open the original org heading for the current row in another window."
  (interactive)
  (+org-project--visit-marker (tabulated-list-get-id) t))

(defun +org-project--tabulated-widths ()
  "Return the current column widths for `org-project-todo-list'."
  (if (= (length tabulated-list-format) 3)
      (list :compact t
            :project (nth 1 (aref tabulated-list-format 0))
            :state (nth 1 (aref tabulated-list-format 1))
            :action (nth 1 (aref tabulated-list-format 2)))
    (list :compact nil
          :project (nth 1 (aref tabulated-list-format 0))
          :hierarchy (nth 1 (aref tabulated-list-format 1))
          :state (nth 1 (aref tabulated-list-format 2))
          :action (nth 1 (aref tabulated-list-format 3))
          :tags (nth 1 (aref tabulated-list-format 4))
          :time (nth 1 (aref tabulated-list-format 5)))))

(defun +org-project--todo-list-maybe-rerender (&rest _)
  "Re-render the todo list when the display width changes."
  (when (derived-mode-p 'org-project-todo-list-mode)
    (let ((width (+org-project--todo-list-window-width)))
      (unless (or org-project-todo-list--edit-marker
                  (equal width org-project-todo-list--render-width))
        (+org-project--render-todo-list)))))

(defun +org-project--agenda-non-project-files ()
  "Return non-project agenda files for `org-project-todo-list'."
  (let* ((project-files (mapcar #'expand-file-name (+org-project-known-files)))
         (agenda-files (when (fboundp 'org-agenda-files)
                         (org-agenda-files nil 'ifmode))))
    (sort
     (delete-dups
      (delq nil
            (mapcar (lambda (file)
                      (when (and (stringp file)
                                 (file-exists-p file))
                        (let ((expanded (expand-file-name file)))
                          (unless (or (member expanded project-files)
                                      (+org-project-file-p expanded))
                            expanded))))
                    agenda-files)))
     #'string<)))

(defun +org-project--marker-source (marker)
  "Return a stable source identifier for MARKER."
  (when (markerp marker)
    (list (buffer-file-name (marker-buffer marker))
          (marker-position marker))))

(defun +org-project--goto-source (source)
  "Move point to the row matching SOURCE.
SOURCE should be produced by `+org-project--marker-source'."
  (when source
    (goto-char (point-min))
    (catch 'found
      (while (< (point) (point-max))
        (let ((marker (tabulated-list-get-id)))
          (when (equal (+org-project--marker-source marker) source)
            (throw 'found t)))
        (forward-line 1))
      nil)))

(defun +org-project--action-cell-bounds ()
  "Return the editable action cell bounds for the current row."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (start (text-property-any bol eol 'org-project-todo-list-column 'action)))
    (when start
      (cons start
            (or (next-single-property-change start 'org-project-todo-list-column nil eol)
                eol)))))

(defun +org-project--cleanup-edit-state ()
  "Reset local edit state for `org-project-todo-list'."
  (when (overlayp org-project-todo-list--edit-overlay)
    (delete-overlay org-project-todo-list--edit-overlay))
  (dolist (overlay org-project-todo-list--edit-guards)
    (when (overlayp overlay)
      (delete-overlay overlay)))
  (when (markerp org-project-todo-list--edit-start)
    (set-marker org-project-todo-list--edit-start nil))
  (when (markerp org-project-todo-list--edit-end)
    (set-marker org-project-todo-list--edit-end nil))
  (setq-local org-project-todo-list--edit-marker nil)
  (setq-local org-project-todo-list--edit-original nil)
  (setq-local org-project-todo-list--edit-overlay nil)
  (setq-local org-project-todo-list--edit-start nil)
  (setq-local org-project-todo-list--edit-end nil)
  (setq-local org-project-todo-list--edit-guards nil)
  (+org-project--update-header-line))

(defun org-project-todo-list-edit-action-item ()
  "Edit the current row's action item in place."
  (interactive)
  (when org-project-todo-list--edit-marker
    (user-error "Already editing; use C-c C-c or C-c C-k"))
  (let* ((marker (tabulated-list-get-id))
         (bounds (+org-project--action-cell-bounds))
         (start (car bounds))
         (end (cdr bounds))
         (state (and start (get-text-property start 'org-project-todo-list-state)))
         (raw (and start (get-text-property start 'org-project-todo-list-value))))
    (unless (and (markerp marker) start end raw)
      (user-error "No editable action item on this line"))
    (setq-local org-project-todo-list--edit-marker marker)
    (setq-local org-project-todo-list--edit-original raw)
    (let ((inhibit-read-only t))
      (setq buffer-read-only nil)
      (goto-char start)
      (delete-region start end)
      (insert (propertize raw
                          'face (+org-project--action-face state)
                          'help-echo raw
                          'org-project-todo-list-column 'action
                          'org-project-todo-list-state state
                          'org-project-todo-list-value raw))
      (setq-local org-project-todo-list--edit-start
                  (copy-marker start nil))
      (setq-local org-project-todo-list--edit-end
                  (copy-marker (+ start (length raw)) t))
      (setq-local org-project-todo-list--edit-overlay
                  (make-overlay start (+ start (length raw)) nil t t))
      (setq-local org-project-todo-list--edit-guards
                  (list (let ((overlay (make-overlay (point-min) start nil nil nil)))
                          (overlay-put overlay 'read-only t)
                          overlay)
                        (let ((overlay (make-overlay (+ start (length raw)) (point-max) nil t nil)))
                          (overlay-put overlay 'read-only t)
                          overlay))))
    (goto-char start)
    (when (fboundp 'evil-insert-state)
      (evil-insert-state))
    (+org-project--update-header-line)
    (message "Editing action item. C-c C-c to save, C-c C-k to cancel.")))

(defun org-project-todo-list-commit-edit ()
  "Apply the current in-place edit to the original project file."
  (interactive)
  (unless org-project-todo-list--edit-marker
    (user-error "No active edit"))
  (let* ((marker org-project-todo-list--edit-marker)
         (source (+org-project--marker-source marker))
         (updated (and (markerp org-project-todo-list--edit-start)
                       (markerp org-project-todo-list--edit-end)
                       (buffer-substring-no-properties
                        org-project-todo-list--edit-start
                        org-project-todo-list--edit-end))))
    (unless (and updated (not (string-empty-p (string-trim updated))))
      (user-error "Action item cannot be empty"))
    (org-with-point-at marker
      (org-back-to-heading t)
      (org-edit-headline updated)
      (with-current-buffer (marker-buffer marker)
        (+org-project--save-buffer-no-hooks)))
    (+org-project--cleanup-edit-state)
    (when (fboundp 'evil-emacs-state)
      (evil-emacs-state))
    (+org-project--render-todo-list)
    (+org-project--goto-source source)
    (message "Updated action item")))

(defun org-project-todo-list-cancel-edit ()
  "Cancel the current in-place edit."
  (interactive)
  (unless org-project-todo-list--edit-marker
    (user-error "No active edit"))
  (let ((source (+org-project--marker-source org-project-todo-list--edit-marker)))
    (+org-project--cleanup-edit-state)
    (when (fboundp 'evil-emacs-state)
      (evil-emacs-state))
    (+org-project--render-todo-list)
    (+org-project--goto-source source)
    (message "Canceled action item edit")))

(defun org-project-todo-list-toggle-state (&optional arg)
  "Toggle the TODO state for the current action item.
With optional ARG, pass it through to `org-todo'."
  (interactive "P")
  (when org-project-todo-list--edit-marker
    (user-error "Finish editing first with C-c C-c or C-c C-k"))
  (let* ((marker (tabulated-list-get-id))
         (source (+org-project--marker-source marker))
         next-state)
    (unless (markerp marker)
      (user-error "No action item on this line"))
    (org-with-point-at marker
      (org-back-to-heading t)
      (org-todo arg)
      (setq next-state (org-get-todo-state))
      (with-current-buffer (marker-buffer marker)
        (+org-project--save-buffer-no-hooks)))
    (+org-project--render-todo-list)
    (unless (+org-project--goto-source source)
      (goto-char (point-min)))
    (message "TODO state: %s" (or next-state "done"))))

(defun org-project-todo-list-set-tags (&optional arg)
  "Set tags on the current action item using Org's tag UI.
With optional ARG, pass it through as `current-prefix-arg'."
  (interactive "P")
  (when org-project-todo-list--edit-marker
    (user-error "Finish editing first with C-c C-c or C-c C-k"))
  (let* ((marker (tabulated-list-get-id))
         (source (+org-project--marker-source marker))
         (current-prefix-arg arg))
    (unless (markerp marker)
      (user-error "No action item on this line"))
    (org-with-point-at marker
      (org-back-to-heading t)
      (call-interactively #'org-set-tags-command)
      (with-current-buffer (marker-buffer marker)
        (+org-project--save-buffer-no-hooks)))
    (+org-project--render-todo-list)
    (unless (+org-project--goto-source source)
      (goto-char (point-min)))
    (message "Updated tags")))

(defun org-project-todo-list-archive ()
  "Archive the current action item into the project's Archive section."
  (interactive)
  (when org-project-todo-list--edit-marker
    (user-error "Finish editing first with C-c C-c or C-c C-k"))
  (let* ((marker (tabulated-list-get-id))
         (source (+org-project--marker-source marker)))
    (unless (markerp marker)
      (user-error "No action item on this line"))
    (org-with-point-at marker
      (if (+org-project-file-p)
          (+org-project-archive-task "todo-list")
        (org-archive-subtree))
      (with-current-buffer (marker-buffer marker)
        (+org-project--save-buffer-no-hooks)))
    (+org-project--render-todo-list)
    (unless (+org-project--goto-source source)
      (goto-char (point-min)))
    (message "Archived action item")))

(defun +org-project--collect-action-items (&optional filter)
  "Collect and sort leaf action items matching FILTER."
  (let (items)
    (dolist (file (+org-project--agenda-non-project-files))
      (setq items
            (nconc items (+org-project--collect-file-action-items
                          file filter 'non-project))))
    (dolist (file (+org-project-known-files))
      (setq items
            (nconc items (+org-project--collect-file-action-items
                          file filter 'project))))
    (sort items #'+org-project--todo-entry<)))

(defun +org-project--todo-list-entries ()
  "Return tabulated entries for `org-project-todo-list'."
  (let* ((widths (+org-project--tabulated-widths))
         (compact (plist-get widths :compact))
         (project-width (plist-get widths :project))
         (hierarchy-width (plist-get widths :hierarchy))
         (state-width (plist-get widths :state))
         (action-width (plist-get widths :action))
         (tag-width (plist-get widths :tags))
         (time-width (plist-get widths :time)))
    (mapcar
     (lambda (item)
       (let ((state (plist-get item :state)))
         (list
          (plist-get item :marker)
          (if compact
              (vector
               (+org-project--tabulated-cell
                (plist-get item :project)
                project-width
                'org-project-todo-list-project-face)
               (+org-project--tabulated-cell
                state
                state-width
                (org-get-todo-face state))
               (+org-project--tabulated-cell
                (plist-get item :action)
                action-width
                (+org-project--action-face state)
                "> "
                'org-project-todo-list-column 'action
                'org-project-todo-list-state state
                'org-project-todo-list-value (plist-get item :action)))
            (vector
             (+org-project--tabulated-cell
              (plist-get item :project)
              project-width
              'org-project-todo-list-project-face)
             (+org-project--tabulated-cell
              (plist-get item :hierarchy)
              hierarchy-width
              'org-project-todo-list-hierarchy-face)
             (+org-project--tabulated-cell
              state
              state-width
              (org-get-todo-face state))
             (+org-project--tabulated-cell
              (plist-get item :action)
              action-width
              (+org-project--action-face state)
              "> "
              'org-project-todo-list-column 'action
              'org-project-todo-list-state state
              'org-project-todo-list-value (plist-get item :action))
             (+org-project--tabulated-cell
              (plist-get item :tags)
              tag-width
              'org-project-todo-list-tag-face)
             (+org-project--tabulated-cell
              (plist-get item :time)
              time-width
              (+org-project--time-face item)))))))
     (+org-project--collect-action-items org-project-todo-list--keyword-filter))))

(defun +org-project--render-todo-list (&optional _ignore-auto _noconfirm)
  "Render the current `org-project-todo-list' buffer."
  (let ((inhibit-read-only t))
    (setq tabulated-list-format (+org-project--todo-list-format))
    (setq tabulated-list-entries (+org-project--todo-list-entries))
    (setq-local org-project-todo-list--render-width
                (+org-project--todo-list-window-width))
    (tabulated-list-init-header)
    (setq-local org-project-todo-list--header-columns header-line-format)
    (+org-project--update-header-line)
    (tabulated-list-print t)
    (setq buffer-read-only t)))

(defun org-project-todo-list-refresh ()
  "Refresh the current `org-project-todo-list' buffer."
  (interactive)
  (unless (derived-mode-p 'org-project-todo-list-mode)
    (user-error "Current buffer is not an org-project todo list"))
  (when org-project-todo-list--edit-marker
    (user-error "Finish editing first with C-c C-c or C-c C-k"))
  (+org-project--render-todo-list)
  (when (called-interactively-p 'interactive)
    (message "%s" (+org-project--todo-list-help-message))))

(defun org-project-todo-list-visit ()
  "Visit the org heading on the current line."
  (interactive)
  (org-project-todo-list-open-at-point))

(define-derived-mode org-project-todo-list-mode tabulated-list-mode "Org Project TODOs"
  "Major mode for browsing leaf agenda action items."
  (setq truncate-lines t)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-format (+org-project--todo-list-format))
  (setq tabulated-list-entries nil)
  (setq-local revert-buffer-function #'+org-project--render-todo-list)
  (add-hook 'window-configuration-change-hook
            #'+org-project--todo-list-maybe-rerender nil t)
  (tabulated-list-init-header))

(define-key org-project-todo-list-mode-map (kbd "RET") #'org-project-todo-list-visit)
(define-key org-project-todo-list-mode-map (kbd "i") #'org-project-todo-list-edit-action-item)
(define-key org-project-todo-list-mode-map (kbd "g") #'org-project-todo-list-refresh)
(define-key org-project-todo-list-mode-map (kbd "o") #'org-project-todo-list-open-original-file)
(define-key org-project-todo-list-mode-map (kbd "C-c C-c") #'org-project-todo-list-commit-edit)
(define-key org-project-todo-list-mode-map (kbd "C-c C-a") #'org-project-todo-list-archive)
(define-key org-project-todo-list-mode-map (kbd "C-c C-k") #'org-project-todo-list-cancel-edit)
(define-key org-project-todo-list-mode-map (kbd "C-c C-q") #'org-project-todo-list-set-tags)
(define-key org-project-todo-list-mode-map (kbd "C-c C-t") #'org-project-todo-list-toggle-state)

(defun org-project-todo-list (&optional arg)
  "Show all leaf action items from agenda files.

Non-project agenda items are listed first.  With prefix ARG, filter by
TODO keyword like `org-todo-list'."
  (interactive "P")
  (let* ((filter (+org-project--resolve-todo-filter arg))
         (buffer (get-buffer-create org-project-todo-list-buffer-name))
         (files (append (+org-project--agenda-non-project-files)
                        (+org-project-known-files))))
    (unless files
      (user-error "No agenda files available for org-project-todo-list"))
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (org-project-todo-list-mode)
      (setq-local org-project-todo-list--keyword-filter filter)
      (setq-local mode-name
                  (format "Org Project TODOs[%s, leaf]"
                          (+org-project--todo-filter-label filter)))
      (+org-project--render-todo-list))
    (message "%s" (+org-project--todo-list-help-message))))

(defun +org-project--goto-section-heading (title)
  "Move point to the heading line for section TITLE."
  (goto-char (+org-project--ensure-section title))
  (org-back-to-heading t))

(defun +org-project--goto-inbox ()
  "Move point to the Inbox heading for child capture insertion."
  (+org-project--goto-section-heading "Inbox"))

(defun +org-project--goto-note ()
  "Move point to the Note heading for child capture insertion."
  (+org-project--goto-section-heading "Note"))

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
    (or (string= (org-get-heading t t t t) "Archive")
        (save-excursion
          (catch 'found
            (while (org-up-heading-safe)
              (when (string= (org-get-heading t t t t) "Archive")
                (throw 'found t)))
            nil)))))

(defun +org-project--capture-target (kind goto-fn)
  "Visit the right project org file for KIND and move point with GOTO-FN."
  (let* ((context (or +org-project--capture-context-override
                      (+org-project-select-or-detect)))
         (file (+org-project-ensure-file
                (plist-get context :file)
                (plist-get context :title)
                (plist-get context :root)
                (plist-get context :slug))))
    (org-capture-put :project-context context)
    (org-capture-put :project-capture-kind kind)
    (set-buffer (find-file-noselect file))
    (widen)
    (funcall goto-fn)))

(defun +org-project-task-capture-target ()
  "Visit the right project org file and move point to Inbox."
  (+org-project--capture-target 'task #'+org-project--goto-inbox))

(defun +org-project-note-capture-target ()
  "Visit the right project org file and move point to Note."
  (+org-project--capture-target 'note #'+org-project--goto-note))

(defun +org-project--put-capture-metadata (context)
  "Write shared project capture metadata for CONTEXT at point."
  (+org-project--set-property "PROJECT" (plist-get context :slug))
  (+org-project--set-property "PROJECT_FILE" (plist-get context :file))
  (when-let ((root (plist-get context :root)))
    (+org-project--set-property "PROJECT_ROOT" root))
  (+org-project--set-property "MACHINE" (system-name))
  (+org-project--set-property "CREATED_AT" (+org-project--inactive-timestamp)))

(defun +org-project-task-capture-prepare-finalize ()
  "Normalize a captured project task before finalization."
  (when-let* ((context (org-capture-get :project-context 'local))
              (point (+org-project--capture-entry-point)))
    (save-excursion
      (goto-char point)
      (let ((task-id (org-id-get-create)))
        (+org-project--ensure-tags +org-project--capture-tags)
        (+org-project--set-property "ORIGIN" "human-capture")
        (+org-project--put-capture-metadata context)
        (+org-project--set-property "TASK_ID" task-id)
        (org-capture-put :project-task-id task-id)
        (org-capture-put :project-task-title (org-get-heading t t t t))))))

(defun +org-project--note-heading-components ()
  "Return the current note heading as (TIMESTAMP TITLE), or nil when it does not match."
  (when-let ((heading (org-get-heading t t t t)))
    (when (string-match
           (rx string-start
               (group "[" (+? (not (any "]"))) "]")
               (* blank)
               (group (* nonl))
               string-end)
           heading)
      (list (match-string 1 heading)
            (string-trim (match-string 2 heading))))))

(defun +org-project--first-content-line (text)
  "Return the first non-empty line from TEXT, or nil."
  (cl-find-if (lambda (line)
                (not (string-empty-p (string-trim line))))
              (split-string text "\n" nil)))

(defun +org-project--normalize-note-title (line)
  "Normalize note title text from LINE."
  (let ((title (replace-regexp-in-string (rx string-start (+ "*") (+ blank)) "" line)))
    (string-trim title)))

(defun +org-project--remove-first-content-line (text)
  "Return TEXT without its first non-empty line."
  (let ((removed nil))
    (string-join
     (cl-loop for line in (split-string text "\n" nil)
              unless (and (not removed)
                          (not (string-empty-p (string-trim line)))
                          (prog1 t (setq removed t)))
              collect line)
     "\n")))

(defun +org-project--capture-initial-text ()
  "Return the current org-capture initial text."
  (let ((initial (or +org-project--capture-initial-override
                     (plist-get org-store-link-plist :initial)
                     (org-capture-get :initial)
                     "")))
    (if (stringp initial)
        (org-no-properties initial)
      "")))

(defun +org-project--active-region-text ()
  "Return the current active region text, or nil."
  (when (+region-active-p)
    (buffer-substring-no-properties (+region-beginning) (+region-end))))

(defun +org-project--capture-initial-todo-heading ()
  "Return the normalized first line when initial content starts with an Org TODO heading."
  (when-let* ((first-line (+org-project--first-content-line
                           (+org-project--capture-initial-text)))
              (normalized (+org-project--normalize-note-title first-line)))
    (when (with-temp-buffer
            (org-mode)
            (insert "* " normalized "\n")
            (goto-char (point-min))
            (org-back-to-heading t)
            (org-get-todo-state))
      normalized)))

(defun +org-project-default-capture-kind ()
  "Return the preferred project capture kind for the current initial content."
  (if (+org-project--capture-initial-todo-heading) 'task 'note))

(defun +org-project-task-capture-heading ()
  "Return the rendered heading text for a captured project task."
  (or (+org-project--capture-initial-todo-heading)
      "TODO "))

(defun +org-project-task-capture-body ()
  "Return the rendered body text for a captured project task."
  (let ((initial (+org-project--capture-initial-text)))
    (if (+org-project--capture-initial-todo-heading)
        (replace-regexp-in-string
         (rx string-start (* (or blank "\n")))
         ""
         (+org-project--remove-first-content-line initial))
      initial)))

(defun +org-project--split-note-initial ()
  "Return note capture initial content as (TITLE BODY)."
  (let* ((initial (+org-project--capture-initial-text))
         (first-line (+org-project--first-content-line initial))
         (title (and first-line
                     (+org-project--normalize-note-title first-line)))
         (body (replace-regexp-in-string
                (rx string-start (* (or blank "\n")))
                ""
                (+org-project--remove-first-content-line initial))))
    (list (or title "")
          (if (string-empty-p body) "" body))))

(defun +org-project-note-capture-heading ()
  "Return the rendered heading text for a captured project note."
  (pcase-let ((`(,title ,_) (+org-project--split-note-initial)))
    (if (string-empty-p title)
        (+org-project--inactive-timestamp)
      (format "%s %s" (+org-project--inactive-timestamp) title))))

(defun +org-project-note-capture-body ()
  "Return the rendered body text for a captured project note."
  (pcase-let ((`(_ ,body) (+org-project--split-note-initial)))
    body))

(defun +org-project-note-capture-prepare-finalize ()
  "Normalize a captured project note before finalization."
  (when-let ((point (+org-project--capture-entry-point)))
    (save-excursion
      (goto-char point)
      (pcase-let ((`(,timestamp ,title) (or (+org-project--note-heading-components)
                                            (list nil nil))))
        (when (and timestamp (string-empty-p title))
          (let* ((body-beg (save-excursion
                             (forward-line 1)
                             (point)))
                 (body-end (save-excursion
                             (org-end-of-subtree t t)
                             (point)))
                 (body (buffer-substring-no-properties body-beg body-end))
                 (first-line (+org-project--first-content-line body))
                 (normalized-title (and first-line
                                        (+org-project--normalize-note-title first-line))))
            (when (and normalized-title
                       (not (string-empty-p normalized-title)))
              (org-edit-headline (concat timestamp " " normalized-title))
              (delete-region body-beg body-end)
              (let ((remaining (replace-regexp-in-string
                                (rx string-start (* (or blank "\n")))
                                ""
                                (+org-project--remove-first-content-line body))))
                (unless (string-empty-p remaining)
                  (insert remaining)
                  (unless (bolp)
                    (insert "\n")))))))))))

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
          (insert (format "** CAPTURED [%s] %s\n:PROPERTIES:\n:TASK_ID: %s\n:PROJECT: %s\n:PROJECT_FILE: %s\n:AUDIT_STATUS: active\n:CREATED_AT: %s\n:END:\n%s"
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

(defun +org-project--save-captured-project-buffer (context)
  "Save the project buffer associated with capture CONTEXT and return its file."
  (when-let ((project-file (plist-get context :file)))
    (with-current-buffer (find-file-noselect project-file)
      (+org-project--save-buffer-no-hooks))
    project-file))

(defun +org-project-task-capture-after-finalize ()
  "Finalize project task capture side effects."
  (when (and (not org-note-abort)
             (org-capture-get :project-context))
    (let* ((context (org-capture-get :project-context))
           (project-file (+org-project--save-captured-project-buffer context))
           (task-id (org-capture-get :project-task-id))
           (task-title (or (org-capture-get :project-task-title) "Captured task")))
      (when project-file
        (+org-project--refresh-id-locations (list project-file)))
      (when (and +org-capture-audit-log-enabled task-id)
        (+org-project-log-capture context task-id task-title)))))

(defun +org-project-note-capture-after-finalize ()
  "Finalize project note capture side effects."
  (when (and (not org-note-abort)
             (org-capture-get :project-context))
    (+org-project--save-captured-project-buffer
     (org-capture-get :project-context))))

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

(defun +org-project-read-capture-kind (&optional prompt)
  "Read a project capture kind using PROMPT."
  (let* ((default-kind (+org-project-default-capture-kind))
         (default-name (symbol-name default-kind))
         (ordered-kinds (+org-project--prefer-candidate
                         default-name
                         +org-project--capture-kinds)))
    (intern
     (completing-read (or prompt "Project capture: ")
                      ordered-kinds nil t nil nil default-name))))

(defun +org-project--capture-template-key (kind)
  "Return the org-capture template key for project capture KIND."
  (pcase kind
    ('task "pt")
    ('note "pn")
    (_ (user-error "Unsupported project capture kind: %S" kind))))

(defun +org-project-capture (&optional context)
  "Capture a project task or note using CONTEXT when provided."
  (interactive)
  (let ((+org-project--capture-context-override context)
        (+org-project--capture-initial-override (+org-project--active-region-text)))
    (org-capture nil
                 (+org-project--capture-template-key
                  (+org-project-read-capture-kind)))))

(defun +org-project-capture-select-project ()
  "Capture a project task or note after manually selecting the project."
  (interactive)
  (+org-project-capture (+org-project-select "Project: " t)))

(with-eval-after-load 'org-capture
  (setq org-capture-templates
        (cl-remove-if (lambda (template)
                        (member (car-safe template) '("p" "pt" "pn")))
                      org-capture-templates))
  (setq org-capture-templates
        (append
         '(("p" "Project")
           ("pt" "Project task" entry
            (function +org-project-task-capture-target)
            "** %(+org-project-task-capture-heading)%?\n%(+org-project-task-capture-body)"
            :empty-lines-before 1
            :empty-lines-after 1
            :no-save t
            :unnarrowed t
            :prepare-finalize +org-project-task-capture-prepare-finalize
            :after-finalize +org-project-task-capture-after-finalize)
           ("pn" "Project note" entry
            (function +org-project-note-capture-target)
            "** %(+org-project-note-capture-heading)%?\n%(+org-project-note-capture-body)"
            :empty-lines-before 1
            :empty-lines-after 1
            :no-save t
            :unnarrowed t
            :prepare-finalize +org-project-note-capture-prepare-finalize
            :after-finalize +org-project-note-capture-after-finalize))
         org-capture-templates)))

(add-hook 'org-mode-hook #'+org-project--configure-project-buffer-h)
(add-hook 'org-mode-hook #'+org-project--maybe-refresh-journal-audit-h)
(add-hook 'org-mode-hook #'+org-project--maybe-collapse-archive-h)

(with-eval-after-load 'org-agenda
  (+org-project-sync-agenda-files))

(provide 'init-org-project)
;;; init-org-project.el ends here
