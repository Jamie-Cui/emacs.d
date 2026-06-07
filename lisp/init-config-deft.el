;;; init-config-deft.el --- Deft support for org notes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom +deft/org-root-dir
  (expand-file-name "~/opt/org-root")
  "Root directory for Deft org notes."
  :type 'directory
  :group 'deft)

(defun +deft/ensure-directory (dir)
  "Ensure DIR exists and return it."
  (make-directory dir t)
  dir)

(defun +deft/use-default-font-h ()
  "Keep the Deft browser on the configured default font."
  (when (fboundp '+deft/apply-browser-faces)
    (+deft/apply-browser-faces))
  (setq-local buffer-face-mode-face 'default)
  (setq-local truncate-lines t)
  (when (bound-and-true-p buffer-face-mode)
    (buffer-face-mode -1)))

(defun +deft/default-font-family ()
  "Return the current default face family, or nil when unset."
  (let ((family (face-attribute 'default :family nil t)))
    (when (stringp family)
      family)))

(defun +deft/set-browser-face (face inherit)
  "Make FACE inherit colors from INHERIT while using the default font."
  (apply #'set-face-attribute
         face nil
         `(,@(when-let* ((family (+deft/default-font-family)))
               (list :family family))
           :inherit ,(append (ensure-list inherit) '(default))
           :weight regular
           :slant normal)))

(defun +deft/apply-browser-faces ()
  "Apply faces for the Deft browser."
  (+deft/set-browser-face 'deft-title-face 'font-lock-function-name-face)
  (+deft/set-browser-face 'deft-summary-face 'font-lock-comment-face)
  (+deft/set-browser-face 'deft-time-face 'font-lock-variable-name-face)
  (+deft/set-browser-face 'deft-separator-face
                          'font-lock-comment-delimiter-face)
  (when (facep '+deft/group-header-face)
    (+deft/set-browser-face '+deft/group-header-face
                            '(font-lock-keyword-face bold))))

(use-package deft
  :ensure t
  :after general
  :custom
  (deft-recursive t)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase)))
  (deft-auto-save-interval -1.0)
  (deft-use-filter-string-for-filename nil)
  (deft-use-filename-as-title nil)
  (deft-directory (+deft/ensure-directory +deft/org-root-dir))
  (deft-ignore-file-regexp "^$")
  (deft-extensions '("org"))
  :config
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                "\\)"))
  (setq deft-default-extension "org")

  (add-hook 'deft-mode-hook #'+deft/use-default-font-h)
  (add-hook 'deft-mode-hook
            (lambda ()
              (setq-local revert-buffer-function
                          (lambda (&rest _) (deft-refresh)))))

  (defcustom +deft/group-by-top-level-directory t
    "When non-nil, group Deft entries by their top-level org-root directory."
    :type 'boolean
    :group 'deft)

  (defcustom +deft/group-order
    '("journal" "roam" "deft" "projects" "external" "root")
    "Preferred top-level directory order for Deft group headers."
    :type '(repeat string)
    :group 'deft)

  (defface +deft/group-header-face
    '((t (:inherit (font-lock-keyword-face bold default))))
    "Face used for group headings in the Deft browser."
    :group 'deft)
  (+deft/apply-browser-faces)

  (defvar-local +deft/current-group nil
    "Current group section while rendering the Deft browser.")

  (defvar +deft/current-profile 'org
    "Current Deft profile.")

(defcustom +deft/new-file-location "deft"
    "Fallback directory used for newly created Deft files.
When point is inside a Deft group, new files are created under that group's
top-level directory instead. When relative, interpret this fallback directory
under `+deft/org-root-dir'."
    :type 'string
    :group 'deft)

  (defvar +deft/auto-refresh-watched-directory nil
    "Directory currently watched by Deft auto refresh.")

  (defconst +deft/profile-extensions
    '((org . ("org"))
      (tex . ("tex")))
    "Mapping from Deft profile to file extensions.")

  (defconst +deft/ignored-directory-names
    '("img" "journal" "ltximg" "pdf" "scripts" "roam")
    "Directory names excluded from unified Deft views.")

  (defconst +deft/ignored-file-regexps
    '("\\.sync-conflict-[^/]*\\.org\\'"
      "/[^/]+-beorg\\.org\\'"
      "/deft/archive/")
    "Additional file path regexps excluded from unified Deft views.")

  (defun +deft/org-root-directory ()
    "Return the normalized org root directory for Deft."
    (file-name-as-directory
     (file-truename (+deft/ensure-directory +deft/org-root-dir))))

  (defun +deft/new-file-directory ()
    "Return the normalized fallback directory for newly created Deft files."
    (let ((directory (if (file-name-absolute-p +deft/new-file-location)
                         +deft/new-file-location
                       (expand-file-name +deft/new-file-location
                                         (+deft/org-root-directory)))))
      (file-name-as-directory
       (file-truename (+deft/ensure-directory directory)))))

  (defun +deft/group-directory (group)
    "Return the normalized directory backing GROUP."
    (if (or (null group)
            (string-empty-p group)
            (string= group "root"))
        (+deft/org-root-directory)
      (file-name-as-directory
       (file-truename
        (+deft/ensure-directory
         (expand-file-name group (+deft/org-root-directory)))))))

  (defun +deft/group-at-point ()
    "Return the Deft group key at point, or nil when unavailable."
    (when (derived-mode-p 'deft-mode)
      (or (when-let* ((file (deft-filename-at-point)))
            (+deft/group-key file))
          (let ((group (or (get-text-property (point) '+deft/group)
                           (and (> (point) (point-min))
                                (get-text-property (1- (point)) '+deft/group)))))
            (or group
                (save-excursion
                  (catch 'found
                    (while (> (point) (point-min))
                      (forward-line -1)
                      (setq group (or (get-text-property (point) '+deft/group)
                                      (and (> (point) (point-min))
                                           (get-text-property (1- (point))
                                                              '+deft/group))))
                      (when group
                        (throw 'found group))))))))))

  (defun +deft/current-new-file-directory ()
    "Return the directory where a new Deft file should be created."
    (if-let* ((group (+deft/group-at-point)))
        (+deft/group-directory group)
      (+deft/new-file-directory)))

  (defun +deft/hidden-path-regexp (&optional trailing)
    "Return a regexp matching hidden paths.
When TRAILING is non-nil, also require a trailing slash."
    (concat "/\\.[^/]*"
            (if trailing
                "/"
              "\\(?:/\\|\\'\\)")))

  (defun +deft/ignored-directory-regexp ()
    "Return a regexp matching directories excluded from Deft recursion."
    (let ((named (regexp-opt +deft/ignored-directory-names)))
      (concat "\\(?:"
              (+deft/hidden-path-regexp nil)
              "\\|/\\(?:" named "\\)\\'"
              "\\|/deft/archive\\'\\)")))

  (defun +deft/ignored-file-regexp ()
    "Return a regexp matching files excluded from Deft views."
    (let ((named (regexp-opt +deft/ignored-directory-names))
          (root-readme
           (concat "\\`"
                   (regexp-quote
                    (expand-file-name "README.org" (+deft/org-root-directory)))
                   "\\'")))
      (concat "\\(?:"
              (+deft/hidden-path-regexp nil)
              "\\|/\\(?:" named "\\)/"
              "\\|" root-readme
              "\\|"
              (mapconcat #'identity +deft/ignored-file-regexps "\\|")
              "\\)")))

  (defun +deft/relative-file (file)
    "Return FILE relative to `+deft/org-root-directory'."
    (string-remove-prefix (+deft/org-root-directory)
                          (file-truename file)))

  (defun +deft/normalize-org-heading (heading)
    "Normalize org HEADING text for Deft titles."
    (let ((text (string-trim heading)))
      (setq text
            (replace-regexp-in-string
             "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" "" text))
      (string-trim (deft-strip-title text))))

  (defun +deft/parse-org-title (file contents)
    "Parse a useful Deft title for org FILE from CONTENTS."
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (let ((case-fold-search t)
            title)
        (when (re-search-forward "^#\\+title:[ \t]*\\(.+\\)$" nil t)
          (setq title (string-trim (match-string-no-properties 1))))
        (unless (and title (not (string-empty-p title)))
          (goto-char (point-min))
          (catch 'found
            (while (not (eobp))
              (cond
               ((looking-at-p "^[ \t]*$")
                (forward-line 1))
               ((looking-at-p "^:PROPERTIES:[ \t]*$")
                (if (re-search-forward "^:END:[ \t]*$" nil t)
                    (forward-line 1)
                  (goto-char (point-max))))
               ((looking-at-p "^#\\+[[:alpha:]_]+:.*$")
                (forward-line 1))
               ((looking-at "^\\*+[ \t]+\\(.*\\)$")
                (let ((heading (+deft/normalize-org-heading
                                (match-string-no-properties 1))))
                  (when (not (string-empty-p heading))
                    (setq title heading)
                    (throw 'found title)))
                (forward-line 1))
               (t
                (let ((line (string-trim
                             (deft-strip-title
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))))
                  (if (string-empty-p line)
                      (forward-line 1)
                    (setq title line)
                    (throw 'found title))))))))
        (or title
            (deft-base-filename file)))))

  (defun +deft/parse-title-a (orig-fn file contents)
    "Parse better titles for org FILE using CONTENTS, else delegate to ORIG-FN."
    (if (string= (downcase (or (file-name-extension file) "")) "org")
        (+deft/parse-org-title file contents)
      (funcall orig-fn file contents)))
  (advice-add #'deft-parse-title :around #'+deft/parse-title-a)

  (defun +deft/absolute-filename-a (orig-fn slug &optional extension)
    "Create new Deft files in `+deft/new-file-directory'."
    (let ((deft-directory (+deft/current-new-file-directory)))
      (funcall orig-fn slug extension)))
  (advice-add #'deft-absolute-filename :around #'+deft/absolute-filename-a)

  (defun +deft/group-key (file)
    "Return FILE's top-level org-root directory key."
    (let* ((relative (+deft/relative-file file))
           (segments (split-string relative "/" t)))
      (downcase
       (or (car segments)
           "root"))))

  (defun +deft/group-label (group)
    "Return display label for GROUP."
    (upcase group))

  (defun +deft/group-rank (group)
    "Return sort rank for GROUP."
    (or (cl-position group +deft/group-order :test #'string=)
        (+ (length +deft/group-order) 100)))

  (defun +deft/group-lessp (left right)
    "Return non-nil when LEFT should sort before RIGHT."
    (let ((left-rank (+deft/group-rank left))
          (right-rank (+deft/group-rank right)))
      (if (/= left-rank right-rank)
          (< left-rank right-rank)
        (string-lessp left right))))

  (defun +deft/pin-files-first (files)
    "Return FILES with pinned files moved to the front."
    (let (pinned rest)
      (dolist (file files)
        (if (member file +deft/pinned-files)
            (push file pinned)
          (push file rest)))
      (append (nreverse pinned) (nreverse rest))))

  (defun +deft/preferred-file-path (left right)
    "Return the preferred Deft path between LEFT and RIGHT."
    (let ((left-rel (+deft/relative-file left))
          (right-rel (+deft/relative-file right)))
      (cond
       ((< (length left-rel) (length right-rel)) left)
       ((> (length left-rel) (length right-rel)) right)
       ((string-lessp left-rel right-rel) left)
       (t right))))

  (defun +deft/dedupe-files (files)
    "Remove duplicate FILES that resolve to the same truename."
    (let ((choices (make-hash-table :test #'equal))
          ordered)
      (dolist (file files)
        (let* ((truth (file-truename file))
               (existing (gethash truth choices)))
          (unless existing
            (push truth ordered))
          (puthash truth
                   (if existing
                       (+deft/preferred-file-path existing file)
                     file)
                   choices)))
      (mapcar (lambda (truth)
                (gethash truth choices))
              (nreverse ordered))))

  ;; NOTE enable auto refresh
  ;; see: https://github.com/jrblevin/deft/pull/62/files
  (defvar deft-auto-refresh-descriptor nil)
  (defun deft-auto-refresh (_event)
    (deft-refresh))

  (defun +deft/update-auto-refresh-watch ()
    "Update the file notification watch for the current `deft-directory'."
    (when (fboundp 'file-notify-add-watch)
      (let ((directory (file-name-as-directory
                        (expand-file-name deft-directory))))
        (unless (equal directory +deft/auto-refresh-watched-directory)
          (when deft-auto-refresh-descriptor
            (ignore-errors
              (file-notify-rm-watch deft-auto-refresh-descriptor)))
          (setq deft-auto-refresh-descriptor
                (file-notify-add-watch
                 directory
                 '(change attribute-change)
                 'deft-auto-refresh))
          (setq +deft/auto-refresh-watched-directory directory)))))

  (+deft/update-auto-refresh-watch)

  ;; NOTE pin files and optionally group them by source directory
  (defcustom +deft/pinned-files nil
    "List of pinned file paths shown before unpinned files in Deft."
    :type '(repeat string)
    :group 'deft)

  (defconst +deft/pin-prefix "* "
    "Prefix string shown before pinned file titles.")

  (defun +deft/sort-files-a (files)
    "Pin FILES first and optionally group them by top-level directory."
    (setq files (+deft/dedupe-files files))
    (if (not +deft/group-by-top-level-directory)
        (+deft/pin-files-first files)
      (let ((buckets (make-hash-table :test #'equal))
            seen-groups
            result)
        (dolist (file files)
          (let* ((group (+deft/group-key file))
                 (bucket (or (gethash group buckets)
                             (cons nil nil))))
            (unless (member group seen-groups)
              (push group seen-groups))
            (if (member file +deft/pinned-files)
                (setcar bucket (cons file (car bucket)))
              (setcdr bucket (cons file (cdr bucket))))
            (puthash group bucket buckets)))
        (dolist (group (sort (nreverse seen-groups) #'+deft/group-lessp))
          (let ((bucket (gethash group buckets)))
            (setq result
                  (nconc result
                         (nreverse (car bucket))
                         (nreverse (cdr bucket))))))
        result)))
  (advice-add #'deft-sort-files :filter-return #'+deft/sort-files-a)

  (defun +deft/reset-group-a (orig-fn &optional refresh)
    "Reset group state before calling ORIG-FN with REFRESH."
    (+deft/use-default-font-h)
    (let ((+deft/current-group nil))
      (funcall orig-fn refresh)))
  (advice-add #'deft-buffer-setup :around #'+deft/reset-group-a)

  (defun +deft/file-button-a (_orig-fn file)
    "Insert a Deft browser row for FILE with robust time-column alignment."
    (when (and file +deft/group-by-top-level-directory)
      (let ((group (+deft/group-key file)))
        (unless (equal group +deft/current-group)
          (when +deft/current-group
            (insert "\n"))
          (setq +deft/current-group group)
          (insert (propertize (concat (+deft/group-label group) "\n")
                              'face '+deft/group-header-face
                              '+deft/group group)))))
    (when file
      (let* ((pin-prefix (if (member file +deft/pinned-files)
                             +deft/pin-prefix
                           ""))
             (full-title (concat pin-prefix
                                 (or (deft-file-title file)
                                     deft-empty-file-title)))
             (summary (deft-file-summary file))
             (mtime (when deft-time-format
                      (format-time-string deft-time-format
                                          (deft-file-mtime file))))
             (mtime-width (deft-string-width mtime))
             (line-width (max 0 (- deft-window-width
                                    mtime-width
                                    (if mtime 1 0))))
             (title-width (min line-width (deft-string-width full-title)))
             (title (truncate-string-to-width full-title title-width))
             (summary-width
              (max 0
                   (min (deft-string-width summary)
                        (- line-width
                           title-width
                           (deft-string-width deft-separator))))))
        (insert-text-button title
                            'type 'deft-button
                            'tag file)
        (when (> summary-width 0)
          (insert (propertize deft-separator 'face 'deft-separator-face))
          (insert (propertize (truncate-string-to-width summary summary-width)
                              'face 'deft-summary-face)))
        (when mtime
          (while (< (current-column) line-width)
            (insert " "))
          (insert (propertize mtime 'face 'deft-time-face)))
        (insert "\n"))))
  (advice-add #'deft-file-button :around #'+deft/file-button-a)

  (defun +deft/toggle-pin ()
    "Toggle pinning of the file at point in the Deft browser."
    (interactive)
    (let ((file (deft-filename-at-point)))
      (if (not file)
          (message "No file at point")
        (if (member file +deft/pinned-files)
            (progn
              (setq +deft/pinned-files (delete file +deft/pinned-files))
              (message "Unpinned: %s" (file-name-nondirectory file)))
          (push file +deft/pinned-files)
          (message "Pinned: %s" (file-name-nondirectory file)))
        (customize-save-variable '+deft/pinned-files +deft/pinned-files)
        (deft-refresh))))
  (define-key deft-mode-map (kbd "C-c C-p") #'+deft/toggle-pin)

  (defun +deft/apply-profile (profile)
    "Apply Deft PROFILE settings."
    (setq +deft/current-profile profile
          deft-directory (+deft/org-root-directory)
          deft-recursive t
          deft-default-extension (if (eq profile 'tex) "tex" "org")
          deft-extensions (copy-sequence
                           (or (alist-get profile +deft/profile-extensions)
                               '("org")))
          deft-recursive-ignore-dir-regexp (+deft/ignored-directory-regexp)
          deft-ignore-file-regexp (+deft/ignored-file-regexp))
    (+deft/update-auto-refresh-watch))

  (defun +deft/update-mode-name ()
    "Update `mode-name' for the current Deft profile."
    (setq mode-name
          (pcase +deft/current-profile
            ('tex "Deft[tex]")
            (_ "Deft[org]"))))

  (add-hook 'deft-mode-hook #'+deft/update-mode-name)

  (defun +deft/open-profile (profile)
    "Open Deft with PROFILE."
    (+deft/apply-profile profile)
    (switch-to-buffer deft-buffer)
    (deft-mode)
    (+deft/update-mode-name)
    (force-mode-line-update))

  (defun org-deft-org ()
    "Open the org-focused unified Deft view."
    (interactive)
    (+deft/open-profile 'org))

  (defun org-deft-tex ()
    "Open the tex-focused unified Deft view."
    (interactive)
    (+deft/open-profile 'tex)))

(provide 'init-config-deft)
;;; init-config-deft.el ends here
