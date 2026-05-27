;;; init-config-ebib.el --- Ebib configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'bibtex)
(require 'cl-lib)

(defcustom +ebib/org-root-dir
  (expand-file-name "~/opt/org-root")
  "Root directory containing the main bibliography file."
  :type 'directory
  :group 'ebib)

(defun +ebib/ensure-directory (dir)
  "Ensure DIR exists and return it."
  (make-directory dir t)
  dir)

(defun +ebib/all-ref-file ()
  "Return the path of the main bibliography file."
  (expand-file-name "all-ref.bib"
                    (+ebib/ensure-directory +ebib/org-root-dir)))

(use-package ebib
  :ensure t
  :custom
  (ebib-keywords-save-on-exit 'always)
  (ebib-layout 'full)
  (ebib-popup-entry-window nil)
  (ebib-width 0.5)
  (ebib-index-window-size 30)
  (ebib-window-vertical-split nil)
  (ebib-uniquify-keys t)
  (ebib-autogenerate-keys t)
  :config
  (add-to-list 'ebib-preload-bib-files
               (+ebib/all-ref-file))

  (defun +ebib/import-from-killring-and-format--current-kill (arg)
    "Return the current kill-ring entry selected by ARG."
    (current-kill (cond
                   ((listp arg)
                    (if (eq last-command '+ebib/import-from-killring-and-format)
                        1
                      0))
                   ((eq arg '-)
                    -2)
                   (t
                    (1- arg)))))

  (defun +ebib/import-from-killring-and-format--generate-autokey (key db)
    "Generate an Ebib autokey for KEY in DB."
    (with-temp-buffer
      ;; Sort fields so BibTeX consistently prefers the author field.
      (ebib--format-entry key db nil 'sort)
      (let ((x-ref (or (ebib-get-field-value "xdata" key db 'noerror 'unbraced)
                       (ebib-get-field-value "crossref" key db 'noerror 'unbraced))))
        (when x-ref
          (ebib--format-entry x-ref db nil 'sort)))
      (goto-char (point-min))
      (bibtex-set-dialect (ebib--get-dialect db) 'local)
      (bibtex-generate-autokey)))

  (defun +ebib/import-from-killring-and-format--load-db (file)
    "Return the Ebib database for FILE, loading it if needed."
    (let* ((file (expand-file-name file))
           (db (cl-find-if (lambda (candidate)
                             (let ((candidate-file (ebib-db-get-filename candidate)))
                               (and candidate-file
                                    (string= file (expand-file-name candidate-file)))))
                           ebib--databases)))
      (or db
          (let ((db (ebib--create-new-database))
                (ebib--log-error nil))
            (ebib-db-set-filename file db)
            (when (file-exists-p file)
              (ebib--bib-read-entries file db)
              (ebib-db-set-backup t db)
              (ebib-db-set-modified nil db))
            db))))

  (defun +ebib/import-from-killring-and-format--ensure-log-buffer ()
    "Ensure Ebib's log buffer exists even if Ebib was not initialized."
    (unless (assq 'log ebib--buffer-alist)
      (push (cons 'log (get-buffer-create "*Ebib-log*")) ebib--buffer-alist)))

  (defun +ebib/import-from-killring-and-format--save-db (db)
    "Save DB without depending on Ebib's window state."
    (when (and (ebib-db-backup-p db)
               (file-exists-p (ebib-db-get-filename db)))
      (ebib--make-backup (ebib-db-get-filename db))
      (ebib-db-set-backup nil db))
    (let ((db-modtime (ebib-db-get-modtime db))
          (file-modtime (ebib--get-file-modtime (ebib-db-get-filename db))))
      (when (and db-modtime file-modtime
                 (time-less-p db-modtime file-modtime))
        (unless (yes-or-no-p (format "File `%s' changed on disk.  Overwrite? "
                                     (ebib-db-get-filename db)))
          (user-error "[Ebib] File not saved"))))
    (let ((ebib--cur-db db))
      (with-temp-buffer
        (ebib--format-database-as-bibtex db)
        (write-region (point-min) (point-max) (ebib-db-get-filename db))))
    (ebib-db-set-modified nil db))

  (defun +ebib/import-from-killring-and-format (arg)
    "Import a BibTeX item from the kill ring into `all-ref.bib' and save it.
This is similar to running `ebib-yank-entry' and `ebib-generate-autokey'
without opening Ebib first.  With prefix ARG, rotate the kill ring as
`yank' and `yank-pop' do."
    (interactive "P")
    (+ebib/import-from-killring-and-format--ensure-log-buffer)
    (let* ((db (+ebib/import-from-killring-and-format--load-db (+ebib/all-ref-file)))
           (entry (+ebib/import-from-killring-and-format--current-kill arg))
           (entry-key nil)
           (imported-kind nil)
           (autokey-generated nil))
      (with-temp-buffer
        (insert entry)
        (goto-char (point-min))
        (let ((entry-type (ebib--bib-find-next-bibtex-item)))
          (cond
           ((cl-equalp entry-type "string")
            (when (ebib--bib-read-string db)
              (setq imported-kind "@String")))
           ((cl-equalp entry-type "preamble")
            (when (ebib--bib-read-preamble db)
              (setq imported-kind "@Preamble")))
           ((cl-equalp entry-type "comment")
            (when (ebib--bib-read-comment db)
              (setq imported-kind "@Comment")))
           ((stringp entry-type)
            (setq entry-key (ebib--bib-read-entry db t))
            (unless entry-key
              (user-error "[Ebib] Could not import a valid entry from the kill ring"))
            (let ((new-key (+ebib/import-from-killring-and-format--generate-autokey entry-key db)))
              (unless (string= new-key "")
                (setq entry-key
                      (or (ebib-db-change-key entry-key
                                              new-key
                                              db
                                              (if ebib-uniquify-keys 'uniquify 'noerror))
                          entry-key))
                (setq autokey-generated t))))
           (t
            (user-error "[Ebib] No BibTeX item found in the current kill-ring entry")))))
      (unless (or entry-key imported-kind)
        (user-error "[Ebib] Could not import the current kill-ring entry"))
      (when entry-key
        (ebib-db-set-current-entry-key entry-key db))
      (ebib-db-set-modified t db)
      (+ebib/import-from-killring-and-format--save-db db)
      (ebib-db-set-modtime (ebib--get-file-modtime (ebib-db-get-filename db)) db)
      (cond
       ((not ebib--initialized))
       ((eq db ebib--cur-db)
        (ebib--update-buffers))
       (t
        (ebib--mark-index-dirty db)))
      (message (cond
                (entry-key
                 (format "[Ebib] Imported entry `%s'%s into %s"
                         entry-key
                         (if autokey-generated "" " (autokey unavailable)")
                         (abbreviate-file-name (ebib-db-get-filename db))))
                (imported-kind
                 (format "[Ebib] Imported %s into %s"
                         imported-kind
                         (abbreviate-file-name (ebib-db-get-filename db)))))))))

(provide 'init-config-ebib)
;;; init-config-ebib.el ends here
