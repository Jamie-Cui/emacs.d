;;; init-latex.el --- latex support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-org)
(require 'reftex)

;; -----------------------------------------------------------
;; DONE latex
;;
;; auctex
;; ebib
;; pdf-tools
;; citar
;; citar-embark
;; -----------------------------------------------------------

(use-package auctex
  :ensure t)

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
               (concat (+emacs/ensure-directory +emacs/org-root-dir) "/all-ref.bib"))

  (defun +ebib/all-ref-file ()
    "Return the path of the main bibliography file."
    (expand-file-name "all-ref.bib"
                      (+emacs/ensure-directory +emacs/org-root-dir)))

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
                         (abbreviate-file-name (ebib-db-get-filename db)))))))
  ))

(use-package pdf-tools
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (pdf-tools-install)
  :config
  ;; pdf-tools have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; Enable SyncTeX
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)

  ;; Set PDF-Tools as the default viewer
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list
        '(("PDF Tools" TeX-pdf-tools-sync-view)))

  ;; Define forward search function
  (defun TeX-pdf-tools-sync-view ()
    (require 'pdf-sync)
    (let ((pdf-file (expand-file-name (TeX-master-file "pdf")))
          (line (line-number-at-pos)))
      (pdf-sync-forward-search line)))

  ;; Enable Inverse Search in PDFs
  (setq pdf-sync-backward-search-method 'generic
        pdf-sync-generic-forward-search-command
        "emacsclient --no-wait +%l '%f'")

  (setq TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))

(use-package citar
  :ensure t
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :custom
  (citar-select-multiple nil)
  ;; (org-cite-insert-processor 'citar)
  ;; (org-cite-follow-processor 'citar)
  ;; (org-cite-activate-processor 'citar)
  :config
  (setq citar-at-point-function 'embark-act)
  (add-to-list 'citar-bibliography
               (concat (+emacs/ensure-directory +emacs/org-root-dir) "/all-ref.bib"))
  (add-to-list 'citar-library-paths
               (+emacs/org-subdir "pdf"))
  (add-to-list 'citar-notes-paths
               (+emacs/org-subdir "roam"))
  (setf (alist-get 'note citar-templates) "${=key=}")
  ;; NOTE this var is used by org-export
  (add-to-list 'org-cite-global-bibliography
               (concat (+emacs/ensure-directory +emacs/org-root-dir) "/all-ref.bib"))

  ;; Make citar-open-entry show item in ebib
  ;; NOTE only global bib works, we do not support project-wise bib
  (defun citar-open-entry-in-ebib (citekey)
    "Open entry for CITEKEY in ebib."
    ;; Adapted from 'bibtex-completion-show-entry'.
    (ebib (concat (+emacs/ensure-directory +emacs/org-root-dir) "/all-ref.bib") citekey))
  (setopt citar-open-entry-function #'citar-open-entry-in-ebib)

  ;; Extend citar-open popup with "Copy Title" and "Open in Ebib" options.
  ;; Candidates are encoded as "[Copy Title] KEY" / "[Open in Ebib] KEY" so
  ;; the key survives citar--select-resource stripping text properties.
  (defun +citar--get-resource-extras-a (orig citekeys &rest args)
    "Inject per-field copy and open-in-ebib candidates into `citar--get-resource-candidates'.
Also simplifies create-note candidates to show only the note filename."
    (let* ((orig-result (apply orig citekeys args))
           ;; Cache entries — citar-get-entry is otherwise called twice per key.
           (entry-cache (let ((tbl (make-hash-table :test #'equal :size (length citekeys))))
                          (dolist (k citekeys tbl) (puthash k (citar-get-entry k) tbl))))
           ;; Replace create-note candidates' verbose display with just the note filename.
           (result (when orig-result
                     (cons (car orig-result)
                           (mapcar (lambda (cand)
                                     (let* ((mc    (get-text-property 0 'multi-category cand))
                                            (inner (and mc (cdr mc)))
                                            (rtype (and inner (get-text-property 0 'citar--resource inner))))
                                       (if (eq rtype 'create-note)
                                           (let* ((key  (substring-no-properties inner))
                                                  (name (when-let* (((fboundp 'citar-file--get-note-filename))
                                                                      (path (citar-file--get-note-filename key)))
                                                          (file-name-nondirectory path)))
                                                  (name (or name key))
                                                  ;; Keep the hidden citekey prefix and outer resource
                                                  ;; type so `citar--select-resource' and Embark can
                                                  ;; still dispatch create-note correctly.
                                                  (display (citar--prepend-candidate-citekey key name)))
                                             (propertize display
                                                         'citar--resource rtype
                                                         'multi-category mc))
                                         cand)))
                                   (cdr orig-result)))))
           (extra-cands
            (mapcan (lambda (key)
                      (let* ((entry   (gethash key entry-cache))
                             (fields  (seq-remove (lambda (c) (string-prefix-p "=" (car c))) entry))
                             (max-len (apply #'max (length "key")
                                            (mapcar (lambda (c) (length (car c))) fields)))
                             (fmt     (format "[%%-%ds] %%s" max-len))
                             (key-cand (propertize (format fmt "key" key)
                                                   'citar--resource 'copy-field))
                             (copy-cands (mapcar (lambda (cell)
                                                   (propertize (format fmt (car cell) (cdr cell))
                                                               'citar--resource 'copy-field))
                                                 fields)))
                        (append (list key-cand) copy-cands
                                (list (propertize key 'citar--resource 'open-in-ebib)))))
                    citekeys)))
      (cons 'multi-category (append (when result (cdr result)) extra-cands))))

  (defun +citar--open-resource-extras-a (orig resource &optional type)
    "Handle copy-field and open-in-ebib in `citar--open-resource'."
    (pcase (or type (get-text-property 0 'citar--resource resource))
      ('copy-field
       (let ((val (and (string-match "\\`\\[[^]]+\\] \\(\\(?:.\\|\n\\)*\\)\\'" resource)
                       (match-string 1 resource))))
         (kill-new (or val resource))
         (message "Copied: %s" (or val resource))))
      ('open-in-ebib
       (citar-open-entry-in-ebib (substring-no-properties resource)))
      (_ (funcall orig resource type))))

  (defun +citar--select-group-extras-a (orig resource transform)
    "Add group labels for copy-field and open-in-ebib in completing-read.
Also fixes create-note grouping: after display replacement the outer string
has no direct `citar--resource', so we look it up via `multi-category'."
    (let* ((direct (get-text-property 0 'citar--resource resource))
           (rtype  (or direct
                       (when-let* ((mc    (get-text-property 0 'multi-category resource))
                                   (inner (cdr mc)))
                         (get-text-property 0 'citar--resource inner)))))
      (pcase rtype
        ('copy-field   (if transform resource "Copy Field"))
        ('open-in-ebib (if transform resource "Open in Ebib"))
        (_
         ;; Pass the resource with citar--resource set so the original
         ;; group function correctly labels create-note (and other) types.
         (funcall orig
                  (if (and rtype (not direct))
                      (propertize resource 'citar--resource rtype)
                    resource)
                  transform)))))

  (advice-add 'citar--get-resource-candidates :around #'+citar--get-resource-extras-a)
  (advice-add 'citar--open-resource :around #'+citar--open-resource-extras-a)
  (advice-add 'citar--select-group-related-resources :around #'+citar--select-group-extras-a)

  (advice-add 'citar-insert-citation :around
              (lambda (orig-fun &rest args)
                (+evil/smart-insert)
                (apply orig-fun args))))

(use-package citar-embark
  :ensure t
  :after (citar embark)
  :no-require
  :config
  (citar-embark-mode))

;; utility function
(defun +latex/isolate-sentence ()
  "Replace '. ' with '.\n%\n' in the selected region, similar to Vim's :'<,'>s/\. /.\n%\n/g."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          (while (search-forward ". " end t)
            (replace-match ".\n%\n" nil t))))
    (message "No region selected!"))
  (message "Replacement done!"))

;; Configure uniquification (appends a/b/c for duplicates)
(setopt bibtex-autokey-name-case-convert-function #'upcase)
(setopt bibtex-autokey-names 4)
(setopt bibtex-autokey-additional-names "+")
(setopt bibtex-autokey-name-stretch 0)
(setopt bibtex-autokey-name-length 1)
(setopt bibtex-autokey-year-length 4)
(setopt bibtex-autokey-titlewords 0)
(setopt bibtex-autokey-titlewords-stretch 0)
(setopt bibtex-autokey-name-year-separator "")

;; HACK override this function from built-in
;; maybe report upstream?
(defun bibtex-autokey-abbrev (string len)
  "Return an abbreviation of STRING with at least LEN characters.
If LEN is positive the abbreviation is terminated only after a consonant
or at the word end.  If LEN is negative the abbreviation is strictly
enforced using abs (LEN) characters.  If LEN is not a number, STRING
is returned unchanged."
  (cond ((or (not (numberp len))
             (<= (length string) (abs len)))
         string)
        ((equal len 0)
         "")
        ((equal len 1)
         (substring string 0 1))
        ((< len 0)
         (substring string 0 (abs len)))
        (t (let* ((case-fold-search t)
                  (abort-char (string-match "[^aeiou]" string (1- len))))
             (message "%s" abort-char)
             (if abort-char
                 (substring string 0 (1+ abort-char))
               string)))))

;; NOTE if you are drawing svg through inkscpae
;;
;; first, use the following in dir-local if you want to svg in latex
;; ((LaTeX-mode . ((TeX-command-extra-options . "-shell-escape"))))
;;
;; then, in you latex file, explicitly uses svg package, and let inkscape controls the font
;; \usepackage{svg}
;; \svgsetup{inkscapelatex=false}

(provide 'init-latex)
