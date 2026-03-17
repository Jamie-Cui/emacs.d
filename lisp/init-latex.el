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
               (concat (+emacs/ensure-directory +emacs/org-root-dir) "/all-ref.bib")))

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
