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
    "Inject copy-title and open-in-ebib candidates into `citar--get-resource-candidates'."
    (let* ((result (apply orig citekeys args))
           (extra-cands
            (apply #'append
                   (mapcar (lambda (key)
                             (list
                              (propertize (format "[Copy Title] %s" key)
                                          'citar--resource 'copy-title)
                              (propertize (format "[Open in Ebib] %s" key)
                                          'citar--resource 'open-in-ebib)))
                           citekeys))))
      (cons 'multi-category (append (when result (cdr result)) extra-cands))))

  (defun +citar--open-resource-extras-a (orig resource &optional type)
    "Handle copy-title and open-in-ebib in `citar--open-resource'."
    (pcase (or type (get-text-property 0 'citar--resource resource))
      ('copy-title
       (let* ((key (string-trim (string-remove-prefix "[Copy Title] "
                                                      (substring-no-properties resource))))
              (title (or (citar-get-value "title" key) key)))
         (kill-new title)
         (message "Copied title: %s" title)))
      ('open-in-ebib
       (citar-open-entry-in-ebib
        (string-trim (string-remove-prefix "[Open in Ebib] "
                                           (substring-no-properties resource)))))
      (_ (funcall orig resource type))))

  (defun +citar--select-group-extras-a (orig resource transform)
    "Add group labels for copy-title and open-in-ebib in completing-read."
    (pcase (get-text-property 0 'citar--resource resource)
      ('copy-title    (if transform resource "Copy Title"))
      ('open-in-ebib  (if transform resource "Open in Ebib"))
      (_ (funcall orig resource transform))))

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
