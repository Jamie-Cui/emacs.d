;;; init-latex.el --- latex support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-org)

(+ensure-packages-installed
 '(
   ;; enrich bib frontend from bib file
   citar
   citar-embark
   ;; download from web
   biblio
   ;; pdf-tools support
   pdf-tools
   ;; latex support
   auctex
   ;; preview org math
   ;; xenops
   ))

(use-package pdf-tools
  :ensure t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode) ; Associate .pdf files with pdf-view-mode
  :magic ("%PDF" . pdf-view-mode) ; Use magic number to identify PDF files
  :custom
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  :init
  (pdf-tools-install)
  :config
  ;; pdf-tools have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Enable SyncTeX
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)

  ;; Set PDF-Tools as the default viewer
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

  ;; Define forward search function
  (defun TeX-pdf-tools-sync-view ()
    (let ((pdf-file (expand-file-name (concat (TeX-master-file "pdf") ".pdf")))
          (tex-file buffer-file-name)
          (line (line-number-at-pos)))
      (find-file pdf-file) ;; default should be pdf-tools
      (pdf-sync-forward-search tex-file line)))

  ;; Enable Inverse Search in PDFs
  (setq pdf-sync-backward-search-method 'generic
        pdf-sync-generic-forward-search-command
        "emacsclient --no-wait +%l '%f'")

  (setq TeX-PDF-mode t) ; Ensure PDF output
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode) ; Auto-enable SyncTeX
  )

(use-package biblio
  :ensure t
  :custom
  (biblio-bibtex-use-autokey t)
  :config
  )

(use-package auctex
  :ensure t)

;; this package is built-in
(use-package bibtex
  :ensure t
  :config
  (defun +bibtex/add-doi ()
    (interactive)
    (progn
      (setq doi-to-query (read-string "DOI: "))
      (find-file (concat jc-org-root-dir "/all-ref.bib"))
      (end-of-buffer)
      (doi-insert-bibtex doi-to-query)
      )
    )

  (defun +bibtex/consult-bibtex-file ()
    (interactive)
    (let ((file (consult--read bibtex-files
                               :prompt "bibtex-files: "
                               :sort nil
                               :require-match t
                               :category 'file
                               :history 'file-name-history)))
      (find-file file)))

  ;; Configure uniquification (appends a/b/c for duplicates)
  (setq bibtex-autokey-add-year t)
  (setq bibtex-autokey-year-length 4)
  (setq bibtex-autokey-titlewords 0)
  (setq bibtex-autokey-titlewords-stretch 0)
  (setq bibtex-autokey-name-year-separator "")

  ;; default bib file
  (add-to-list 'bibtex-files (concat jc-org-root-dir "/all-ref.bib"))
  )

(use-package citar
  :ensure t
  :config
  (add-to-list 'citar-bibliography (concat jc-org-root-dir "/all-ref.bib"))
  (add-to-list 'citar-notes-paths (concat jc-org-root-dir "/roam"))
  ;; NOTE this var is used by org-export
  (add-to-list 'org-cite-global-bibliography (concat jc-org-root-dir "/all-ref.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config
  (citar-embark-mode))

;; (use-package xenops
;;   :ensure t
;;   :if window-system ;; do not load xenops on termial emacs
;;   :config
;;   (add-hook 'org-mode-hook #'xenops-mode)
;;   (setq xenops-math-image-current-scale-factor 1.2)
;;   (setq xenops-math-image-margin 0))

(use-package engrave-faces
  :ensure t)

(provide 'init-latex)
