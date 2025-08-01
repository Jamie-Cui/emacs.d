;;; init-latex.el --- latex support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-org)

(+ensure-packages-installed
 '(
   ;; enrich bib frontend from bib file
   citar
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
  :custom
  (bibtex-autokey-name-year-separator ":")
  (bibtex-autokey-year-title-separator ":")
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-titlewords 3)
  (bibtex-autokey-titleword-length -1) ;; -1 means exactly one
  (bibtex-autokey-titlewords-stretch 0)
  (bibtex-autokey-titleword-separator "")
  (bibtex-autokey-titleword-case-convert 'upcase)
  :config
  (add-to-list 'bibtex-files (concat jc-org-root-dir "/all-ref.bib"))
  ;; call bibtex-reformat to reformat all bib file
  ;; C-c C-e : add a new entry
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
