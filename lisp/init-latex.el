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
  :config
  ;; HACK the following may not work
  ;; (eval-after-load "bibtex"
  ;;   '(defun bibtex-generate-autokey ()
  ;;      (let* ((author (bibtex-autokey-get-field "author"))
  ;;             (names (if author (bibtex-autokey-get-names author)))))
  ;;      (concat
  ;;       ;; Author part
  ;;       (upcase 
  ;;        names
  ;;        (cond
  ;;         ((null names) "???")
  ;;         ((= (length names) 1) (car names)) ; Full last name for single author
  ;;         (t (mapconcat (lambda (n) (substring n 0 1)) ; Initials for multiple
  ;;                       (seq-take names (min (length names) 4)) 
  ;;                       "")))
  ;;        )
  ;;       ;; Year part
  ;;       (if-let ((year (bibtex-autokey-get-field "year")))
  ;;           (substring (concat "????" year) -4)
  ;;         "????"))
  ;;      ))

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
