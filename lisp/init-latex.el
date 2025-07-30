;;; init-latex.el --- latex support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-org)

(+ensure-packages-installed
 '(
   ;; enrich org mode
   citar
   ;; preview org math
   ;; xenops
   ;; pdf-tools support`'
   pdf-tools
   ;; latex support
   auctex
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

(use-package auctex
  :ensure t)

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

(setopt org-startup-with-latex-preview 'nil) ;; do not preview
(setopt org-preview-latex-default-process 'dvisvgm)

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
