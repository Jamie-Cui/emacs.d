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
   ;; latex support
   auctex
   ;; export org code in colors
   engrave-faces
   ))

(use-package auctex
  :ensure t)

(use-package org
  :config
  (add-to-list 'org-latex-packages-alist
               '("lambda, advantage, operators, sets, adversary, landau,\
 probability, notions, logic, ff, mm, primitives, events, complexity, oracles,\
 asymptotics, keys" "cryptocode" t))
  (add-to-list 'org-latex-packages-alist
               '("" "booktabs" t))
  )

(use-package ox-latex
  :after (engrave-faces citar)
  :custom
  (org-export-with-toc nil)
  :config
  (setq org-latex-src-block-backend 'engraved)
  (setq org-latex-engraved-theme 't)
  ;; this var is used by org-export
  (add-to-list 'org-cite-global-bibliography (concat jc-org-root-dir "/zotero_all.bib"))
  )

(use-package citar
  :ensure t
  :config
  (add-to-list 'citar-bibliography (concat jc-org-root-dir "/zotero_all.bib"))
  (add-to-list 'citar-notes-paths (concat jc-org-root-dir "/roam"))
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
