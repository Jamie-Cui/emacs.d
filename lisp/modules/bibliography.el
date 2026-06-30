;;; bibliography.el --- bibliography management -*- lexical-binding: t -*-
;;; Commentary:
;; Bibliography management: citar, ebib, bibtex and citar-embark.
;;; Code:


(setq +ebib/org-root-dir +emacs/org-root-dir)
(require 'init-config-ebib)

(setq +citar/org-root-dir +emacs/org-root-dir)
(require 'init-config-citar)

(use-package citar-embark
  :ensure t
  :after (citar embark)
  :no-require
  :config
  (citar-embark-mode))

(require 'init-config-bibtex)


(provide 'init-bibliography)
;;; bibliography.el ends here
