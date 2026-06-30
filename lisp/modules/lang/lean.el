;;; lean.el --- Lean support (nael) -*- lexical-binding: t -*-
;;; Commentary:
;; Lean support (nael).
;;; Code:

;; Emacs package for lean
(use-package nael
  :ensure t
  :defer t
  :custom
  (nael-prepare-lsp nil)
  (nael-prepare-eglot t)
  :config
  (add-hook 'nael-mode-hook #'abbrev-mode)
  (add-hook 'nael-mode-hook #'eglot-ensure))

(provide 'init-lang-lean)
;;; lean.el ends here
