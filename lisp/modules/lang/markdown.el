;;; markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;; Markdown support.
;;; Code:

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(provide 'init-lang-markdown)
;;; markdown.el ends here
