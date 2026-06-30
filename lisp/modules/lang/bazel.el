;;; bazel.el --- Bazel support -*- lexical-binding: t -*-
;;; Commentary:
;; Bazel support.
;;; Code:

(use-package bazel
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.BUILD\\'" . bazel-mode))
  (setq bazel-buildifier-before-save 't))

(provide 'init-lang-bazel)
;;; bazel.el ends here
