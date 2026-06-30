;;; early-init.el --- Machine-local early init -*- lexical-binding: t -*-
;;; Commentary:
;; Installed to ~/.emacs.d/early-init.el by `make init'.  Points Emacs at the
;; configuration repository and loads its early-init.
;;; Code:

;; Path to the configuration repository (filled in by `make init').
(setq +emacs/repo-directory "@REPO_DIRECTORY@")

;; Uncomment to request undecorated GUI frames before the first frame is made.
;; (add-to-list 'default-frame-alist '(undecorated . t))

(load (expand-file-name "early-init.el" +emacs/repo-directory))

;;; early-init.el ends here
