;;; -*- lexical-binding: t; -*-
(defvar +emacs/repo-directory (expand-file-name "~/emacs.d"))
(defvar +emacs/repo-directory (expand-file-name "~/org-root"))

(load (concat +emacs/repo-directory "/init.el"))

;; (add-to-list 'default-frame-alist '(undecorated . t))

;;; Set Proxy

;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;      ("http" . "proxy.com:8080")
;;      ("https" . "proxy.com:8080")))
