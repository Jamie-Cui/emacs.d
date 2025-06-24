;;; -*- lexical-binding: t; -*-
(defvar jc-emacs-directory (expand-file-name "~/emacs.d"))
(defvar jc-emacs-directory (expand-file-name "~/org-root"))

(load (concat jc-emacs-directory "/init.el"))

;;; Set Proxy

;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;      ("http" . "proxy.com:8080")
;;      ("https" . "proxy.com:8080")))
