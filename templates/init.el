;;; -*- lexical-binding: t; -*-

;;; ------------------------
;;; DONE configure constants
;;; ------------------------

(defvar +emacs/repo-directory (expand-file-name "~/emacs.d"))
(defvar +emacs/org-root-dir (expand-file-name "~/org-root"))
(defvar +emacs/proxy "127.0.0.1:10808")

;;; ----------------------------
;;; REVIEW use undecorated frame
;;; ----------------------------

;; (add-to-list 'default-frame-alist '(undecorated . t))

;;; ----------------------------
;;; REVIEW setup proxy
;;; ----------------------------

;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;      ("http" . "proxy.com:8080")
;;      ("https" . "proxy.com:8080")))

;;; ----------------------------
;;; DONE load all packages
;;; ----------------------------

(load (concat +emacs/repo-directory "/init.el"))

;;; ----------------------------
;;; DONE load theme
;;; ----------------------------

(load-theme 'zenburn t)

;;; ----------------------------
;;; others
;;; ----------------------------
