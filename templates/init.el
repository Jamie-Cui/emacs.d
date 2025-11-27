;;; -*- lexical-binding: t; -*-

;;; ------------------------
;;; REVIEW configure constants
;;; ------------------------

;; (setopt +emacs/repo-directory (expand-file-name "~/emacs.d"))
;; (setopt +emacs/org-root-dir (expand-file-name "~/org-root"))
;; (setopt +emacs/proxy "127.0.0.1:10808")

;; NOTE if you are using magic keyboard
(setq x-super-keysym 'meta)

;;; ----------------------------
;;; REVIEW package urls
;;; ----------------------------

;; use tuna mirros
;; (setq package-archives 
;;       '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;;         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;         ))

(require 'package)

;; use official
(setq package-archives 
      '(
        ("gnu"   . "http://elpa.gnu.org/packages/")
        ("nongnu"   . "http://elpa.nongnu.org/nongnu/")
        ("org"   . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ))

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

;; ------------------------------------------------------------------
;;; DONE EAF
;; ------------------------------------------------------------------

;; (use-package eaf
;;   :load-path "~/opt/eaf"
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (require 'eaf-browser)
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki

;;; ----------------------------
;;; others
;;; ----------------------------
