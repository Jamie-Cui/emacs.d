;;; init.el --- Machine-local init -*- lexical-binding: t -*-
;;; Commentary:
;; Installed to ~/.emacs.d/init.el by `make init'.  Put machine-local and
;; user-tunable settings here; shared configuration lives in the repository.
;;; Code:

;; Path to the configuration repository (filled in by `make init').
(setq +emacs/repo-directory "@REPO_DIRECTORY@")

;; User-tunable defaults (uncomment and edit as needed).
;; (setq +emacs/org-root-dir (expand-file-name "~/opt/org-root"))
;; (setq +emacs/proxy "127.0.0.1:10808")
;; (setq +emacs/email-address "you@outlook.com"
;;       +emacs/email-full-name "Your Name"
;;       +emacs/email-maildir (expand-file-name "~/.local/share/mail/outlook")
;;       +emacs/mu4e-load-path "/usr/share/emacs/site-lisp/mu4e")
;; (setq +emacs/disabled-modules '("llm" "lang/lean"))

;; If you use an Apple keyboard, map the Super key to Meta.
(setq x-super-keysym 'meta)

;; Package archives.  Switch to a mirror if the official hosts are slow.
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
;; TUNA mirror:
;; (setq package-archives
;;       '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;;         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Load the shared configuration.
(load (expand-file-name "init.el" +emacs/repo-directory))

;; Theme (loaded after the configuration installs the theme packages).
(load-theme 'zenburn t)

;;; init.el ends here
