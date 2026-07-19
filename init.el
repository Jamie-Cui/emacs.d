;;; init.el --- Configuration manifest -*- lexical-binding: t -*-
;;; Commentary:
;; Entry point for the shared configuration.  Loads the core layer, then the
;; feature modules via `+emacs/load-modules'.  Machine-local settings live in
;; ~/.emacs.d/init.el.
;;; Code:

;; --- Bootstrap: locate the repository and the core layer. ---
(let ((repo (file-name-directory
             (file-truename (or load-file-name buffer-file-name
                                (expand-file-name "init.el"))))))
  (add-to-list 'load-path (expand-file-name "lisp" repo))
  (add-to-list 'load-path (expand-file-name "lisp/core" repo))
  (unless (boundp '+emacs/repo-directory)
    (setq +emacs/repo-directory (directory-file-name repo))))

;; --- Core layer (fixed order). ---
(require 'core-vars)
(require 'core-paths)
(require 'core-startup)
(require 'core-package)
(require 'core-loader)
(require 'core-util)

;; --- Module manifest -------------------------------------------------------
;; Evil and its config provide commands and helpers used by feature modules at
;; load time, so they load before the manifest.  `keys' loads last so every
;; module command it binds is already defined.
(require 'init-config-evil)

(+emacs/load-modules
 '("os"
   "ui"
   "editor"
   "completion"
   "files"
   "project"
   "vc"
   "prog"
   "lang/lean"
   "lang/protobuf"
   "lang/meson"
   "lang/cmake"
   "lang/bazel"
   "lang/markdown"
   "org"
   "notes"
   "bibliography"
   "latex"
   "reading"
   "email"
   "llm"
   "input"
   "keys"))

;;; init.el ends here
