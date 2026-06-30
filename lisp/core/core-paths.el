;;; core-paths.el --- Load paths and directory helpers -*- lexical-binding: t -*-
;;; Commentary:
;; Sets up `load-path' for the configuration layers and local packages, and
;; provides the small directory helpers used across modules.
;;; Code:

(require 'core-vars)

(defvar +emacs/lisp-directory (expand-file-name "lisp" +emacs/repo-directory)
  "Directory holding the configuration's Elisp.")
(defvar +emacs/core-directory (expand-file-name "core" +emacs/lisp-directory)
  "Directory holding the core layer.")
(defvar +emacs/modules-directory (expand-file-name "modules" +emacs/lisp-directory)
  "Directory holding feature modules.")
(defvar +emacs/site-lisp-directory (expand-file-name "site-lisp" +emacs/repo-directory)
  "Directory holding local and forked packages.")

;; NOTE: `+emacs/modules-directory' is deliberately NOT added to `load-path'.
;; Modules are loaded by absolute path via `+emacs/load-modules', and several
;; module basenames (project, vc, org, files) would otherwise shadow built-in
;; Emacs libraries of the same name.
(dolist (dir (list +emacs/lisp-directory
                   +emacs/core-directory
                   +emacs/site-lisp-directory))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(defun +emacs/ensure-directory (dir)
  "Create DIR (and parents) if needed and return it."
  (make-directory dir t)
  dir)

(defun +emacs/org-subdir (name)
  "Return path to org subdirectory NAME, creating it if needed."
  (+emacs/ensure-directory (expand-file-name name +emacs/org-root-dir)))

(defun +emacs/bin-directory ()
  "Return path to the user bin directory, creating it if needed."
  (when (and user-init-file (stringp user-init-file))
    (+emacs/ensure-directory
     (expand-file-name "bin" (file-name-directory user-init-file)))))

(provide 'core-paths)
;;; core-paths.el ends here
