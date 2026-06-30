;;; core-loader.el --- Module manifest loader -*- lexical-binding: t -*-
;;; Commentary:
;; `+emacs/load-modules' loads feature modules from `lisp/modules' in the given
;; order, honoring `+emacs/disabled-modules'.  Module names may be path-like
;; (e.g. "lang/cmake" -> lisp/modules/lang/cmake.el).  No dependency inference
;; is performed; manifest order is the contract.
;;; Code:

(require 'core-vars)
(require 'core-paths)

(defun +emacs/module-disabled-p (name)
  "Return non-nil when module NAME is in `+emacs/disabled-modules'."
  (member name +emacs/disabled-modules))

(defun +emacs/load-modules (modules)
  "Load each module in MODULES unless disabled.
MODULES is a list of name strings such as \"ui\" or \"lang/cmake\",
resolved to files under `+emacs/modules-directory'."
  (dolist (name modules)
    (unless (+emacs/module-disabled-p name)
      (load (expand-file-name name +emacs/modules-directory) nil t))))

(provide 'core-loader)
;;; core-loader.el ends here
