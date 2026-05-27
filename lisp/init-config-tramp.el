;;; init-config-tramp.el --- TRAMP configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)
(require 'tramp)

(setopt enable-remote-dir-locals t)
(setopt tramp-use-file-attributes nil)
(setopt remote-file-name-inhibit-cache nil)
(setopt remote-file-name-inhibit-auto-save t)
(setopt remote-file-name-inhibit-auto-save-visited t)

(setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

;; Improve tramp speed.
;; see: https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setopt tramp-allow-unsafe-temporary-files t ; do not warn me, please
        remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;; forgot why I add this ...
(setopt tramp-pipe-stty-settings "")

;; this improves magit efficiency
(unless (featurep :system 'windows)
  (setopt tramp-default-method "ssh")) ; faster than the default scp

;; allow asyn in tramp
(setopt tramp-async-enabled t)

;; PERF: Calls over TRAMP are expensive, so reduce the number of calls by more
;; aggressively caching some common data. Inspired by
;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr.
(defun +tramp--memoize (key cache fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key (file-remote-p key))
      (if-let* ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply fn args)))

;;;###package magit
(defvar +tramp--magit-toplevel-cache nil)
(defun +tramp--memoized-magit-toplevel-a (orig &optional directory)
  (+tramp--memoize (or directory default-directory)
                   '+tramp--magit-toplevel-cache orig directory))

(with-eval-after-load 'magit
  (advice-add #'magit-toplevel :around #'+tramp--memoized-magit-toplevel-a))

;;;###package project
(defvar +tramp--project-current-cache nil)
(defun +tramp--memoized-project-current (fn &optional prompt directory)
  (+tramp--memoize (or directory
                       project-current-directory-override
                       default-directory)
                   '+tramp--project-current-cache fn prompt directory))

(advice-add #'project-current :around #'+tramp--memoized-project-current)

;;;###package vc-git
(defvar +tramp--vc-git-root-cache nil)
(defun +tramp--memoized-vc-git-root-a (fn file)
  (let ((value
         (+tramp--memoize (file-name-directory file)
                          '+tramp--vc-git-root-cache fn file)))
    ;; sometimes vc-git-root returns nil even when there is a root there
    (unless (cdar +tramp--vc-git-root-cache)
      (setq +tramp--vc-git-root-cache (cdr +tramp--vc-git-root-cache)))
    value))

(with-eval-after-load 'vc-git
  (advice-add #'vc-git-root :around #'+tramp--memoized-vc-git-root-a))

(provide 'init-config-tramp)
;;; init-config-tramp.el ends here
