;;; core-startup.el --- Startup tuning and workarounds -*- lexical-binding: t -*-
;;; Commentary:
;; Emacs version gate, startup performance restoration, proxy application,
;; process tuning, version workarounds, and session side-effects (compression,
;; server).
;;; Code:

(require 'core-vars)

;; Require a modern Emacs.
(let ((minver "30.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(setq load-prefer-newer t)

;; Faster subprocess (e.g. LSP) reads.  Default is 4KB, too small for LSP.
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Quiet down development-time warning noise.
(setopt warning-suppress-log-types '((files)))
(setq byte-compile-warnings '(not lexical))
;; Silently report native-comp warnings/errors (handled by the deny-list below).
(setopt native-comp-async-report-warnings-errors 'silent)

;; `early-init.el' raises the GC ceiling and clears `file-name-handler-alist'.
;; When early-init did not run (e.g. `emacs -q --load init.el') the `defvar'
;; below captures the live value as a safe fallback so the restore is correct.
(defvar +emacs/initial-file-name-handler-alist file-name-handler-alist
  "Value of `file-name-handler-alist' before startup tuning.")

(defun +emacs/restore-startup-settings-h ()
  "Reset GC and file-handler settings to normal post-startup values."
  (setq gc-cons-threshold (* 16 1024 1024) ; 16MB
        gc-cons-percentage 0.1
        file-name-handler-alist +emacs/initial-file-name-handler-alist))

(add-hook 'emacs-startup-hook #'+emacs/restore-startup-settings-h)

;; Apply the configured proxy to Emacs' URL stack when it is available.
(when (boundp 'url-proxy-services)
  (add-to-list 'url-proxy-services `("http" . ,+emacs/proxy))
  (add-to-list 'url-proxy-services `("https" . ,+emacs/proxy)))

;; Work around an Emacs 30.2+ native-comp regression in built-in Org.
(defvar native-comp-jit-compilation-deny-list nil)
(dolist (regexp '(".*org-element.*" ".*org-macs.*"))
  (add-to-list 'native-comp-jit-compilation-deny-list regexp))

;; Org 9.8.5 bytecode can call this version check as a runtime function under
;; Emacs 31 snapshots, although Org defines it as a macro.
(with-eval-after-load 'org-macs
  (when (macrop 'org-assert-version)
    (defalias 'org-assert-version #'ignore)))

;; Keep `auto-compression-mode' robust across reloads.
(auto-compression-mode 0)
(auto-compression-mode 1)

;; Start the Emacs server for emacsclient (interactive sessions only).
(require 'server)
(unless (or noninteractive (server-running-p))
  (server-start))

(provide 'core-startup)
;;; core-startup.el ends here
