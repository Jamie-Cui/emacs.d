;;; early-init.el --- Early initialization -*- lexical-binding: t -*-
;;; Commentary:
;; Loaded before the package system and the first frame.  Only settings that
;; must take effect this early belong here; everything else lives in the core
;; layer and modules.  Performance values raised here are restored in
;; `core-startup'.
;;; Code:

;; Raise the GC ceiling during startup; restored on `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Skip the file-name handler machinery during startup I/O.
(defvar +emacs/initial-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; We initialize package.el explicitly in `core-package'.
(setq package-enable-at-startup nil)

;; Prefer newer source over stale byte-code.
(setq load-prefer-newer t)

;; Disable UI chrome before the first frame to avoid flicker.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;;; early-init.el ends here
