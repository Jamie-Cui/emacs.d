;;; core-package.el --- Package system bootstrap -*- lexical-binding: t -*-
;;; Commentary:
;; Initializes package.el and use-package.  Machine-local `package-archives'
;; and theme loading live in the installed `~/.emacs.d/init.el'.
;;; Code:

(require 'cl-lib)

;; The generated quickstart file loads before the user init, so it cannot see
;; the setup done here; keep it disabled.
(setopt package-quickstart nil)

(require 'package)

;; Trust the configured archives without signature checks.
(setq package-check-signature nil)
(setq package-native-compile t)
;; Installed ELPA overrides of built-in packages are upgraded normally.  Do not
;; also add every active built-in to `package-upgrade-all': Emacs 31.0.60
;; destructively joins its memoized built-in alist to `package-alist', which can
;; produce duplicate upgrades and eventually make the package list circular.
(setq package-install-upgrade-built-in nil)

(unless package--initialized
  (package-initialize))

(require 'use-package)

;; Several feature modules use `general-define-key' at load time, before the
;; final keys module is loaded.  Ensure and load it as part of package
;; bootstrap so clean installs don't depend on an existing ELPA cache.
(use-package general
  :ensure t
  :demand t)

(provide 'core-package)
;;; core-package.el ends here
