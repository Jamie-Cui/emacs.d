;;; init-tty.el --- native emacs tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)

;; Support for child frames in terminal frames was added in 31. Enable it, if it
;; is available.
(when (featurep 'tty-child-frames)
  (add-hook 'tty-setup-hook #'tty-tip-mode))

;; Use OSC52 protocol, which is used by alacritty by default
(defun copy-to-system-clipboard (text &optional push)
  (send-string-to-terminal (format "\e]52;c;%s\a" (base64-encode-string text))))

(when (not (display-graphic-p))
  (setq interprogram-cut-function 'copy-to-system-clipboard))

(provide 'init-tty)
