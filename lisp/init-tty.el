;;; init-tty.el --- native emacs tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(unless (display-graphic-p)
  (+ensure-packages-installed
   '(
     corfu-terminal
     ))

  ;; Support for child frames in terminal frames was added in 31. Enable it, if it
  ;; is available.
  (when (featurep 'tty-child-frames)
    (add-hook 'tty-setup-hook #'tty-tip-mode))

  ;; Use OSC52 protocol, which is used by alacritty by default
  (defun copy-to-system-clipboard (text &optional push)
    (send-string-to-terminal (format "\e]52;c;%s\a" (base64-encode-string text))))

  (setq interprogram-cut-function 'copy-to-system-clipboard)

  (use-package corfu-terminal
    :ensure t
    :config
    (corfu-terminal-mode +1)
    )

  (xterm-mouse-mode 1)
  
  (setopt eldoc-echo-area-use-multiline-p nil)
  )

(provide 'init-tty)
