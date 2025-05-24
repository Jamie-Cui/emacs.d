;;; init-chinese.el --- chinese support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)

(+ensure-packages-installed
 '(
   ;; Chinese input
   rime
   ;; chinese spacing
   pangu-spacing
   ;; chinese s alignment, or valign (maybe?)
   cnfonts
   ))

;; linux use rime
(use-package rime
  :ensure t
  :when (not (eq system-type 'darwin)) ;; do not load rime on macos
  :config
  (setq default-input-method "rime")
  (setq rime-show-candidate 'popup))

(use-package pangu-spacing
  :ensure t
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor nil)
  (add-hook 'org-mode-hook
            '(lambda ()
               (set (make-local-variable
                     'pangu-spacing-real-insert-separtor) t)))
  )

(use-package cnfonts
  :ensure t
  :custom
  (cnfonts-personal-fontnames '(
                                ("Maple Mono NF CN" "0xProto Nerd Font Mono") ;; English
                                ("Maple Mono NF CN") ;; Chinese
                                nil  ;; Ext-B
                                ("Symbols Nerd Font Mono") ;; Symbol
                                nil ;; Others
                                ))
  :config
  ;; use this to list all fonts
  ;; (cl-prettyprint (font-family-list))
  (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)
  (cnfonts-mode 1)
  )

(provide 'init-chinese)
