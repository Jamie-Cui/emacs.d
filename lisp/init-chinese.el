;;; init-chinese.el --- chinese support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(+package/ensure-install
 '(
   ;; chinese spacing
   pangu-spacing
   ;; chinese s alignment, or valign (maybe?)
   cnfonts
   ))

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
  (cnfonts-use-face-font-rescale t)
  :config
  ;; use this to list all fonts
  ;; (cl-prettyprint (font-family-list))
  (cnfonts-mode 1)
  )

(provide 'init-chinese)
