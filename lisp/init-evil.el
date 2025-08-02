;;; init-evil.el --- evil support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(+ensure-packages-installed
 '(
   ;; more convenient way of defining keys
   general 
   which-key
   ;; evil-related
   evil
   evil-collection
   evil-multiedit
   evil-mc
   evil-goggles
   evil-nerd-commenter
   evil-args
   evil-surround
   evil-terminal-cursor-changer
   ))

;; evil
(use-package evil
  :ensure t
  :preface
  (setq evil-overriding-maps nil)
  (setq evil-want-keybinding nil)
  (setq evil-auto-indent nil)
  (setq evil-want-Y-yank-to-eol t) ; this need to be set before evil
  (setq evil-want-C-g-bindings t) ; this need to be set before evil
  ;; whether to use emacs bindings in insert-mode
  (setq evil-disable-insert-state-bindings nil)
  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t)
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1))

;;; evil-collection
(use-package evil-collection
  :ensure t
  :preface
  (setq evil-want-keybinding nil)
  :after (:and evil evil-mc)
  :config
  ;; make sure the follwing key bindings always work
  (evil-collection-init)
  ;; HACK re-bind keys
  (evil-collection-define-key '(normal visual) 'evil-mc-key-map (kbd "gz") evil-mc-cursors-map)
  )

;; HACK https://www.reddit.com/r/emacs/comments/45w9mv/comment/d3ud03t/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(defun normal-escape-pre-command-handler ()
  (interactive)
  (pcase this-command
    (_ (when (and (string= "C-g" (key-description (this-command-keys)))
                  (bound-and-true-p evil-mode)
                  (or (evil-insert-state-p)
                      (evil-emacs-state-p)))
         (evil-force-normal-state)))))
(add-hook 'pre-command-hook 'normal-escape-pre-command-handler)

(use-package evil-args
  :ensure t
  :after evil
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  
  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  
  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(use-package evil-nerd-commenter
  :ensure t
  :after evil)

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil ; too slow
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  )

(use-package evil-mc
  :ensure t
  :after evil
  :config
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  (global-evil-mc-mode  1) ;; enable
  )

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(when (not (display-graphic-p))
  (use-package evil-terminal-cursor-changer
    :ensure t
    :after evil
    :config
    (evil-terminal-cursor-changer-activate) ; or (etcc-on)
    (setq evil-motion-state-cursor 'box)  ; █
    (setq evil-visual-state-cursor 'box)  ; █
    (setq evil-normal-state-cursor 'box)  ; █
    (setq evil-insert-state-cursor 'bar)  ; ⎸
    (setq evil-emacs-state-cursor  'hbar) ; _
    ))

(provide 'init-evil)
