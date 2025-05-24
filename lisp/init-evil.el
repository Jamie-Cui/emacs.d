;;; init-evil.el --- evil support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)

(+ensure-packages-installed
 '(
   ;; evil-related
   evil
   evil-collection
   evil-multiedit
   evil-mc
   evil-goggles
   evil-nerd-commenter
   evil-args
   evil-escape ; quit everything with C-g!
   general ; more convenient way of defining keys
   which-key
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

(use-package evil-escape
  :ensure t
  :after (evil general)
  :custom
  (evil-escape-excluded-states '(normal visual multiedit emacs motion))
  (evil-escape-excluded-major-modes '(vterm-mode))
  (evil-escape-key-sequence nil)
  (evil-escape-delay 0.15)
  :config
  ;; `evil-escape' in the minibuffer is more disruptive than helpful. That is,
  ;; unless we have `evil-collection-setup-minibuffer' enabled, in which case we
  ;; want the same behavior in insert mode as we do in normal buffers.
  (add-hook 'evil-escape-inhibit-functions
            (defun +evil-inhibit-escape-in-minibuffer-fn ()
              (and (minibufferp)
                   (or (not (bound-and-true-p evil-collection-setup-minibuffer))
                       (evil-normal-state-p)))))
  (general-define-key
   :states '(insert replace visual operator)
   "C-g" #'evil-escape)

  (evil-escape-mode)
  )

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
  (global-evil-mc-mode  1) ;; enable
  (setq evil-mc-undo-cursors-on-keyboard-quit t))

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(provide 'init-evil)
