;;; init-evil.el --- evil support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(+package/ensure-install
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
  (setq evil-mode-line-format '(before . mode-line-front-space))
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

;; ------------------------------------------------------------------
;; DONE Key Bindings
;; ------------------------------------------------------------------

(use-package general
  :ensure t
  :after (evil evil-mc)
  :config
  (defconst my-leader "SPC")
  (defconst my-local-leader "SPC m")

  (general-create-definer +my-leader-def
    :prefix my-leader)

  (general-create-definer +my-local-leader-def
    :prefix my-local-leader)
  
  
  ;; HACK always get a new eat terminal
  (defun +eat/new ()
    (interactive)
    (let ((current-prefix-arg '(t)))
      (call-interactively 'eat)))

  ;; HACK kill current persp without asking
  (defun +persp/kill-current ()
    (interactive)
    (persp-kill (persp-current-name)))

  ;; ** keybindings that should not be overriden
  (general-define-key
   :keymaps 'override
   "M-i"     #'completion-at-point
   "M-y"     #'yas-expand
   "M-/"     #'evilnc-comment-or-uncomment-lines
   "M-f"     #'consult-line ; search like mac
   "M-a"     #'mark-whole-buffer ; select like mac
   "M-s"     #'save-buffer ; save like mac
   "M-v"     #'evil-paste-after ; paste like mac
   "C-u"     #'evil-scroll-up
   "C-d"     #'evil-scroll-down
   "C-="     #'cnfonts-increase-fontsize
   "C--"     #'cnfonts-decrease-fontsize
   "C-h"     #'persp-prev
   "C-l"     #'persp-next
   "C-M-h"   #'+persp/move-buffer-prev
   "C-M-l"   #'+persp/move-buffer-next)
  ;; ** Global Keybindings
  (+my-leader-def
    :states '(normal visual motion)
    :keymaps 'override ; prevent from being override
    ;; most-frequency keys
    "RET"    #'gptel
    "."      #'find-file
    "<"      #'consult-buffer
    ","      #'consult-project-buffer
    "/"      #'+vertico/project-search
    "TAB"    #'evil-switch-to-windows-last-buffer
    "SPC"    #'projectile-find-file
    ;; llm-related key bindings
    "i" '(:ignore t :which-key "search")
    "i RET"  #'gptel
    "ir"     #'gptel-rewrite
    "is"     #'gptel-send
    "iq"     #'gptel-abort
    "ic"     #'gptel-add
    "ii"     #'gptel-menu
    ;; action-related key bindings
    "a" '(:ignore t :which-key "actions")
    "a RET"  #'embark-dwim
    ;; window-related key bindings
    "w" '(:ignore t :which-key "window")
    "wh"     #'evil-window-left
    "wj"     #'evil-window-down
    "wk"     #'evil-window-up
    "wl"     #'evil-window-right
    "ww"     #'other-window
    "wx"     #'evil-window-exchange
    "wd"     #'evil-window-delete
    "ws"     #'evil-window-split
    "wv"     #'evil-window-vsplit
    "wm"     #'delete-other-windows
    ;; buffer-related key bindings
    "b" '(:ignore t :which-key "buffer")
    "bn"     #'evil-buffer-new
    "bd"     #'kill-current-buffer
    "br"     #'+revert-buffer-no-confirm
    "bc"     #'clean-buffer-list
    "bo"     #'persp-kill-other-buffers
    "B" '(:ignore t :which-key "bookmark")
    "BB"     #'consult-bookmark
    "Bn"     #'bookmark-set
    "Bd"     #'bookmark-delete
    "f" '(:ignore t :which-key "file")
    "fp"     #'consult-bookmark
    ;; open-related key bindings
    "o" '(:ignore t :which-key "open")
    "ot"     #'+eshell/new
    "oT"     #'+eat/new
    "od"     #'dired-jump
    "og"     #'magit-status-quick
    "ok"     #'kubernetes-overview
    "oc"     #'compile ; popup
    "ox"     #'scratch-buffer ; popup
    "om"     #'popwin:messages ; popup
    "oi"     #'gptel ; popup
    ;; project-related key bindings
    "p" '(:ignore t :which-key "project")
    "pa"     #'projectile-add-known-project
    "pd"     #'projectile-remove-known-project
    "pp"     #'projectile-switch-project
    "pC"     #'projectile-configure-project
    "pc"     #'projectile-compile-project
    "pt"     #'projectile-test-project
    "pr"     #'projectile-run-project
    "pi"     #'projectile-invalidate-cache
    "pf"     #'+vertico/project-search
    "po"     #'find-sibling-file
    "pH"     #'+persp/move-buffer-prev
    "pL"     #'+persp/move-buffer-next
    "ph"     #'persp-prev
    "pl"     #'persp-next
    "pq"     #'+persp/kill-current
    ;; note functions
    "n" '(:ignore t :which-key "note")
    "n@"      #'citar-insert-citation ;; insert bib
    "nb" '(:ignore t :which-key "bibtex")
    "nba"     #'+bibtex/add-doi ;; add bib from doi
    "nbb"     #'+bibtex/consult-bibtex-file ;; check bib source
    "nbf"     #'citar-open ;; consult for entry
    "ny"      #'org-store-link
    "np"      #'org-insert-link
    "ne"      #'org-export-dispatch
    "nd"      #'deft
    "nj"      #'org-journal-new-entry
    "nr" '(:ignore t :which-key "org-roam")
    "nrf"     #'org-roam-node-find
    "nri"     #'org-roam-node-insert
    "nrs"     #'org-roam-db-sync
    ;; help functions
    "h" '(:ignore t :which-key "help")
    "hf"     #'helpful-callable
    "hk"     #'helpful-key
    "hv"     #'helpful-variable
    "hm"     #'describe-mode
    ;; quit emacs
    "q" '(:ignore t :which-key "quit")
    "qq"     #'save-buffers-kill-terminal
    "qr"     #'restart-emacs
    ;; toggles
    "t" '(:ignore t :which-key "toggle")
    "th"     #'hs-hide-level
    "tf"     #'toggle-frame-maximized
    "tF"     #'toggle-frame-fullscreen
    "tt"     #'toggle-truncate-lines
    "tn"     #'display-line-numbers-mode
    "ta"     #'+treesit-auto/toggle
    ;; code
    "c" '(:ignore t :which-key "code")
    "cx"     #'list-flycheck-errors
    "ca"     #'eglot-code-actions
    "cr"     #'eglot-rename
    "cf"     #'eglot-format-buffer
    "cj"     #'consult-eglot-symbols
    ;; search
    "s" '(:ignore t :which-key "search")
    "si"     #'consult-imenu ;; search item
    "sh"     #'consult-history ;; search history
    )

  ;; deft mode map
  (+my-local-leader-def
    :keymaps 'deft-mode-map
    :states '(normal visual motion)
    "A" #'deft-archive-file
    "n" #'deft-new-file
    "f" #'deft-filter
    "d" #'deft-delete-file
    "g" #'deft-refresh
    )
  
  ;; org mode map
  (+my-local-leader-def
    :keymaps 'org-mode-map
    :states '(normal visual motion)
    "e" #'org-export-dispatch
    "j" #'org-present-next
    "k" #'org-present-prev
    "q" #'org-present-quit
    "t" #'org-todo
    "p" #'org-priority
    )
  )

(provide 'init-evil)
