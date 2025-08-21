;;; init-kbd.el --- evil support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-evil)

(+package/ensure-install
 '(
   ;; more convenient way of defining keys
   general 
   ;; consult which key
   which-key
   ))

;; ------------------------------------------------------------------
;; DONE Key Bindings
;; ------------------------------------------------------------------

(use-package which-key
  :ensure t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1))

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

  (defun +compilation/open-projectile-compilation-buffer ()
    (interactive)
    (let ((target-buffer (projectile-compilation-buffer-name "compilation")))
      (if (bufferp (get-buffer target-buffer))
          (display-buffer target-buffer) ;; should handled by popwin
        (call-interactively 'projectile-compile-project))
      ))

  ;; ** keybindings that should not be overriden
  (general-define-key
   :keymaps 'override
   "M-i"     #'completion-at-point
   "M-u"     #'nil
   "M-y"     #'yas-expand
   "M-/"     #'evilnc-comment-or-uncomment-lines
   "M-f"     #'consult-line ; search like mac
   "M-a"     #'mark-whole-buffer ; select like mac
   "M-s"     #'save-buffer ; save like mac
   "M-v"     #'evil-paste-after ; paste like mac
   "M-h"     #'nil
   "M-j"     #'move-text-down
   "M-k"     #'move-text-up
   "M-l"     #'nil
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
    "RET"    #'gptel ;; popup
    "."      #'find-file
    "<"      #'consult-buffer
    ","      #'consult-project-buffer
    "/"      #'+vertico/project-search
    "TAB"    #'evil-switch-to-windows-last-buffer
    "SPC"    #'projectile-find-file
    ;; llm-related key bindings
    "i" '(:ignore t :which-key "intelligence")
    "ir"     #'gptel-rewrite
    "is"     #'gptel-send
    "iq"     #'gptel-abort
    "ia"     #'gptel-add
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
    "wr"     #'redraw-display
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
    ;; open-related key bindings
    "o" '(:ignore t :which-key "open")
    "ot"     #'+eshell/new
    "oT"     #'+eat/new
    "od"     #'dired-jump
    "oD"     #'+os-explorer/dwim
    "oc"     #'+compilation/open-projectile-compilation-buffer
    "oi"     #'gptel
    "oI"     #'claude-code-toggle
    "og"     #'magit-status-quick
    "ox"     #'scratch-buffer ; popup
    "om"     #'popwin:messages ; popup
    ;; project-related key bindings
    "p" '(:ignore t :which-key "project")
    "pa"     #'projectile-add-known-project
    "pD"     #'projectile-remove-known-project
    "pd"     #'projectile-run-gdb
    "pp"     #'projectile-switch-project
    "pC"     #'projectile-configure-project 
    "pc"     #'projectile-compile-project 
    "pt"     #'projectile-test-project 
    "pr"     #'projectile-run-project 
    "pb"     #'consult-project-buffer 
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
    "nbb"     #'citar-open ;; consult for entry
    "nbf"     #'+bibtex/consult-bibtex-file ;; check bib source
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
    "hM"     #'consult-man
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
    "tm"     #'+inhibit-mouse/toggle-mode
    "tM"     #'+org-imgtog/toggle
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
    "ss"     #'consult-locate ;; system wide search
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

(provide 'init-kbd)
