;;; Init-kbd.el --- evil support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-evil)
(require 'which-key)

;; ------------------------------------------------------------------
;; DONE Key Bindings
;;
;; general
;; ------------------------------------------------------------------

(use-package general
  :ensure t
  :after (evil evil-mc which-key)
  :config
  (defconst my-leader "SPC")
  (defconst my-local-leader "SPC m")

  (general-create-definer +my-leader-def
    :prefix my-leader)

  (general-create-definer +my-local-leader-def
    :prefix my-local-leader)

  ;; tweak evil default key bindings
  (general-define-key 
   :keymaps 'override
   :states 'motion
   "gD"      #'citre-jump
   "gR"      #'citre-jump-to-reference)

  ;; ** keybindings that should not be overriden
  (general-define-key
   :keymaps 'override
   "M-i"     #'completion-at-point
   "M-u"     #'(lambda () (interactive) (message "M-u is disabled!"))
   "M-y"     #'yas-expand
   "M-/"     #'evilnc-comment-or-uncomment-lines
   "M-f"     #'consult-line ; search like mac
   "M-a"     #'mark-whole-buffer ; select like mac
   "M-s"     #'save-buffer ; save like mac
   "M-v"     #'evil-paste-after ; paste like mac
   "M-e"     #'er/expand-region ; expand region selection, in case you dont like evil
   "M-h"     #'(lambda () (interactive) (message "M-h is disabled!"))
   "M-j"     #'(lambda () (interactive) (message "M-j is disabled!"))
   "M-J"     #'move-text-down
   "M-k"     #'(lambda () (interactive) (message "M-k is disabled!"))
   "M-K"     #'move-text-up
   "M-l"     #'(lambda () (interactive) (message "M-l is disabled!"))
   "M-n"     #'narrow-to-region
   "M-w"     #'widen
   "C-a"     #'move-beginning-of-line
   "C-e"     #'move-end-of-line
   "C-u"     #'evil-scroll-up
   "C-d"     #'evil-scroll-down
   "C-="     #'cnfonts-increase-fontsize
   "C--"     #'cnfonts-decrease-fontsize
   "C-M-j"   #'(lambda () (interactive) (+persp/show-name-in-echo))
   "C-M-k"   #'(lambda () (interactive) (+persp/show-name-in-echo))
   "C-M-g"   #'(lambda () (interactive) (+persp/show-name-in-echo))
   "C-M-h"   #'(lambda () (interactive) (persp-prev) (+persp/show-name-in-echo))
   "C-M-l"   #'(lambda () (interactive) (persp-next) (+persp/show-name-in-echo))
   )
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
    "ay"     #'embark-org-copy-link-target
    "aY"     #'embark-org-copy-link-description
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
    "br" '(:which-key "revert-buffer")
    "br"     #'(lambda () (interactive) (revert-buffer t t))
    "B" '(:ignore t :which-key "bookmark")
    "BB"     #'consult-bookmark
    "Bn"     #'bookmark-set
    "Bd"     #'bookmark-delete
    ;; docker-related key bindings
    "d"     #'docker
    ;; open-related key bindings
    "o" '(:ignore t :which-key "open")
    "ob"     #'ebib ; edit bib
    "oc" '(:which-key "compile")
    "oc" #'(lambda () (interactive)
             (let* ((compile-command 
                     (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       "")))
               (call-interactively 'compile)))
    "od"     #'dired-jump
    "oD"     #'+os-explorer/dwim
    "oe"     #'elfeed
    "og"     #'magit-status-quick
    "ot"     #'+eshell/new
    "op"     #'dired-sidebar-toggle-sidebar
    "oT" '(:which-key "eat/new")
    "oT"     #'(lambda () (interactive) 
                 (let ((current-prefix-arg '(t)))
                   (call-interactively 'eat)))
    "ox"     #'scratch-buffer ; popup
    "om"     #'popwin:messages ; popup
    ;; project-related key bindings
    "p" '(:ignore t :which-key "project")
    "pa"     #'projectile-add-known-project
    "pb"     #'consult-project-buffer 
    "pc"     #'projectile-compile-project 
    "pC"     #'projectile-configure-project 
    "px"     #'projectile-remove-known-project
    "pd"     #'projectile-run-gdb
    "pf"     #'+vertico/project-search
    "pi"     #'projectile-invalidate-cache
    "po"     #'ff-find-related-file
    "pp"     #'projectile-switch-project
    "pq" '(:which-key "persp/kill-current-workspace")
    "pq"     #'(lambda () (interactive) 
                 (persp-kill (persp-current-name))
                 (+persp/show-name-in-echo))
    "pr"     #'projectile-run-project 
    "pt"     #'projectile-test-project 
    ;; note functions
    "n" '(:ignore t :which-key "note")
    "n@"      #'citar-insert-citation ;; insert bib
    "nb"      #'citar-open 
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
    "hK" '(:which-key "general-describe-keybindings")
    "hK"     #'(lambda () (interactive) 
                 (let* ((current-prefix-arg '(t))) 
                   (call-interactively 'general-describe-keybindings)))
    "hv"     #'helpful-variable
    "hm"     #'describe-mode
    ;; quit emacs
    "q" '(:ignore t :which-key "quit")
    "qq"     #'save-buffers-kill-terminal
    "qr"     #'restart-emacs
    ;; toggles
    "t" '(:ignore t :which-key "toggle")
    "th"     #'hs-hide-level
    "tH"     #'hexl-mode
    "ti"     #'agent-shell-toggle
    "tg"     #'magit-blame-addition
    "tf"     #'toggle-frame-maximized
    "tF"     #'toggle-frame-fullscreen
    "tt"     #'toggle-truncate-lines
    "tn"     #'display-line-numbers-mode
    "tm"     #'+inhibit-mouse/toggle-mode
    "tM"     #'+org-imgtog/toggle
    "ta"     #'+treesit-auto/toggle
    ;; code (lsp)
    "c" '(:ignore t :which-key "code")
    "cx"     #'list-flycheck-errors
    "ca"     #'eglot-code-actions
    "cr"     #'eglot-rename 
    "cf"     #'eglot-format-buffer
    "cj"     #'consult-eglot-symbols
    "ct"     #'citre-create-tags-file
    "cT"     #'citre-update-this-tags-file
    ;; find
    "f" '(:ignore t :which-key "find")
    "fi"     #'consult-imenu ; find item
    "fc"     #'evil-avy-goto-char-timer ; find char
    "fI"     #'consult-citre ; find citre items
    "ff"     #'consult-find ; find file (in this directory)
    "fF"     #'consult-locate ; find file (system wide)
    "fl"     #'consult-focus-lines ; find lines
    "fL"     #'consult-keep-lines ; find lines
    "fh"     #'consult-history ; find history
    )

  ;; deft mode map
  (+my-local-leader-def
    :keymaps 'deft-mode-map
    :states '(normal visual motion)
    "A" #'deft-archive-file
    "n" #'deft-new-file
    "f" #'deft-filter
    "d" #'deft-delete-file
    "g" #'deft-refresh)

  ;; org mode map
  (+my-local-leader-def
    :keymaps 'org-mode-map
    :states '(normal visual motion)
    "e" #'org-export-dispatch
    "j" #'org-present-next
    "k" #'org-present-prev
    "q" #'org-present-quit
    "t" #'org-todo
    "p" #'org-priority))

(provide 'init-kbd)
