;;; init-kbd.el --- keybindings configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Keybindings setup using general.el with leader key system
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

  ;; HACK Separate TAB and C-i in GUI emacs
  ;; FIXME the following code breaks citar-insert-citation when 
  ;; citar-select-multiple is enabled
  ;; 
  (setopt evil-want-C-i-jump t) 
  (define-key input-decode-map [(control ?i)] [control-i])
  (general-define-key
   :keymaps 'evil-motion-state-map
   "C-i" nil
   [control-i] 'evil-jump-forward)

  ;; ** keybindings that should not be overriden
  (general-define-key
   :keymaps 'override
   "M-RET"   #'completion-at-point ;; FIXME org-mode should not use that
   "C-M-<return>" #'completion-at-point ; alternative
   ;; less-frequent commands
   "M-y"     #'yas-expand 
   "M-n"     #'narrow-to-region
   "M-w"     #'widen
   "M-p"     #'+compile-with-no-preset ; just like vscode
   "M-d"     #'evil-multiedit-match-symbol-and-next ; default
   ;; more-frequent commands
   "M-e"     #'evil-avy-goto-char-2 ; locate
   "M-i"     #'consult-imenu
   "M-d"     #'evil-multiedit-match-symbol-and-next  ; match
   ;; mac-like binding
   "M-f"     #'consult-line ; search like mac
   "M-a"     #'mark-whole-buffer ; select like mac
   "M-s"     #'save-buffer ; save like mac
   "M-c"     #'evil-yank ; REVIEW copy like mac
   "M-/"     #'evilnc-comment-or-uncomment-lines
   "M-v"     #'evil-paste-after ; paste like mac
   ;; disabled
   "M-u"     #'(lambda () (interactive) (message "M-u is disabled!"))
   "M-k"     #'move-text-up
   "M-j"     #'move-text-down
   "C-h"     #'(lambda () (interactive) (persp-prev) (+persp/show-name-in-echo))
   "C-l"     #'(lambda () (interactive) (persp-next) (+persp/show-name-in-echo))
   ;; emacs binding
   ;; NOTE those bindings are set to global since most of time, mac and terminal adopts those bindings
   "C-a"     #'evil-first-non-blank ; like "^" in vim 
   "C-e"     #'evil-end-of-line ; like "$" in vim
   ;; NOTE the following kbds are avaliable when in insert mode
   ;; "C-f"     #'forward-char ; native 
   ;; "C-b"     #'backward-char ; native
   ;; "C-w"     #'evil-delete-backward-word ; native
   ;; vim binding
   "C-u"     #'evil-scroll-up
   "C-d"     #'evil-scroll-down
   ;; "C-i"     #'evil-jump-forward ; FIXME C-i is tab in tui
   ;; "C-o"     #'evil-jump-backward
   "C-="     #'cnfonts-increase-fontsize
   "C--"     #'cnfonts-decrease-fontsize
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
    ;; action-related key bindings
    "a" '(:ignore t :which-key "actions")
    "a RET"  #'embark-dwim
    "aa"     #'embark-act
    "ay"     #'embark-org-copy-link-target
    "aY"     #'embark-org-copy-link-description
    ;; gptel-related key bindings
    "g" '(:ignore t :which-key "gptel")
    "g RET"  #'gptel-send
    "ga"     #'gptel-add
    "gr"     #'gptel-rewrite
    "gq"     #'gptel-context-remove-all
    "gc"     #'gptel-abort
    ;; window-related key bindings
    "w" '(:ignore t :which-key "window")
    "wh"     #'evil-window-left
    "wj"     #'evil-window-down
    "wk"     #'evil-window-up
    "wl"     #'evil-window-right
    "ww"     #'other-window
    "wx"     #'evil-window-exchange
    "wd"     #'delete-window
    "ws"     #'evil-window-split
    "wv"     #'evil-window-vsplit
    "wm"     #'delete-other-windows
    "wr"     #'redraw-display
    ;; buffer-related key bindings
    "b" '(:ignore t :which-key "buffer")
    "ba"     #'evil-buffer-new
    "bn"     #'evil-buffer-new ; alias
    "bd"     #'kill-current-buffer
    "by"     #'+copy-buffer-file-name
    "br"     #'(lambda () (interactive) (revert-buffer t t))
    "B" '(:ignore t :which-key "bookmark")
    "BB"     #'consult-bookmark
    "Bn"     #'bookmark-set
    "Bd"     #'bookmark-delete
    ;; docker-related key bindings
    ;; I do not use that very often
    "D"      #'docker
    ;; open-related key bindings
    "o" '(:ignore t :which-key "open")
    "ob"     #'citar-open ; open/find bib
    "oB"     #'ebib ; edit bib
    "oe"     #'elfeed
    "oE"     #'ielm ; elisp repl
    "og"     #'magit-status-quick
    "op"     #'dired-sidebar-toggle-sidebar
    "od"     #'dired-jump
    "oD"     #'+os-explorer/dwim
    "ot"     #'+eshell/new
    "oT"     #'+eat/new
    "ox"     #'scratch-buffer ; popup
    "om"     #'popwin:messages ; popup
    ;; project-related key bindings
    "p" '(:ignore t :which-key "project")
    "pp"     #'projectile-switch-project
    "pq"     #'(lambda () (interactive) 
                 (persp-kill (persp-current-name)))
    "pa"     #'projectile-add-known-project
    "px"     #'projectile-remove-known-project
    "pg"     #'projectile-cleanup-known-projects
    "pi"     #'projectile-invalidate-cache
    "pc"     #'projectile-compile-project 
    "pC"     #'projectile-configure-project 
    "pd"     #'projectile-remove-known-project
    "pD"     #'projectile-run-gdb
    "pf"     #'+vertico/project-search
    "po"     #'ff-find-related-file
    "pr"     #'projectile-run-project 
    "pt"     #'projectile-test-project 
    ;; note functions
    "n" '(:ignore t :which-key "note")
    "n@"      #'citar-insert-citation ;; insert bib
    "ny"      #'org-store-link
    "np"      #'org-insert-link
    "nd"      #'deft
    "nt"      #'org-todo
    "nj"      #'org-journal-new-entry
    "nr" '(:ignore t :which-key "org-roam")
    "nra"     #'org-roam-alias-add
    "nrf"     #'org-roam-node-find
    "nri"     #'org-roam-node-insert
    "nrs"     #'org-roam-db-sync
    "nrq"     #'org-roam-tag-add
    "nrc"     #'org-roam-db-clear-all
    "nx" '(:ignore t :which-key "xenops")
    "nx RET"  #'xenops-dwim
    "nxe"     #'xenops-reveal-at-point
    "nxy"     #'xenops-copy-at-point
    "nxr"     #'xenops-regenerate-at-point
    "nxR"     #'xenops-regenerate
    ;; help functions
    "h" '(:ignore t :which-key "help")
    "hf"     #'helpful-callable
    "hk"     #'helpful-key
    "hv"     #'helpful-variable
    "hm"     #'describe-mode
    "hp" '(:ignore t :which-key "profiler")
    "hps"    #'profiler-start
    "hpk"    #'profiler-stop
    "hpr"    #'profiler-report
    ;; quit emacs
    "q" '(:ignore t :which-key "quit")
    "qq"     #'save-buffers-kill-terminal
    "qr"     #'restart-emacs
    ;; toggles
    "t" '(:ignore t :which-key "toggle")
    "tb"     #'magit-blame-addition
    "tf"     #'toggle-frame-maximized
    "tF"     #'toggle-frame-fullscreen
    "tt"     #'toggle-truncate-lines
    "tn"     #'display-line-numbers-mode
    "ta"     #'+treesit-auto/toggle
    ;; code (lsp/tags)
    "c" '(:ignore t :which-key "code")
    "cx"     #'list-flycheck-errors
    "ca"     #'eglot-code-actions
    "cr"     #'eglot-rename 
    "cf"     #'eglot-format-buffer
    "ct"     #'citre-update-this-tags-file
    ;; find
    "f" '(:ignore t :which-key "find")
    "fi"     #'consult-citre ; find citre items
    "ff"     #'consult-fd ; find file (in this directory)
    "fF"     #'consult-locate ; find file (system wide)
    "fl"     #'consult-focus-lines ; find lines
    "fL"     #'consult-keep-lines ; find lines
    "fh"     #'consult-history ; find history
    )
  )

(provide 'init-kbd)
