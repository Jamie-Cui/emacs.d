;;; keys.el --- keybindings and leader map -*- lexical-binding: t -*-
;;; Commentary:
;; which-key, general.el and the SPC leader map.  Loaded last so that
;; commands provided by other modules are defined before being bound.
;;; Code:

(require 'init-input)

;; Named functions for disabled keybindings (better debugging/profiling)
(defun +keys/disabled-M-u ()
  "Placeholder for disabled M-u binding."
  (interactive)
  (message "M-u is disabled!"))

(defun +keys/disabled-M-q ()
  "Placeholder for disabled M-q binding (reserved for kill app)."
  (interactive)
  (message "M-q is disabled!"))

(defun +keys/disabled-M-Q ()
  "Placeholder for disabled M-Q binding (reserved for lock screen)."
  (interactive)
  (message "M-Q is disabled!"))

(defun +keys/disabled-M-k ()
  "Placeholder for disabled M-k binding."
  (interactive)
  (message "M-k is disabled!"))

(defun +keys/persp-prev-and-show ()
  "Switch to previous perspective and show name."
  (interactive)
  (persp-prev)
  (+project/show-name-in-echo))

(defun +keys/persp-next-and-show ()
  "Switch to next perspective and show name."
  (interactive)
  (persp-next)
  (+project/show-name-in-echo))

(defun +keys/revert-buffer-no-confirm ()
  "Revert buffer without confirmation when the buffer is revertible."
  (interactive)
  (condition-case err
      (revert-buffer t t)
    (error
     (if (equal (cadr err) "Buffer does not seem to be associated with any file")
         (user-error "Buffer is not associated with a file")
       (signal (car err) (cdr err))))))

(defun +keys/magit-status-quick ()
  "Show Magit status quickly, prompting outside Git repositories."
  (interactive)
  (condition-case nil
      (call-interactively #'magit-status-quick)
    (magit-outside-git-repo
     (call-interactively #'magit-status))))

(defun +keys/persp-kill-current ()
  "Kill the current perspective."
  (interactive)
  (persp-kill (persp-current-name)))

(defun +keys/find-private-config ()
  "Find private emacs config."
  (interactive)
  (projectile-switch-project-by-name +emacs/repo-directory))

(defun +keys/find-user-emacs-config ()
  "Find private emacs config in user-emacs-directory."
  (interactive)
  (when (not (projectile-project-p user-emacs-directory))
    (magit-call-git "init" (magit-convert-filename-for-git
                            (expand-file-name user-emacs-directory)))
    (projectile-discover-projects-in-directory user-emacs-directory))
  (projectile-switch-project-by-name user-emacs-directory))

(use-package which-key
  :ensure t
  :custom
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 3)
  (which-key-side-window-slot -10)
  (which-key-add-column-padding 1)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

;; ------------------------------------------------------------------
;; DONE Key Bindings
;;
;; general
;; ------------------------------------------------------------------

(use-package general
  :ensure t
  :after (evil evil-mc which-key evil-collection)
  :config
  (defconst my-leader "SPC")

  (defun +my-leader-def (&rest args)
    "Define key bindings under `my-leader' with `general-define-key'."
    (apply #'general-define-key :prefix my-leader args))

  ;; HACK Separate TAB and C-i in GUI emacs
  ;; FIXME the following code breaks citar-insert-citation when
  ;; citar-select-multiple is enabled
  (setopt evil-want-C-i-jump t)
  (define-key input-decode-map [(control ?i)] [control-i])
  (general-define-key
   :keymaps 'evil-motion-state-map
   "C-i" nil
   [control-i] 'evil-jump-forward
   "[["      #'flycheck-previous-error
   "]]"      #'flycheck-next-error
   )

  ;; ** keybindings that should not be overriden
  (general-define-key
   :keymaps 'override
   "M-RET"   #'completion-at-point ;; FIXME org-mode should not use that
   "C-M-<return>" #'completion-at-point ; alternative
   ;; HACK change default toggle input method (this may has conflict with system-wide kbd)
   "C-SPC"   #'toggle-input-method ; alternative
   ;; more-frequent commands
   "M-y"     #'yas-expand
   "M-p"     #'+prog/compile-with-no-preset ; just like vscode
   "M-w"     #'evil-avy-goto-char-timer ; quick find edit (point)?
   "M-n"     #'narrow-to-region ; when done, M-x Widen
   "M-i"     #'consult-imenu
   "M-d"     #'evil-multiedit-match-symbol-and-next  ; dup
   ;; mac-like binding
   "M-f"     #'consult-line ; search like mac
   "M-a"     #'mark-whole-buffer ; select like mac
   "M-s"     #'save-buffer ; save like mac
   "M-c"     #'evil-yank ; REVIEW copy like mac
   "M-/"     #'evilnc-comment-or-uncomment-lines
   "M-v"     #'evil-paste-after ; paste like mac
   ;; disabled
   "M-u"     #'+keys/disabled-M-u
   "M-q"     #'+keys/disabled-M-q ; reserved for kill app
   "M-Q"     #'+keys/disabled-M-Q ; reserved for lock screen
   "M-k"     #'+keys/disabled-M-k
   "C-h"     #'+keys/persp-prev-and-show
   "C-l"     #'+keys/persp-next-and-show
   ;; emacs binding
   ;; NOTE those bindings are set to global since most of time, mac and terminal adopts those bindings
   "C-a"     #'evil-first-non-blank ; like "^" in vim
   "C-e"     #'end-of-line ; like "$" in vim, DO NOT use evil-end-of-line
   ;; NOTE the following kbds are avaliable when in insert mode
   ;; "C-f"     #'forward-char ; native
   ;; "C-b"     #'backward-char ; native
   ;; "C-w"     #'evil-delete-backward-word ; native
   ;; vim binding
   "C-u"     #'+input/evil-scroll-up-dwim
   "C-d"     #'+input/evil-scroll-down-dwim
   ;; "C-i"     #'evil-jump-forward ; FIXME C-i is tab in tui
   ;; "C-o"     #'evil-jump-backward
   "C--"     #'+input/text-scale-decrease-dwim ; buffer-local
   "C-="     #'+input/text-scale-increase-dwim ; buffer-local
   "C-0"     #'(lambda () (interactive) (text-scale-adjust 0))
   "C-<wheel-up>" #'+input/cnfonts-mouse-wheel-dwim
   "C-<wheel-down>" #'+input/cnfonts-mouse-wheel-dwim
   "C-<mouse-4>" #'+input/cnfonts-mouse-wheel-dwim
   "C-<mouse-5>" #'+input/cnfonts-mouse-wheel-dwim
   "<pinch>" #'+input/pinch-dwim
   "<magnify-up>" #'+input/pinch-dwim
   "<magnify-down>" #'+input/pinch-dwim
   "C-M--"   #'cnfonts-decrease-fontsize ; global
   "C-M-="   #'cnfonts-increase-fontsize ; global
   "C-M-0"   #'cnfonts-reset-fontsize ; global
   )

  ;; ** Global Keybindings
  (+my-leader-def
   :states '(normal visual motion)
   :keymaps 'override ; prevent from being override
   ;; application keys
   "A"      #'agent-shell ;; A -> Agent Shell
   "G"      #'gptel  ;; G -> Gptel
   "D"      #'docker
   ;; "T"      #'telega ;; T -> Telegram
   ;; most-frequency keys
   "."      #'find-file
   "<"      #'consult-buffer
   ","      #'consult-project-buffer
   "/"      #'+completion/project-search
   "TAB"    #'evil-switch-to-windows-last-buffer
   "SPC"    #'projectile-find-file
   ;; action-related key bindings
   "a" '(:ignore t :which-key "actions")
   "a RET"  #'embark-dwim
   "aa"     #'embark-act
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
   "wd"     #'delete-window
   "wr"     #'balance-windows
   "ws"     #'+evil/window-split-and-follow
   "wv"     #'+evil/window-vsplit-and-follow
   "wm"     #'delete-other-windows
   "wR"     #'redraw-display
   ;; yank-related key bindings
   "y" '(:ignore t :which-key "yank")
   "yf"     #'+editor/copy-buffer-file-name ; copy file name
   "ya"     #'+editor/copy-ref-dwim ; copy dwim
   ;; buffer-related key bindings
   "b" '(:ignore t :which-key "buffer")
   "ba"     #'evil-buffer-new
   "bn"     #'evil-buffer-new ; alias
   "bd"     #'kill-current-buffer
   "bs"     #'save-buffer
   "bS"     #'+editor/save-all-buffers
   "br"     #'+keys/revert-buffer-no-confirm
   "j" '(:ignore t :which-key "jump (bookmark)")
   "j RET"  #'consult-bookmark
   "jj"     #'consult-bookmark
   "ja"     #'bookmark-set
   "jx"     #'bookmark-delete
   ;; open-related key bindings
   "o" '(:ignore t :which-key "open")
   "oo"     #'crux-open-with
   "oe"     #'elfeed
   "oE"     #'ielm ; elisp repl
   "ob"     #'citar-open
   "oB"     #'ebib
   "og"     #'+keys/magit-status-quick
   "op"     #'proced
   "od"     #'dired-jump
   "oD"     #'+os/explorer-dwim
   "ot"     #'+eshell/new
   "oT"     #'+eat/new
   "ox"     #'scratch-buffer
   "om"     #'popwin:messages
   ;; project-related key bindings
   "p" '(:ignore t :which-key "project")
   "pp"     #'projectile-switch-project
   "pq"     #'+keys/persp-kill-current
   "pa"     #'projectile-add-known-project
   "px"     #'projectile-remove-known-project
   "pg"     #'projectile-cleanup-known-projects
   "pi"     #'projectile-invalidate-cache
   "pc"     #'projectile-compile-project
   "pC"     #'projectile-configure-project
   "pd"     #'projectile-remove-known-project
   "pD"     #'projectile-run-gdb
   "pf"     #'+completion/project-search
   "po"     #'ff-find-related-file
   "pr"     #'projectile-run-project
   "pt"     #'projectile-test-project
   ;; note functions
   "n" '(:ignore t :which-key "note")
   "na"      #'org-agenda-list
   "n@"      #'citar-insert-citation
   "nc"      #'+org-project-capture-select-project
   "nt"      #'org-project-todo-list
   "nj"      #'org-journal-new-entry
   "nl"      #'org-insert-link
   "ny"      #'org-store-link
   "np"      #'+org-project-consult-notes
   "nd"      #'org-deft-org
   "nD"      #'org-deft-tex
   "nq"      #'org-set-tags-command
   "nT" '(:ignore t :which-key "timer")
   "nTa"     #'org-timer-set-timer ; add timer
   "nT RET"  #'org-timer-pause-or-continue ; pause or continue
   "nTd"     #'org-timer-stop ; delete timer
   "nr" '(:ignore t :which-key "org-roam")
   "nra"     #'org-roam-alias-add
   "nrf"     #'org-roam-node-find
   "nri"     #'org-roam-node-insert
   "nrs"     #'org-roam-db-sync
   "nrq"     #'org-roam-tag-add
   "nrc"     #'org-roam-db-clear-all
   "nx" '(:ignore t :which-key "xenops")
   "nxe"     #'xenops-reveal ; edit
   "nxc"     #'xenops-copy-at-point ; yank
   "nxr"     #'xenops-regenerate ; regenerate
   ;; help functions
   "h" '(:ignore t :which-key "help")
   "h RET"  #'helpful-at-point
   "hf"     #'helpful-callable
   "hF"     #'helpful-function
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
   "qR"     #'+emacs/clear-native-compile-cache
   ;; toggles
   "t" '(:ignore t :which-key "toggle")
   "td"     #'toggle-debug-on-error
   "tb"     #'magit-blame-addition
   "tf"     #'toggle-frame-maximized
   "tF"     #'toggle-frame-fullscreen
   "tt"     #'toggle-truncate-lines
   "tn"     #'display-line-numbers-mode
   "ta"     #'+prog/treesit-auto-toggle
   "tm"     #'org-toggle-inline-images
   ;; code (lsp/tags)
   "c" '(:ignore t :which-key "code")
   "c RET"  #'eglot
   "cx"     #'list-flycheck-errors
   "ca"     #'eglot-code-actions
   "cr"     #'eglot-rename
   "ci"     #'eglot-inlay-hints-mode
   "cf"     #'eglot-format-buffer
   "ct"     #'citre-update-this-tags-file
   "cj"     #'consult-eglot-symbols
   "cl"     #'eglot-list-connections
   "cq"     #'eglot-shutdown
   ;; find
   "f" '(:ignore t :which-key "find")
   "fr"     #'consult-recent-file ; find recent file (globally)
   "fb"     #'consult-buffer
   "fB"     #'consult-bookmark ; find recent file (globally)
   ;; FIXME consult-fd is a better alternative, but it seems consult-fd only finds the
   ;; project files, which does not make sense to me here. Since when I called
   ;; consult-find/consult-fd, I want to find files that resides outside of the repo
   "ff"     #'consult-find ; find file (in this directory),
   "fF"     #'consult-locate ; find file (system wide)
   "fm"     #'consult-man
   "fM"     #'consult-woman
   "fh"     #'consult-history ; find history
   "fp"     #'+keys/find-private-config ; find private emacs config
   "fP"     #'+keys/find-user-emacs-config ; find private emacs config in .emacs.d
   )

  ;; occur mode
  (general-define-key
   :keymaps 'occur-mode-map
   "C-c C-p"   #'occur-edit-mode ;; make it behaves the same as wgrep-mode map
   "C-c C-c"   #'occur-mode ;; make it behaves the same as wgrep-mode map
   )

  ;; minibuffer mode
  (general-define-key
   :keymaps 'minibuffer-mode-map
   :states '(insert normal visual motion) ;; all modes
   "C-n"       #'next-history-element ;; make it behaves the same as wgrep-mode map
   "C-p"       #'previous-history-element ;; make it behaves the same as wgrep-mode map
   )

  ;; org-agenda
  (general-define-key
   :keymaps 'org-agenda-mode-map
   :states 'normal ;; all modes
   "q"       #'org-agenda-quit ;; make it behaves the same as wgrep-mode map
   )

  ;; org-project-todo-list
  (general-define-key
   :keymaps 'org-project-todo-list-mode-map
   :states 'normal
   "RET"     #'org-project-todo-list-visit
   "i"       #'org-project-todo-list-edit-action-item
   "o"       #'org-project-todo-list-open-original-file
   "q"       #'quit-window)

  (general-define-key
   :keymaps 'org-project-todo-list-mode-map
   :states '(normal insert)
   "C-c C-a" #'org-project-todo-list-archive
   "C-c C-c" #'org-project-todo-list-commit-edit
   "C-c C-k" #'org-project-todo-list-cancel-edit
   "C-c C-q" #'org-project-todo-list-set-tags
   "C-c C-t" #'org-project-todo-list-toggle-state)

  ;; smerge-mode
  (general-define-key
   :keymaps 'smerge-mode-map
   "C-c C-c"     #'smerge-keep-current)
  )

(provide 'init-keys)
;;; keys.el ends here
