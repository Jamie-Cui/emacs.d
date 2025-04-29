;;; -*- lexical-binding: t; -*-

;; -----------------------------------------------------------
;; Emacs native configurations
;; -----------------------------------------------------------

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "28.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; setup 
(when (not jc-emacs-directory)
  (setq jc-emacs-directory "~/Desktop/emacs.d"))

;; load theme
(load-theme 'wombat t)

;; defining your own
;; macos: ~/Library/Mobile Documents/com~apple~CloudDocs/org-root
;; linux: ~/org-root
;; it's recommended to symlink your remote file here
;; ln -s ~/Library/Mobile\ Documents/com\~apple\~CloudDocs/org-root .
(defvar +my-org-root-dir "~/org-root")
(make-directory (concat +my-org-root-dir "/roam") t)
(make-directory (concat +my-org-root-dir "/journal") t)
(make-directory (concat +my-org-root-dir "/deft") t)
(make-directory (concat (file-name-directory user-init-file) "/bin") t)

;; stop makding ~ files!
(setq make-backup-files nil) 

;; disable electric-indent-mode, forever
(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; do not show me native-comp warning and erros
(setq native-comp-async-report-warnings-errors 'silent)

;; set default font
(set-frame-font "0xProto Nerd Font Mono 14" nil t)

;; disable certain things
(menu-bar-mode 0)
(tool-bar-mode 0)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(setq ring-bell-function 'ignore)

;; word wrap
(global-visual-line-mode 1)
(setq word-wrap-by-category t)

;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(dolist (mode '(pdf-view-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                imenu-list-minor-mode-hook
                imenu-list-major-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))
(setq-default display-line-numbers-type 'relative)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq tab-always-indent t) ;  TAB just indents the current line

;; allow use minibuffer inside minibuffer
(setq enable-recursive-minibuffers t)

;; use short answers
(setq use-short-answers t)

;; enable hideshow in all programming modes
(use-package hideshow
  :config
  (add-hook 'prog-mode-hook #'hs-minor-mode))

;; column
(setq fill-column 80)

;; dired hide .. and .
(add-hook 'dired-mode-hook 'dired-omit-mode)

;; add auto-mdoe list
(let* ((cc-files '(".h" ".cc" ".cpp" ".hpp" ".c"))
       (cc-regexp (concat (regexp-opt cc-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons cc-regexp 'c++-ts-mode)))

(let* ((rust-files '(".rs"))
       (rust-regexp (concat (regexp-opt rust-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons rust-regexp 'rust-ts-mode)))

(let* ((rust-files '("Cargo.lock"))
       (rust-regexp (concat (regexp-opt rust-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons rust-regexp 'conf-toml-mode)))

;; HACK setup environment
;; see: https://www.emacswiki.org/emacs/ExecPath
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
  that used by the user's shell.

  This is particularly useful under Mac OS X and macOS, where GUI
  apps are not started from a shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" "" (shell-command-to-string
                          "$SHELL --login -c 'echo $PATH'"
                          ))))
    ;; (message path-from-shell)
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; run this setup function
(set-exec-path-from-shell-PATH)

;; after all emacs built-in pacakges are loaded
(use-package emacs
  :custom
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.
  ;; Corfu ;; commands are hidden, since they are not used via M-x.
  ;; This setting is ;; useful beyond Corfu.
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  )

;; HACK for vertico
;; Persist history over Emacs restarts.
;; Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package dired-x
  :config
  (setq-default dired-dwim-target t)
  (setq dired-listing-switches "-alh")
  )

(use-package org
  :config
  (add-to-list 'org-latex-packages-alist
               '("lambda, advantage, operators, sets, adversary, landau,\
 probability, notions, logic, ff, mm, primitives, events, complexity, oracles,\
 asymptotics, keys" "cryptocode" t))
  (add-to-list 'org-latex-packages-alist
               '("" "booktabs" t))
  (setq org-log-done t)
  (setq org-src-window-setup 'current-window)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-confirm-babel-evaluate nil) ; don't ask, just do it
  (setq org-startup-with-inline-images t)
  (add-to-list 'org-export-backends 'beamer)

  (org-babel-do-load-languages
   'org-babel-load-languages
   ;; this line activates plantuml
   '((C . t) ; c, c++, and D
     (shell . t)
     (latex . t)
     )))

(use-package compile
  :custom
  (compilation-auto-jump-to-first-error 'if-location-known)
  (compilation-scroll-output t))

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; -----------------------------------------------------------
;; (my) emacs core thirdparty configurations
;; -----------------------------------------------------------

;; Enable package
(require 'package)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("nongnu"   . "http://elpa.nongnu.org/nongnu/")
        ("org"   . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; make sure package-refresh-contents will only run once
(when (not package-archive-contents)
  (package-refresh-contents))

(defun +ensure-packages-installed (packages-alist)
  "Make sure the given package is installed."
  (dolist (p packages-alist)
    (unless (package-installed-p p)
      (package-install p))))

(+ensure-packages-installed
 '(
   ;; evil-related
   evil
   evil-collection
   undo-tree
   undo-fu
   evil-multiedit
   evil-mc
   evil-goggles
   evil-nerd-commenter
   evil-args
   evil-escape ; quit everything with C-g!
   general ; more convenient way of defining keys
   ;; org-related pacakages
   evil-org
   org-download
   org-superstar
   org-roam
   org-appear
   ;; show key helps (it's builtin with emacs > 30)
   which-key
   consult
   ;; show helps of fun, key, mode
   helpful
   ;; search engine
   vertico
   orderless
   ;; project engine
   projectile
   ;; complete engine
   corfu
   ;; better terminal emulater
   vterm
   ;; the killer app: git ui
   magit
   ;; lsp
   eglot
   consult-eglot
   flycheck-eglot
   ;; highlight todo keywords
   hl-todo
   ;; search tool based on ripgrep
   rg
   ;; citar
   citar
   ;; better error checking
   flycheck
   flycheck-popup-tip
   ;; better dired
   dirvish
   ;; better place to write diaries
   org-journal
   ;; tiling windown manager
   edwina
   ;; dashboard at startup
   dashboard
   ;; icons
   nerd-icons
   ;; modeline
   doom-modeline
   ;; cpplint
   flycheck-google-cpplint
   ;; bazel-mode
   bazel
   ;; protobuf-mdoe
   protobuf-mode
   ;; meson-mode
   meson-mode
   ;; allow drawing
   plantuml-mode
   ;; preview org math
   xenops
   ;; show key frenquency
   keyfreq
   ;; llm client
   gptel
   ;; export org code in colors
   engrave-faces
   ;; adds marginalia to the minibuffer completions
   marginalia
   ;; make line-break look nicer
   page-break-lines
   ;; markdown mode
   markdown-mode
   ;; Colorize color names in buffers
   rainbow-mode
   ;; cmake
   cmake-mode
   ;; code auto formating
   apheleia
   ;; Emacs Mini-Buffer Actions Rooted in Keymaps
   embark
   embark-consult
   ;; Chinese input
   rime
   ;; colorful compilation output
   ansi-color
   ;; make eldoc looks nicer
   eldoc-box
   ;; pdf tools
   pdf-tools
   ;; latex support 
   auctex
   ;; deft for note taking
   deft
   ;; better snippet
   yasnippet
   ;; mu4e evil key bindings
   ;; evil-mu4e
   ;; chinese spacing
   pangu-spacing
   ;; workspace
   perspective
   ))

;; ------------------------------------------------------------------
;; TODO
;; ------------------------------------------------------------------


(use-package perspective
  :custom 
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode)
  )
(use-package org-appear
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))


;; see: https://github.com/doomemacs/doomemacs/blob/da32e8e6f233a80d54d51964d21c4b46b000323b/modules/editor/evil/config.el#L324C1-L341C42
(use-package evil-escape
  :after (:and evil general)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence nil
        evil-escape-delay 0.15)
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
   "C-g" #'evil-escape
   )
  )

(use-package evil-args
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

(use-package pangu-spacing
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor nil)
  (add-hook 'org-mode-hook
            '(lambda ()
               (set (make-local-variable
                     'pangu-spacing-real-insert-separtor) t)))
  )

(use-package gptel
  :config
  (setq gptel-model   'deepseek-r1
        gptel-default-mode 'org-mode
        gptel-org-branching-context 't
        gptel-log-level 'info
        gptel-backend
        (gptel-make-deepseek "DeepSeek"
          :host "dashscope.aliyuncs.com/compatible-mode/v1"
          :endpoint "/chat/completions"
          :stream t
          :key "**************************"
          :models '(deepseek-r1)))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "** @jc\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "** @ai\n"))

(use-package auctex)

;;(use-package evil-mu4e
;;  :after evil)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  ;; TODO
  (let ((my-yas-dir (concat jc-emacs-directory "/snippets/")))
    (add-to-list 'yas-snippet-dirs my-yas-dir))
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  )

;; ------------------------------------------------------------------
;; DONE
;; ------------------------------------------------------------------

(use-package cmake-mode
  :config
  (defun +my-modify-cmake-mode-syntax-table ()
    (modify-syntax-entry ?/ "-" cmake-mode-syntax-table))
  (add-hook 'cmake-mode-hook #'+my-modify-cmake-mode-syntax-table)
  )

(use-package engrave-faces)

(use-package ox-latex
  :after (:and engrave-faces citar)
  :custom
  (org-export-with-toc nil)
  :config
  (setq org-latex-src-block-backend 'engraved)
  (setq org-latex-engraved-theme 't)
  ;; this var is used by org-export
  (add-to-list 'org-cite-global-bibliography (concat +my-org-root-dir "/zotero_all.bib"))
  )

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-loader-install)
  (add-hook
   'org-mode-hook
   '(lambda ()
      (delete '("\\.pdf\\'" . default) org-file-apps)
      (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdftools-open))))
  )

(use-package deft
  :after general
  :config
  (setq deft-default-extension "org")
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename nil)
  (setq deft-auto-save-interval -1.0) ; disable auto-save
  (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  (setq deft-directory (concat +my-org-root-dir "/deft"))
  )

(use-package eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :after general
  :custom
  (dired-listing-switches (purecopy "-alh --group-directories-first"))
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  (general-define-key
   :states 'normal
   :keymaps 'dirvish-mode-map
   "TAB" #'dirvish-subtree-toggle
   "h"   #'dired-up-directory
   "l"   #'dired-find-file
   )
  )

(use-package iedit
  :init
  ;; Fix conflict with embark.
  (setq iedit-toggle-key-default nil))

(use-package embark
  :ensure t
  :bind
  (("C-;" . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package plantuml-mode
  :after org
  :custom 
  (org-plantuml-jar-path plantuml-jar-path)
  :config
  (setq plantuml-jar-path
        (concat (file-name-directory user-init-file) "bin/plantuml.jar"))
  (setq plantuml-default-exec-mode 'executable)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   ;; this line activates plantuml
   '((plantuml . t))))

(use-package xenops
  :config
  (add-hook 'org-mode-hook #'xenops-mode)
  (setq xenops-math-image-current-scale-factor 1.2)
  (setq xenops-math-image-margin 0)
  ;; (setq xelatex-dvisvgm-preview '(xelatex-dvisvgm :programs ("xelatex" "dvisvgm")
  ;;                                                 :description "dvi > svg"
  ;;                                                 :message "you need to install lualatex and dvisvgm."
  ;;                                                 :image-input-type "dvi"
  ;;                                                 :image-output-type "svg"
  ;;                                                 :image-size-adjust (2.0 . 2.0)
  ;;                                                 :latex-compiler
  ;;                                                 ("xelatex --interaction=nonstopmode --shell-escape --output-format=dvi --output-directory=%o %f")
  ;;                                                 :image-converter
  ;;                                                 ("dvisvgm %f -n -b min -c %S -o %O")))
  ;; (add-to-list 'xenops-math-latex-process-alist xelatex-dvisvgm-preview)
  ;; (setq xenops-math-latex-process 'xelatex-dvisvgm)
  ;; HACK error from xenops with org>9.7
  ;; https://github.com/syl20bnr/spacemacs/issues/16577
  ;; https://github.com/dandavison/xenops/pull/74/files
  ;; https://github.com/dandavison/xenops/issues/73
  (defun fn/xenops-src-parse-at-point ()
    (-if-let* ((element (xenops-parse-element-at-point 'src))
               (org-babel-info
                (xenops-src-do-in-org-mode
                 (org-babel-get-src-block-info 'light (org-element-context)))))
        (xenops-util-plist-update
         element
         :type 'src
         :language (nth 0 org-babel-info)
         :org-babel-info org-babel-info)))

  (advice-add 'xenops-src-parse-at-point
              :override 'fn/xenops-src-parse-at-point)
  )

(use-package org-journal
  :custom
  (org-journal-dir (concat +my-org-root-dir "/journal"))
  (org-journal-find-file-fn 'find-file)
  )

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  )

(use-package org-download
  :config
  ;; see: https://www.emacswiki.org/emacs/BufferLocalVariable
  (setq-default org-download-image-dir "img")
  (setq-default org-download-heading-lvl nil) ; no headings
  (setq org-download-method 'directory)
  (setq org-download-image-org-width 500)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function
        #'org-download-link-format-function-default))

(use-package citar
  :config
  (add-to-list 'citar-bibliography (concat +my-org-root-dir "/zotero_all.bib"))
  (add-to-list 'citar-notes-paths (concat +my-org-root-dir "/roam"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))


(use-package flycheck-popup-tip
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
  )

(use-package apheleia
  :config
  (apheleia-global-mode +1)
  ;; HACK use elgot-format when possible
  ;; https://github.com/radian-software/apheleia/issues/153#issuecomment-1446651497
  (cl-defun apheleia-indent-eglot-managed-buffer
      (&key buffer scratch callback &allow-other-keys)
    "Copy BUFFER to SCRATCH, then format scratch, then call CALLBACK."
    (with-current-buffer scratch
      (setq-local eglot--cached-server
                  (with-current-buffer buffer
                    (eglot-current-server)))
      (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
        (eglot-format-buffer))
      (funcall callback)))
  (add-to-list 'apheleia-formatters
               '(eglot-managed . apheleia-indent-eglot-managed-buffer))
  ;; HACK add all eglot-ensured modes 
  ;; This determines what formatter to use in buffers without a
  ;; setting for apheleia-formatter. The keys are major mode
  ;; (add-to-list 'apheleia-mode-alist '(cc-mode-hook . eglot-managed))
  ;; (add-to-list 'apheleia-mode-alist '(c++-mode-hook . eglot-managed))
  ;; (add-to-list 'apheleia-mode-alist '(c-mode-hook . eglot-managed))
  ;; (add-to-list 'apheleia-mode-alist '(c-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(c++-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(rust-ts-mode-hook . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(cmake-mode . cmake-format))
  )

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; flycheck has performace issues, make it less automate
  (flycheck-check-syntax-automatically '(save mode-enable idle-change))
  (flycheck-idle-change-delay 4)
  )

(use-package evil-nerd-commenter
  :after evil)

(use-package rainbow-mode
  :hook (emacs-lisp-mode text-mode lisp-mode cc-mode cmake-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode 1))

(use-package flycheck-google-cpplint
  :ensure t
  :custom
  (flycheck-c/c++-googlelint-executable "cpplint")
  (flycheck-googlelint-verbose "0")
  (flycheck-cppcheck-standards "c++17")
  (flycheck-googlelint-linelength "80")
  (flycheck-googlelint-filter
   (concat
    "-whitespace,"
    "-whitespace/braces,"
    "-whitespace/indent,"
    "-build/include_order,"
    "-build/header_guard,"
    "-runtime/reference,"
    )))

(use-package flycheck-eglot
  :ensure t
  :after (:and flycheck eglot)
  :custom
  (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1)
  ;; see: https://github.com/kkholst/.doom.d/blob/main/config.org
  ;; We need to tweak a little bit to make cpplint and eglot to work
  ;; together.
  ;; see: https://melpa.org/#/flycheck-eglot
  ;;
  ;; see: https://github.com/flycheck/flycheck-eglot
  ;; By default, the Flycheck-Eglot considers the Eglot to be the only
  ;; provider of syntax checks. Other Flycheck checkers are ignored.
  ;; There is a variable `flycheck-eglot-exclusive' that controls this
  ;; You can override it system wide or for some major modes.
  ;;
  (flycheck-add-next-checker 'eglot-check
                             '(warning . c/c++-googlelint))
  )

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.
  ;; To make the binding ;; available in the *Completions* buffer,
  ;; add it to the `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package
  ;; such that ;; the mode gets enabled right away. Note that this
  ;; forces loading the package.
  (marginalia-mode))

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :config
  (setq doom-modeline-height 1) ; optional
  ;; (if (facep 'mode-line-active)
  ;;     (set-face-attribute 'mode-line-active nil :family "0xProto Nerd Font Mono 14") ; For 29+
  ;;   (set-face-attribute 'mode-line nil :family "0xProto Nerd Font Mono 14"))
  ;; (set-face-attribute 'mode-line-inactive nil :family "0xProto Nerd Font Mono 14")
  (doom-modeline-mode 1)
  )

(use-package nerd-icons)

(use-package dashboard
  :ensure t
  :custom
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook)

  ;; HACK from https://github.com/emacs-dashboard/emacs-dashboard/issues/153#issuecomment-714406661
  (defvar my-banners-dir (concat jc-emacs-directory "/data/"))
  (defun install-banners ()
    "Copy all files under under banners directory to dashboard banners directory"
    (when (boundp 'dashboard-banners-directory)
      (copy-directory my-banners-dir dashboard-banners-directory nil nil t)))
  (install-banners)

  ;; Set the title  
  (setq dashboard-banner-logo-title "It's possible to build a cabin with no foundations, but not a lasting building.")
  ;; (setq dashboard-page-separator "\n\f\n")

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-startup-banner 4) ; 4 means using 4.txt
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)))
  (setq dashboard-projects-backend 'projectile)
  (setq initial-buffer-choice (lambda() (dashboard-open)))
  (setq dashboard-after-initialize-hook (lambda() (dashboard-open)))
  )

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

(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory (concat +my-org-root-dir "/roam"))

  ;; If you're using a vertical completion framework, you might want
  ;; a more informative completion interface
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package hl-todo
  :config
  (global-hl-todo-mode 1)
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("DONE" success bold)
          ("BUG" error bold)
          )))

(use-package corfu
  :ensure t
  :init
  :config
  (global-corfu-mode)
  ;; Enable auto completion and configure quitting
  (setq corfu-auto t
        corfu-cycle t
        corfu-preview-current 'nil ; do not insert unless i select it
        corfu-preselect 'nil ; do not preselect anything
        corfu-quit-no-match 'separator) ;; or t
  )

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (partial-completion))))))

(use-package consult-eglot)

(use-package eglot
  :ensure t
  :config
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (setq eglot-confirm-server-initiated-edits nil)
  )

(use-package magit
  :ensure t
  :after evil-collection
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (general-define-key
   :keymaps 'smerge-mode-map
   "C-c C-c"     #'smerge-keep-current))

;; projectile
(use-package vterm
  :ensure t
  :config
  (setq vterm-mode-hook (lambda() (display-line-numbers-mode -1)))
  )

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  )

;; helpful
(use-package helpful)

;; which-key
(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1))

;; vertico
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first) 
  (vertico-count 17)
  )

;; consult
(use-package consult
  :custom
  (consult-preview-max-count 17)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; my/command-wrapping-consult    ;; disable auto previews inside my command
   :preview-key '(:debounce 1 any) ;; Option 1: Delay preview
   ;; :preview-key "M-.")            ;; Option 2: Manual preview
   )
  )

;; evil
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  ;; do not save history by default
  (setq undo-tree-auto-save-history nil)

  )
(use-package undo-fu)

(use-package evil
  :ensure t
  :after (:and undo-tree undo-fu)
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

  ;; use undo-tree
  (evil-set-undo-system 'undo-tree)

  (evil-mode 1)
  )

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(use-package general
  :after (:and evil evil-mc)
  :config
  (defconst my-leader "SPC")
  (defconst my-local-leader "SPC m")

  (general-create-definer +my-leader-def
    :prefix my-leader)

  (general-create-definer +my-local-leader-def
    :prefix my-local-leader)

  (general-define-key
   :states '(insert replace normal visual operator)
   "C-g" #'evil-escape
   )
  ;; ** keybindings that should not be overriden
  (general-define-key
   :keymaps 'override
   "M-f"     #'consult-line
   "M-y"     #'yas-expand
   "M-s"     #'save-buffer
   "M-c"     #'evil-yank
   "M-v"     #'evil-paste-before
   "M-/"     #'evilnc-comment-or-uncomment-lines
   "M-z"     #'evil-undo
   "M-Z"     #'evil-redo
   "M-a"     #'mark-whole-buffer
   "C-u"     #'evil-scroll-up
   "C-d"     #'evil-scroll-down
   "C-="     #'text-scale-increase
   "C--"     #'text-scale-decrease
   "C-SPC"   #'toggle-input-method
   "C-/"     #'persp-switch
   "C-h"     #'persp-prev
   "C-l"     #'persp-next
   )

  ;; ** Global Keybindings
  (+my-leader-def
    :states 'normal
    :keymaps 'override ; prevent from being override
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
    ;; buffeer-related key bindings
    "b" '(:ignore t :which-key "buffer")
    "bn"     #'evil-buffer-new
    "bd"     #'kill-current-buffer
    "br"     #'revert-buffer-no-confirm
    "bc"     #'clean-buffer-list
    "bo"     #'persp-kill-other-buffers
    ;; open-related key bindings
    "o" '(:ignore t :which-key "open")
    "ot"     #'vterm
    "oT"     #'projectile-run-vterm
    "od"     #'dired-jump
    "oD"     #'projectile-dired
    "og"     #'magit-status-quick
    "oG"     #'magit-project-status
    ;; project-related key bindings
    "p" '(:ignore t :which-key "project")
    "pa"     #'projectile-add-known-project
    "pd"     #'projectile-remove-known-project
    "pp"     #'projectile-switch-project
    "pC"     #'projectile-configure-project
    "pc"     #'projectile-compile-project
    "pt"     #'projectile-test-project
    "pr"     #'projectile-run-project
    "pq"     #'projectile-kill-buffers
    "pi"     #'projectile-invalidate-cache
    "pf"     #'consult-ripgrep
    "po"     #'find-sibling-file
    ;; note functions
    "n" '(:ignore t :which-key "note")
    "n@"      #'citar-insert-citation
    "ny"      #'org-store-link
    "np"      #'org-insert-link
    "ne"      #'org-export-dispatch
    "nd"      #'deft
    "nj"      #'org-journal-new-entry
    "nrf"     #'org-roam-node-find
    "nri"     #'org-roam-node-insert
    "nrs"     #'org-roam-db-sync
    "nb"     #'citar-open
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
    "tf"     #'toggle-frame-fullscreen
    "tt"     #'toggle-truncate-lines
    "tc"     #'display-fill-column-indicator-mode
    ;; code
    "c" '(:ignore t :which-key "code")
    "cx"     #'list-flycheck-errors
    "ca"     #'eglot-code-actions
    "cr"     #'eglot-rename
    "cf"     #'eglot-format-buffer
    "cj"     #'consult-eglot-symbols
    ;; search
    "s" '(:ignore t :which-key "search")
    "si"     #'consult-imenu
    ;; other
    "RET"    #'gptel
    "x"      #'scratch-buffer
    "."      #'find-file
    "<"      #'consult-buffer
    "/"      #'consult-ripgrep
    "TAB"    #'evil-switch-to-windows-last-buffer
    "SPC"    #'projectile-find-file
    )

  ;; deft mode map
  (+my-local-leader-def
    :keymaps 'deft-mode-map
    :states '(normal visual)
    "A" #'deft-archive-file
    "n" #'deft-new-file
    "f" #'deft-filter
    "d" #'deft-delete-file
    "g" #'deft-refresh
    )
  )


;;; evil-collection
(use-package evil-collection
  :preface
  (setq evil-want-keybinding nil)
  :after evil
  :config
  ;; make sure the follwing key bindings always work
  (evil-collection-init)
  )

;;;
(use-package cc-mode
  :config
  (add-to-list 'find-sibling-rules '("/\\([^/]+\\)\\.c\\(c\\|pp\\)?\\'" "\\1.h\\(h\\|pp\\)?\\'"))
  (add-to-list 'find-sibling-rules '("/\\([^/]+\\)\\.h\\(h\\|pp\\)?\\'" "\\1.c\\(c\\|pp\\)?\\'"))
  )

;; -----------------------------------------------------------
;; System-specific configurations
;; -----------------------------------------------------------

;; HACK for mac only
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (add-to-list 'default-frame-alist '(undecorated . t)) 

  (use-package vterm
    :config
    (setq vterm-shell 'zsh)))

;; HACK for linux and wsl
(when (eq system-type 'linux)
  ;; linux use rime
  (use-package rime
    :ensure t
    :config
    (setq default-input-method "rime")
    (setq rime-show-candidate 'popup)))

;; HACK for windows wsl2
(when (getenv "WSLENV")
  ;; from: https://gist.github.com/minorugh/1770a6aa93df5fe55f70b4d72091ff76
  ;; Emacs on WSL open links in Windows web browser
  ;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic)))
  
  ;; declare new functions
  (defun cp-current-file-to-windows()
    "Copy the current file to windows"
    (interactive)
    (let ((dest-path (concat "~/Desktop/tmp/" (format-time-string "%Y-%m-%d") "/")))
      (when buffer-file-name
        (make-directory dest-path 'parents)
        (message (concat "cp -r " buffer-file-name " " dest-path))
        (shell-command (concat "cp -r " buffer-file-name " " dest-path)))))
  
  ;; org-download from windows clipboard
  (use-package org-download
    :ensure t
    :config
    (setq org-download-screenshot-method
          "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\""))

  ;; wsl use rime
  (use-package rime
    :ensure t
    :config
    (setq default-input-method "rime")
    (setq rime-show-candidate 'popup)))

(evil-escape-mode)
