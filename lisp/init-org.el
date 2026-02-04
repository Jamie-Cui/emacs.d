;;; init-org.el --- org support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; defining your own
;; macos: ~/Library/Mobile Documents/com~apple~CloudDocs/org-root
;; linux: ~/opt/org-root
;; it's recommended to symlink your remote file here
;; ln -s ~/Library/Mobile\ Documents/com\~apple\~CloudDocs/org-root .

(require 'org)
(require 'org-agenda)
(require 'init-utils)

;; -----------------------------------------------------------
;; DONE org: the built-in package
;; -----------------------------------------------------------

(use-package org-remoteimg
  :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/"))
  :config
  (setq url-cache-directory (concat user-emacs-directory "url/")) ; cache store location
  (setq org-display-remote-inline-images 'cache) ; enable caching
  )

(use-package toc-org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

;; REVIEW no need for this package if we already have org-toggle-inline images
;; (use-package org-imgtog
;;   :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/"))
;;   :hook (org-mode . org-imgtog-mode)
;;   :config
;;   (setq org-imgtog-preview-delay 0.5) ;; wait 0.5 seconds before toggling
;;   (setq org-imgtog-preview-delay-only-remote t)) ;; only delay for remote images

;;; list
(setopt org-list-allow-alphabetical t)

;;; editing
(setopt org-return-follows-link nil)
(setopt org-link-elisp-confirm-function nil)
(setopt org-use-fast-todo-selection 'auto)

;;; agenda
(setopt org-agenda-window-setup 'current-window)

;; make org file find org-journal files (maybe not needed?)
;; (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")

;;; timer
(setopt org-timer-default-timer "20") ; 20 mins
(setopt org-clock-sound t) ; use default sound

;;; ui
(setopt org-log-done 'time) ; when DONE, log the time
(setopt org-image-actual-width nil)
(setopt org-cycle-hide-drawer-startup t)
(setopt org-cycle-hide-block-startup t)
(setopt org-cycle-open-archived-trees nil)

(setopt org-startup-indented t)
(setopt org-startup-folded 'nofold)
(setopt org-startup-with-inline-images t)
(setopt org-startup-with-latex-preview nil)

(setq org-preview-latex-process-alist
      '((xdvisvgm
		 :programs ("xelatex" "dvisvgm")
		 :description "xdv > svg"
		 :image-input-type "xdv"
		 :image-output-type "svg"
		 :image-size-adjust (1.7 . 1.5)
		 :latex-compiler ;; Default `xelatex' as the process previewing LaTeX fragments
		 ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
		 :image-converter ;; Set `dvisvgm' with --exact option
		 ("dvisvgm %f -e -n -b min -c %S -o %O"))
        (ximagemagick
         :programs ("xelatex" "convert")
         :description "pdf > png"
         :image-input-type "pdf"
         :image-output-type "png"
         :image-size-adjust (1.0 . 1.0)
         :latex-compiler
         ("xelatex -interaction nonstopmode -output-directory %o %f")
         :image-converter
         ("magick convert -density %D -trim -antialias %f -quality 300 %O"))
        ))

;; latex to dvisvgm
(setq org-preview-latex-default-process 'ximagemagick)

;; todo keywords
(setopt org-use-fast-todo-selection 'expert)
(setopt org-todo-keywords '((sequence
                             "TODO(t)"
                             "NEXT(n)"
                             "WAIT(w)"
                             "|"
                             "DONE(d)"
                             "KILL(k)")))

;;; org-src
(setopt org-src-fontify-natively t)
(setopt org-src-strip t)
(setopt org-src-preserve-indentation nil)

;;; org-export
(setopt org-latex-tables-booktabs t)
(setopt org-export-dispatch-use-expert-ui t)
(setopt org-export-with-toc nil)
(setopt org-cite-export-processors '((latex biblatex) (beamer natbib) (t basic)))
(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

(add-to-list 'org-export-backends 'beamer)
(add-to-list 'org-export-backends 'markdown)

;;; org-latex
;; NOTE default latex:
;; - font: Computer Modern Unicode (CMU)
;; - font size: 10
(setopt org-latex-compiler "xelatex")
(setopt org-latex t)
(setopt org-latex-src-block-backend 'engraved)
(setopt org-latex-engraved-theme 't)
(setopt org-latex-packages-alist
        '(
          ("" "parskip" t)
          ("" "xeCJK" t)
          ("" "booktabs" t)
          ("margin=1.5cm" "geometry" t)
          ("lambda, advantage, operators, sets, adversary, landau,\
    probability, notions, logic, ff, mm, primitives, events, complexity, oracles,\
    asymptotics, keys" "cryptocode" t)
          )
        )

;;; org-babel
(setopt org-confirm-babel-evaluate nil) ; do not confirm, just run

(org-babel-do-load-languages
 'org-babel-load-languages
 ;; this line activates plantuml
 '((C . t) ; c, c++, and D
   (shell . t)
   (latex . t)
   ))

;; HACK from doom-emacs
(defun +org-fix-newline-and-indent-in-src-blocks-a
    (&optional indent _arg _interactive)
  "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
  (when (and indent
             org-src-tab-acts-natively
             (org-in-src-block-p t))
    (save-window-excursion
      (org-babel-do-in-edit-buffer
       (call-interactively #'indent-for-tab-command)))))

(advice-add #'org-return :after #'+org-fix-newline-and-indent-in-src-blocks-a)

;; HACK Fix #6061. Seems `org-babel-do-in-edit-buffer' has the side effect of
;;   deleting side windows. Should be reported upstream! This advice
;;   suppresses this behavior wherever it is known to be used.
(defun +org-fix-window-excursions-a (fn &rest args)
  "Suppress changes to the window config anywhere
    `org-babel-do-in-edit-buffer' is used."
  (save-window-excursion (apply fn args)))

(dolist (target '(evil-org-open-below evil-org-open-above org-indent-region org-indent-line))
  (advice-add target :around #'+org-fix-window-excursions-a))

;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
;;      underlying faces like the `org-todo' face does, so we define our own
;;      intermediary faces that extend from org-todo.
(with-no-warnings
  (custom-declare-face '+org-todo-active
                       '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face '+org-todo-project
                       '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face '+org-todo-onhold
                       '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face
   '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

;; -----------------------------------------------------------
;; DONE org
;;
;; org-journal
;; org-download
;; ob-http
;; plantuml-mode
;; deft
;; org-roam
;; org-appear
;; xenops
;; engrave-faces
;; consult-notes
;; -----------------------------------------------------------

(use-package org-journal
  :ensure t
  :custom
  (org-journal-dir (concat +emacs/org-root-dir "/journal"))
  (org-journal-find-file-fn 'find-file)
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-file-type 'monthly)
  (org-journal-carryover-items "TODO=\"[ ]\"|TODO=\"[?]\"|TODO=\"[-]\"")
  (org-journal-enable-agenda-integration t)
  :config
  (add-to-list 'org-agenda-files org-journal-dir))

(use-package org-download
  :ensure t
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

(use-package ob-http
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :after org
  :custom
  (plantuml-jar-path
   (concat (file-name-directory user-init-file) "plantuml.jar"))
  (org-plantuml-jar-path plantuml-jar-path)
  :config
  (setq plantuml-default-exec-mode 'executable)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   ;; this line activates plantuml
   '((plantuml . t))))

(use-package deft
  :ensure t
  :after general
  :custom
  (deft-recursive t)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase)))
  (deft-auto-save-interval -1.0)
  (deft-use-filter-string-for-filename nil)
  (deft-use-filename-as-title nil)
  (deft-directory (concat +emacs/org-root-dir "/deft"))
  (deft-ignore-file-regexp "^\\(?:\\.|$\\)")
  :config
  (setq deft-strip-summary-regexp
	    (concat "\\("
		        "[\n\t]" ;; blank
		        "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		        "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		        "\\)"))
  (setq deft-default-extension "org")
  ;; NOTE enable auto refresh
  ;; see: https://github.com/jrblevin/deft/pull/62/files
  (defvar deft-auto-refresh-descriptor nil)
  (defun deft-auto-refresh (event)
    (deft-refresh))
  (when (fboundp 'file-notify-add-watch)
    (setq deft-auto-refresh-descriptor
          (file-notify-add-watch
           deft-directory
           '(change attribute-change)
           'deft-auto-refresh))))

(use-package org-roam
  :ensure t
  :after evil
  :custom
  (org-roam-directory (concat +emacs/org-root-dir "/roam"))
  :config
  ;; If you're using a vertical completion framework, you might want
  ;; a more informative completion interface
  (setq org-roam-node-display-template
        (format "${title:50}%s"
                (propertize "${tags:10}" 'face 'org-tag)))
  (require 'org-roam-db)
  (org-roam-db-autosync-mode +1)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)

  ;; NOTE Make org-roam case insensitve
  ;; from: https://emacs.stackexchange.com/a/77296
  (defun +org-roam/case-insensitive-org-roam-node-read (orig-fn &rest args)
    (let ((completion-ignore-case t))
      (apply orig-fn args)))
  (advice-add 'org-roam-node-read :around #'+org-roam/case-insensitive-org-roam-node-read)
  (advice-add 'org-roam-node-insert :before #'+evil/smart-insert))

(use-package org-appear
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

(use-package xenops
  :ensure t
  :if (and window-system (not (eq system-type 'windows-nt))) ;; do not load xenops on termial emacs or windows nt
  :config
  (setq xenops-font-family "Maple Mono NL NF CN")
  (setq xenops-reveal-on-entry nil)
  (setopt xenops-math-image-scale-factor 1.2)
  (setq xenops-math-latex-process-alist org-preview-latex-process-alist)
  (add-hook 'org-mode-hook #'xenops-mode)
  (setq xenops-math-latex-process 'xdvisvgm) ; HACK or, ximagemagick
  (defun fn/xenops-src-parse-at-point ()
    (-if-let*
        ((element (xenops-parse-element-at-point 'src))
         (org-babel-info
          (xenops-src-do-in-org-mode
           (org-babel-get-src-block-info 'light (org-element-context)))))
        (xenops-util-plist-update
         element
         :type 'src
         :language (nth 0 org-babel-info)
         :org-babel-info org-babel-info)))

  ;; NOTE error from xenops with org>9.7
  ;; https://github.com/syl20bnr/spacemacs/issues/16577
  ;; https://github.com/dandavison/xenops/pull/74/files
  ;; https://github.com/dandavison/xenops/issues/73
  (advice-add 'xenops-src-parse-at-point
              :override 'fn/xenops-src-parse-at-point))

(use-package engrave-faces
  :ensure t)

(use-package org-gtd
  :ensure t
  :after popwin
  :init
  ;; Suppress upgrade warnings (must be set before package loads)
  (setq org-gtd-update-ack "4.0.0")
  :custom
  (org-gtd-save-after-organize t)
  (org-gtd-directory (concat +emacs/org-root-dir "/gtd"))
  ;; Map GTD semantic states to your keywords
  (org-gtd-keyword-mapping '((todo . "TODO")
                             (next . "NEXT")
                             (wait . "WAIT")
                             (done . "DONE")
                             (canceled . "KILL")))
  :config

  ;; Advise =org-gtd-clarify-setup-windows=
  (defun +org-gtd/clarify--use-popwin (orig-fun buffer-or-name)
    "Display BUFFER-OR-NAME using popwin instead of default window setup."
    (let ((buffer (get-buffer buffer-or-name)))
      ;; Show main clarify buffer via popwin
      (popwin:popup-buffer buffer)
      ;; Optionally show horizons if enabled (keep original behavior)
      (when org-gtd-clarify-show-horizons
        (org-gtd-clarify--display-horizons-window))))

  (advice-add 'org-gtd-clarify-setup-windows :around #'+org-gtd/clarify--use-popwin)

  ;; Prevent manual window restoration in org-gtd-clarify-stop
  (defun +org-gtd/clarify--skip-window-restore (orig-fun &rest args)
    "Skip restoring window config (handled by popwin)."
    (let ((org-gtd-clarify--window-config nil)) ; Shadow the var
      (apply orig-fun args)))

  (advice-add 'org-gtd-clarify-stop :around #'+org-gtd/clarify--skip-window-restore)

  ;; REQUIRED: Enable org-edna for project dependencies
  (org-edna-mode 1)
  ;; Add org-gtd files to your agenda (must be in :config so org-gtd-directory is defined)
  (setq org-agenda-files (list org-gtd-directory))

  (general-define-key
   :keymaps 'org-gtd-clarify-mode-map
   "C-c c"   #'org-gtd-organize)

  (general-define-key
   :keymaps 'org-agenda-mode-map
   "C-c ."   #'org-gtd-agenda-transient)
  )

(defun consult-org--get-heading-time-info (marker)
  "Extract time info (SCHEDULED, DEADLINE, or timestamp) from heading at MARKER."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let ((scheduled (org-entry-get (point) "SCHEDULED"))
            (deadline (org-entry-get (point) "DEADLINE"))
            (ts (org-get-scheduled-time (point))))
        (cond
         (scheduled (concat " SCH: " (substring scheduled 1 -1)))
         (deadline (concat " DDL: " (substring deadline 1 -1)))
         (ts (format-time-string " 🗓 %Y-%m-%d" ts))
         (t ""))))))

(defun consult-org-agenda-by-todo-status-with-time (orig-fun &optional match)
  "Around advice for `consult-org-agenda' to group by TODO and show time info,
with aligned time columns by using fixed-width priority placeholder."
  (unless org-agenda-files
    (user-error "No agenda files"))
  (let* ((prefix t)
         (cands (consult--slow-operation "Collecting agenda headings..."
                  (or (consult-org--headings t match 'agenda)
                      (user-error "No agenda headings"))))
         (my-annotate
          (lambda (cand)
            (pcase-let ((`(,_level ,todo ,prio . ,_)
                         (get-text-property 0 'consult-org--heading cand)))
              (let* ((priority-str
                      (if prio
                          (propertize (format " [#%c]" prio) 'face 'org-priority)
                        "     ")) ; 5 spaces = width of " [#A]"
                     (base-annot (concat (or todo "") priority-str))
                     (marker (get-text-property 0 'org-marker cand))
                     (time-info (consult-org--get-heading-time-info marker)))
                (consult--annotate-align
                 cand
                 (concat base-annot
                         (and (not (string-empty-p time-info)) time-info))))))))
    (consult--read
     cands
     :prompt "Go to agenda heading (by TODO): "
     :category 'org-heading
     :sort nil
     :require-match t
     :history '(:input consult-org--history)
     :narrow (consult-org--narrow)
     :state (consult--jump-state)
     :annotate my-annotate
     :group (lambda (cand transform)
              (pcase-let ((`(,_level ,todo ,_prio . ,_)
                           (get-text-property 0 'consult-org--heading cand)))
                (if transform
                    (substring cand (string-match-p " " cand))
                  (or todo "[no TODO]"))))
     :lookup (apply-partially #'consult--lookup-prop 'org-marker))))

(advice-add 'consult-org-agenda :around #'consult-org-agenda-by-todo-status-with-time)

(provide 'init-org)
