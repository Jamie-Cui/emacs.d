;;; init-org.el --- org support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; defining your own
;; macos: ~/Library/Mobile Documents/com~apple~CloudDocs/org-root
;; linux: ~/org-root
;; it's recommended to symlink your remote file here
;; ln -s ~/Library/Mobile\ Documents/com\~apple\~CloudDocs/org-root .

(make-directory (concat jc-org-root-dir "/roam") t)
(make-directory (concat jc-org-root-dir "/journal") t)
(make-directory (concat jc-org-root-dir "/deft") t)
(make-directory (concat (file-name-directory user-init-file) "/bin") t)

(+package/ensure-install
 '(
   org-download
   org-roam
   ;; make org prettier
   org-appear
   ;; better place to write diaries
   org-journal
   ;; deft for note taking
   deft
   ;; allow drawing plantuml
   plantuml-mode
   ;; export org code in colors
   engrave-faces
   ))

;; site-lisp
(use-package org-imgtog
  :load-path (lambda () (concat jc-emacs-directory "/site-lisp"))
  :hook org-mode)

(use-package org
  :custom
  (org-image-actual-width nil)
  (org-export-dispatch-use-expert-ui t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil) ; don't ask, just do it
  (org-link-elisp-confirm-function nil)
  (org-startup-indented t)
  (org-startup-folded 'nofold)
  (org-cycle-hide-drawer-startup t)
  (org-cycle-hide-block-startup t)
  (org-cycle-open-archived-trees nil)
  (org-log-done t)
  (org-src-window-setup 'other-window)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t) 
  (org-preview-latex-default-process 'dvisvgm)
  :config
  (add-to-list 'org-export-backends 'beamer)

  ;; HACK from doom emacs
  (defmacro defadvice! (symbol arglist &optional docstring &rest body)
    (declare (doc-string 3) (indent defun))
    (unless (stringp docstring)
      (push docstring body)
      (setq docstring nil))
    (let (where-alist)
      (while (keywordp (car body))
        (push `(cons ,(pop body) (ensure-list ,(pop body)))
              where-alist))
      `(progn
         (defun ,symbol ,arglist ,docstring ,@body)
         (dolist (targets (list ,@(nreverse where-alist)))
           (dolist (target (cdr targets))
             (advice-add target (car targets) #',symbol))))))

  (defadvice! +org-fix-newline-and-indent-in-src-blocks-a (&optional indent _arg _interactive)
    "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
    :after #'org-return
    (when (and indent
               org-src-tab-acts-natively
               (org-in-src-block-p t))
      (save-window-excursion
        (org-babel-do-in-edit-buffer
         (call-interactively #'indent-for-tab-command)))))
  ;; HACK Fix #6061. Seems `org-babel-do-in-edit-buffer' has the side effect of
  ;;   deleting side windows. Should be reported upstream! This advice
  ;;   suppresses this behavior wherever it is known to be used.
  (defadvice! +org-fix-window-excursions-a (fn &rest args)
    "Suppress changes to the window config anywhere
`org-babel-do-in-edit-buffer' is used."
    :around #'evil-org-open-below
    :around #'evil-org-open-above
    :around #'org-indent-region
    :around #'org-indent-line
    (save-window-excursion (apply fn args)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   ;; this line activates plantuml
   '((C . t) ; c, c++, and D
     (shell . t)
     (latex . t)
     ))

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

  (setopt org-todo-keywords
          '((sequence
             "[ ](t)"   ; A task that needs doing
             "[-](s)"   ; Task is in progress
             "[?](w)"   ; Task is being held up or paused
             "|"
             "[X](d)")  ; Task was completed
            )
          )

  (setopt org-todo-keyword-faces
          '(("[-]" . +org-todo-active)
            ("[?]" . +org-todo-onhold))
          )

  (setopt org-use-fast-todo-selectiona t)

  (add-to-list 'org-latex-packages-alist
               '("lambda, advantage, operators, sets, adversary, landau,\
 probability, notions, logic, ff, mm, primitives, events, complexity, oracles,\
 asymptotics, keys" "cryptocode" t))
  (add-to-list 'org-latex-packages-alist
               '("" "booktabs" t))
  )

(use-package org-journal
  :ensure t
  :custom
  (org-journal-dir (concat jc-org-root-dir "/journal"))
  (org-journal-find-file-fn 'find-file)
  (org-journal-file-type 'monthly)
  (org-journal-carryover-items "TODO=\"[ ]\"|TODO=\"[?]\"|TODO=\"[-]\"")
  )

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

;; https://github.com/plantuml/plantuml/releases/download/v1.2024.7/plantuml-1.2024.7.jar
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
  (deft-directory (concat jc-org-root-dir "/deft"))
  (deft-ignore-file-regexp "^\\(?:\\.|$\\)")
  :config
  (setq deft-strip-summary-regexp
	    (concat "\\("
		        "[\n\t]" ;; blank
		        "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		        "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		        "\\)"))
  (setq deft-default-extension "org")
  ;; HACK enable auto refresh
  ;; see: https://github.com/jrblevin/deft/pull/62/files
  (defvar deft-auto-refresh-descriptor nil)
  (defun deft-auto-refresh (event)
    (deft-refresh))
  (when (fboundp 'file-notify-add-watch)
    (setq deft-auto-refresh-descriptor
          (file-notify-add-watch
           deft-directory
           '(change attribute-change)
           'deft-auto-refresh
           )
          )
    )
  )

(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory (concat jc-org-root-dir "/roam"))

  ;; If you're using a vertical completion framework, you might want
  ;; a more informative completion interface
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-appear
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

(use-package ox-latex
  :after (engrave-faces citar)
  :custom
  (org-export-with-toc nil)
  :config
  (setq org-latex-src-block-backend 'engraved)
  (setq org-latex-engraved-theme 't)
  )

(provide 'init-org)
