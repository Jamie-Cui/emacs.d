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
   ;; roam
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
   ;; preview org math
   xenops
   ))

;; DEPRECATED I'm considering drop this package
(use-package org-imgtog
  :load-path (lambda () (concat jc-emacs-directory "/site-lisp"))
  :config
  (defun +org-imgtog/toggle ()
    "Toggle global-treesit-auto-mode."
    (interactive)
    (if org-imgtog-mode
        (progn
          (org-imgtog-mode -1)
          (message "org-imgtog-mode disabled"))
      (org-imgtog-mode 1)
      (message "org-imgtog-mode enabled"))))

(use-package org
  :custom
  ;; general
  (org-link-elisp-confirm-function nil)
  (org-image-actual-width nil)
  (org-cycle-hide-drawer-startup t)
  (org-cycle-hide-block-startup t)
  (org-cycle-open-archived-trees nil)
  (org-log-done t)
  (org-preview-latex-default-process 'dvisvgm)
  (org-use-fast-todo-selection t)
  (org-todo-keyword-faces
   '(("[-]" . +org-todo-active)
     ("[?]" . +org-todo-onhold))
   )
  (org-todo-keywords
   '((sequence
      "[ ](t)"   ; A task that needs doing
      "[-](s)"   ; Task is in progress
      "[?](w)"   ; Task is being held up or paused
      "|"
      "[X](d)")  ; Task was completed
     )
   )
  ;; org export
  (org-export-dispatch-use-expert-ui t)
  (org-export-with-toc nil)
  (org-latex-compiler "xelatex")
  (org-latex t)
  (org-latex-src-block-backend 'engraved)
  (org-latex-engraved-theme 't)
  (org-latex-packages-alist 
   '(
     ("" "xeCJK" t)
     ("" "booktabs" t)
	 ("margin=1.5cm" "geometry" t)
     ("lambda, advantage, operators, sets, adversary, landau,\
    probability, notions, logic, ff, mm, primitives, events, complexity, oracles,\
    asymptotics, keys" "cryptocode" t)
     )
   )
  ;; org src, org babel
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'other-window)
  (org-confirm-babel-evaluate nil) ; don't ask, just do it
  ;; org startup
  (org-startup-indented t)
  (org-startup-folded 'nofold)
  (org-startup-with-inline-images nil)
  (org-startup-with-latex-preview nil)
  :config
  (add-to-list 'org-export-backends 'beamer)
  (setq org-preview-latex-process-alist
        '((dvisvgm
		   :programs ("xelatex" "dvisvgm")
		   :description "xdv > svg"
		   :image-input-type "xdv"
		   :image-output-type "svg"
		   :image-size-adjust (1.7 . 1.5)
		   :latex-compiler ;; Default `xelatex' as the process previewing LaTeX fragments
		   ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
		   :image-converter ;; Set `dvisvgm' with --exact option
		   ("dvisvgm %f -e -n -b min -c %S -o %O"))))

  ;; HACK from doom emacs
  (defmacro defadvice! (symbol arglist &optional docstring &rest body)
    "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
    (declare (doc-string 3) (indent defun))
    (unless (stringp docstring) (push docstring body) (setq docstring nil))
    (let (where-alist)
      (while (keywordp (car body))
        (push `(cons ,(pop body) (ensure-list ,(pop body)))
              where-alist))
      `(progn
         (defun ,symbol ,arglist ,docstring ,@body)
         (dolist (targets (list ,@(nreverse where-alist)))
           (dolist (target (cdr targets))
             (advice-add target (car targets) #',symbol))))))

  (defadvice! +org-fix-newline-and-indent-in-src-blocks-a 
    (&optional indent _arg _interactive)
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
    (custom-declare-face '+org-todo-active  
                         '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project 
                         '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  
                         '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face 
     '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) "")))

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
           'deft-auto-refresh))))

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

(use-package xenops
  :ensure t
  :if window-system ;; do not load xenops on termial emacs
  :config
  (setopt xenops-math-image-scale-factor 0.4)
  (add-hook 'org-mode-hook #'xenops-mode)
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

  ;; HACK error from xenops with org>9.7
  ;; https://github.com/syl20bnr/spacemacs/issues/16577
  ;; https://github.com/dandavison/xenops/pull/74/files
  ;; https://github.com/dandavison/xenops/issues/73
  (advice-add 'xenops-src-parse-at-point
              :override 'fn/xenops-src-parse-at-point))

(provide 'init-org)
