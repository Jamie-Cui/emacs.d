;;; init-org.el --- org support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; defining your own
;; macos: ~/Library/Mobile Documents/com~apple~CloudDocs/org-root
;; linux: ~/org-root
;; it's recommended to symlink your remote file here
;; ln -s ~/Library/Mobile\ Documents/com\~apple\~CloudDocs/org-root .

(require 'org)

;; -----------------------------------------------------------
;; DONE org: the built-in package
;; -----------------------------------------------------------

;;; editing
(setopt org-return-follows-link nil)
(setopt org-link-elisp-confirm-function nil)
(setopt org-use-fast-todo-selection 'auto)

;;; ui
(setopt org-log-done 'time) ; when DONE, log the time
(setopt org-image-actual-width nil)
(setopt org-cycle-hide-drawer-startup t)
(setopt org-cycle-hide-block-startup t)
(setopt org-cycle-open-archived-trees nil)
(setopt org-preview-latex-default-process 'dvisvgm)

(setopt org-startup-indented t)
(setopt org-startup-folded 'nofold)
(setopt org-startup-with-inline-images t)
(setopt org-startup-with-latex-preview nil)

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

;; todo keywords
(setopt org-todo-keyword-faces '(
                                 ("[-]" . +org-todo-active)
                                 ("[?]" . +org-todo-onhold))
        org-todo-keywords '((sequence
                             "[ ](t)"
                             "[-](s)" 
                             "[?](w)"  
                             "|"
                             "[X](d)"))
        )

;;; org-src
(setopt org-src-fontify-natively t)
(setopt org-src-strip t)
(setopt org-src-preserve-indentation nil) 

;;; org-export
(setopt org-export-dispatch-use-expert-ui t)
(setopt org-export-with-toc nil)
(setopt org-cite-export-processors '((latex biblatex) (beamer natbib) (t plain)))
(add-to-list 'org-export-backends 'beamer) ;; support beamer

;;; org-latex
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
(org-babel-do-load-languages
 'org-babel-load-languages
 ;; this line activates plantuml
 '((C . t) ; c, c++, and D
   (shell . t)
   (latex . t)
   ))

;; HACK from doom-emacs
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
  :custom
  (org-roam-directory (concat +emacs/org-root-dir "/roam"))
  :config
  (org-roam-db-autosync-mode +1)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)

  ;; NOTE Make org-roam case insensitve
  ;; from: https://emacs.stackexchange.com/a/77296
  (defun case-insensitive-org-roam-node-read (orig-fn &rest args)
    (let ((completion-ignore-case t))
      (apply orig-fn args)))
  (advice-add 'org-roam-node-read :around #'case-insensitive-org-roam-node-read))

(use-package org-appear
  :ensure t
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

(use-package engrave-faces
  :ensure t)

(provide 'init-org)
