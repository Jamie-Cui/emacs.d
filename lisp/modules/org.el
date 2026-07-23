;;; org.el --- Org mode core -*- lexical-binding: t -*-
;;; Commentary:
;; Org mode core: base settings, babel, export, plantuml, org-download and
;; org-appear.
;;; Code:


(require 'org)
(require 'org-agenda)
(require 'org-element)
(require 'ob-plantuml)

;; archive to same file
(setopt org-archive-location "::* Archive")
(define-key org-mode-map (kbd "C-c C-a") #'org-archive-subtree)

;; Emacs 30.2 can native-compile this helper incorrectly and then call
;; `org-element-with-disabled-cache' like a function while dashboard renders
;; agenda items.
(defun +org--org-element-get-category-interpreted ()
  "Interpreted replacement for `org-element--get-category'."
  (let ((default-category
         (cond ((null org-category)
                (when (org-with-base-buffer nil
                        buffer-file-name)
                  (file-name-sans-extension
                   (file-name-nondirectory
                    (org-with-base-buffer nil
                      buffer-file-name)))))
               ((symbolp org-category) (symbol-name org-category))
               (t org-category)))
        category)
    (org-with-point-at (point-max)
      (while (and (not category)
                  (re-search-backward "^[ \t]*#\\+CATEGORY:" (point-min) t))
        (let ((element (org-element-at-point-no-context)))
          (when (org-element-type-p element 'keyword)
            (setq category (org-element-property :value element))))))
    (or category default-category)))

(when (and (string-prefix-p "30.2" emacs-version)
           (fboundp 'org-element--get-category)
           (subrp (symbol-function 'org-element--get-category)))
  (defalias 'org-element--get-category
    #'+org--org-element-get-category-interpreted))


;; -----------------------------------------------------------
;; DONE org: the built-in package
;; -----------------------------------------------------------

(use-package toc-org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

;;; list
(setopt org-list-allow-alphabetical t)

;;; editing
(setopt org-return-follows-link nil)
(setopt org-link-elisp-confirm-function nil)

;;; agenda
(setopt org-agenda-window-setup 'current-window)

(general-define-key
 :keymaps 'org-agenda-mode-map
 :states 'normal
 "RET"   #'org-agenda-switch-to)

;; make org file find org-journal files (maybe not needed?)
;; (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")

;; HACK markup 记号前后允许中文
(setopt org-emphasis-regexp-components
        (list (concat " \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
              " \t\r\n,\"'"
              "."
              1))
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;;; timer
(setopt org-timer-default-timer "20") ; 20 mins
(setopt org-clock-sound t) ; use default sound

;;; ui
(setopt org-log-done 'time) ; when DONE, log the time
(setopt org-image-actual-width nil)
(setopt org-cycle-hide-drawer-startup t)
(setopt org-cycle-hide-block-startup t)
(setopt org-cycle-open-archived-trees nil)
(setopt org-indirect-buffer-display 'current-window)

(setopt org-startup-indented t)
(setopt org-startup-folded 'nofold)
(setopt org-startup-with-inline-images t)
(setopt org-startup-with-latex-preview nil)

(setopt org-enforce-todo-dependencies t)

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
                             "WAIT(w)"
                             "PROJ(p)"
                             "|"
                             "DONE(d@)" ;; done
                             "KILL(k@)" ;; killed
                             "CAPTURED(c@)" ;; captured elsewhere
                             "MOVED(m@)" ;; moved elsewhere
                             )))

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
;; (add-to-list 'org-export-backends 'markdown)

;;; org-latex
;; NOTE default latex:
;; - font: Computer Modern Unicode (CMU)
;; - font size: 10
(setopt org-latex-compiler "xelatex")
(setopt org-latex-src-block-backend 'engraved)
(setopt org-latex-engraved-theme 't)
(setopt org-latex-packages-alist
        '(
          ("" "parskip" t)
          ("" "xeCJK" t)
          ("" "booktabs" t)
          ("" "amssymb" t)
          ("margin=1.5cm" "geometry" t)
          ("lambda, advantage, operators, sets, adversary, landau,\
    probability, notions, logic, ff, mm, primitives, events, complexity, oracles,\
    asymptotics, keys" "cryptocode" t)
          )
        )

;;; org-babel
(setopt org-confirm-babel-evaluate nil) ; do not confirm, just run

(defun +org/plantuml-jar-path ()
  "Return the machine-local PlantUML jar path."
  (expand-file-name
   "plantuml.jar"
   (file-name-directory
    (or user-init-file
        (expand-file-name "init.el" user-emacs-directory)))))

(setopt org-plantuml-jar-path (+org/plantuml-jar-path)
        org-plantuml-exec-mode 'jar)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t) ; c, c++, and D
   (shell . t)
   (latex . t)
   (plantuml . t)
   ))

(defun +org/plantuml-src-block-bounds-at-point ()
  "Return textual bounds of the PlantUML source block at point."
  (save-excursion
    (let ((case-fold-search t)
          (pos (point))
          beg end)
      (when (re-search-backward
             "^[ \t]*#\\+begin_src[ \t]+plantuml\\(?:[ \t].*\\)?$"
             nil t)
        (setq beg (match-beginning 0)
              end (save-excursion
                    (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
                      (line-end-position))))
        (when (and end (<= beg pos) (<= pos end))
          (cons beg end))))))

(defun +org/plantuml-clear-src-syntax-table-properties (beg end)
  "Remove syntax-table properties from PlantUML source block text."
  (with-silent-modifications
    (remove-text-properties beg end '(syntax-table nil))))

(defun +org/plantuml-clear-src-syntax-table-at-point ()
  "Remove syntax-table properties from the PlantUML source block at point."
  (when (derived-mode-p 'org-mode)
    (let ((bounds (+org/plantuml-src-block-bounds-at-point)))
      (when bounds
        (+org/plantuml-clear-src-syntax-table-properties
         (car bounds) (cdr bounds))))))

(defun +org/plantuml-clear-src-syntax-table-after-fontify-a (lang start end)
  "Keep PlantUML native fontification from confusing Babel parsing."
  (when (string-equal lang "plantuml")
    (+org/plantuml-clear-src-syntax-table-properties start end)))

(unless (advice-member-p #'+org/plantuml-clear-src-syntax-table-after-fontify-a
                         'org-src-font-lock-fontify-block)
  (advice-add 'org-src-font-lock-fontify-block
              :after #'+org/plantuml-clear-src-syntax-table-after-fontify-a))

(defun +org/plantuml-clear-src-syntax-table-before-babel-a (fn &rest args)
  "Clean PlantUML syntax-table properties before Babel reads block info."
  (+org/plantuml-clear-src-syntax-table-at-point)
  (apply fn args))

(unless (advice-member-p #'+org/plantuml-clear-src-syntax-table-before-babel-a
                         'org-babel-get-src-block-info)
  (advice-add 'org-babel-get-src-block-info
              :around #'+org/plantuml-clear-src-syntax-table-before-babel-a))

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
  (plantuml-jar-path (+org/plantuml-jar-path))
  (org-plantuml-jar-path (+org/plantuml-jar-path))
  (plantuml-svg-background "white")
  :config
  (setq plantuml-default-exec-mode 'jar)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package org-appear
  :ensure t
  :custom
  (org-hide-emphasis-markers t)
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))



;; ---------------------------------------------------------------------------
;; Reference: commented-out org drawing package (kept for future use).
;; ---------------------------------------------------------------------------

;; (use-package edraw
;;   :vc (:url "https://github.com/misohena/el-easydraw.git")
;;   :after org
;;   :demand t
;;   :config
;;   (require 'edraw-org)
;;   (edraw-org-setup-default)
;;   (edraw-org-setup-exporter)

;;   ;; keybindings that should not be overriden
;;   (general-define-key
;;    :keymaps 'edraw-editor-map
;;    "<backspace>"   #'edraw-editor-delete-selected
;;    )
;;   )

(provide 'init-org)
;;; org.el ends here
