;;; init-org.el --- org support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-element)

(add-to-list 'load-path (expand-file-name "site-lisp" +emacs/repo-directory))
(setq +org-project-root-dir +emacs/org-root-dir)
(require 'org-project)
(require 'dashboard-org-project)
(dashboard-org-project-setup)

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
;; DONE hack: filter out sync-conflict and *-beorg
;; -----------------------------------------------------------

(defgroup +org-agenda nil
  "Local agenda hygiene helpers."
  :group 'org)

(defcustom +org-agenda-ignored-file-regexps
  '("\\.sync-conflict-[^/]*\\.org\\'"
    "/[^/]+-beorg\\.org\\'")
  "Regexps for agenda files that should be ignored locally."
  :type '(repeat string)
  :group '+org-agenda)

(defun +org-agenda-ignored-file-p (file)
  "Return non-nil when FILE should be excluded from `org-agenda-files'."
  (when (stringp file)
    (let ((path (expand-file-name file))
          ignored)
      (dolist (regexp +org-agenda-ignored-file-regexps ignored)
        (when (string-match-p regexp path)
          (setq ignored t))))))

(defun +org-agenda-prune-files (&optional files)
  "Remove ignored entries from FILES or `org-agenda-files'."
  (interactive)
  (let* ((targets (or files org-agenda-files))
         (normalized (delete-dups
                      (delq nil
                            (mapcar (lambda (file)
                                      (when (stringp file)
                                        (expand-file-name file)))
                                    (copy-sequence targets)))))
         (filtered (delq nil
                         (mapcar (lambda (file)
                                   (unless (+org-agenda-ignored-file-p file)
                                     file))
                                 normalized))))
    (when (or (null files)
              (called-interactively-p 'interactive))
      (setq org-agenda-files filtered))
    filtered))

(defun +org-agenda--prune-after-journal-update (&rest _)
  "Keep generated journal side files out of `org-agenda-files'."
  (+org-agenda-prune-files))

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
(setopt org-startup-folded nil)
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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t) ; c, c++, and D
   (shell . t)
   (latex . t)
   (plantuml . t)
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
;; -----------------------------------------------------------

(use-package org-journal
  :ensure t
  :custom
  (org-journal-dir (concat +emacs/org-root-dir "/journal"))
  (org-journal-find-file-fn 'find-file)
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-file-type 'monthly)
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"WAIT\"|TODO=\"PROJ\"")
  (org-journal-enable-agenda-integration t)
  :config
  (setq org-element-use-cache nil)
  (add-to-list 'org-agenda-files org-journal-dir)
  (+org-project-sync-agenda-files)
  (+org-agenda-prune-files)
  (advice-add 'org-journal--update-org-agenda-files
              :after
              #'+org-agenda--prune-after-journal-update))

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
  (org-plantuml-jar-path
   (concat (file-name-directory user-init-file) "plantuml.jar"))
  :config
  (setq plantuml-default-exec-mode 'executable)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(setq +deft/org-root-dir +emacs/org-root-dir)
(require 'init-config-deft)

(use-package org-roam
  :ensure t
  :after evil
  :custom
  (org-roam-directory (+emacs/org-subdir "roam"))
  :config
  ;; 例如改成只用时间戳：
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y-%m-%dt%H%M>.org" "#+title: ${title}\n")
           :unnarrowed t)))

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
  :custom
  (org-hide-emphasis-markers t)
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

(require 'init-config-xenops)

(use-package engrave-faces
  :ensure t)

(require 'init-config-consult)

(provide 'init-org)
