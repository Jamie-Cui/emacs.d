;;; init-org.el --- org support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-element)
(require 'init-utils)
(require 'init-org-project)

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
  (org-plantuml-jar-path plantuml-jar-path)
  :config
  (setq plantuml-default-exec-mode 'executable)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

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
  (deft-directory (+emacs/ensure-directory +emacs/org-root-dir))
  (deft-ignore-file-regexp "^$")
  (deft-extensions '("org"))
  :config
  (setq deft-strip-summary-regexp
	    (concat "\\("
		        "[\n\t]" ;; blank
		        "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		        "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		        "\\)"))
  (setq deft-default-extension "org")

  (add-hook 'deft-mode-hook
            (lambda ()
              (setq-local revert-buffer-function
                          (lambda (&rest _) (deft-refresh)))))

  (defcustom +deft/group-by-top-level-directory t
    "When non-nil, group Deft entries by their top-level org-root directory."
    :type 'boolean
    :group 'deft)

  (defcustom +deft/group-order
    '("journal" "roam" "deft" "projects" "external" "root")
    "Preferred top-level directory order for Deft group headers."
    :type '(repeat string)
    :group 'deft)

  (defface +deft/group-header-face
    '((t (:inherit (font-lock-keyword-face bold))))
    "Face used for group headings in the Deft browser."
    :group 'deft)

  (defvar-local +deft/current-group nil
    "Current group section while rendering the Deft browser.")

  (defvar +deft/current-profile 'org
    "Current Deft profile.")

  (defcustom +deft/new-file-location "deft"
    "Fallback directory used for newly created Deft files.
When point is inside a Deft group, new files are created under that group's
top-level directory instead. When relative, interpret this fallback directory
under `+emacs/org-root-dir'."
    :type 'string
    :group 'deft)

  (defvar +deft/auto-refresh-watched-directory nil
    "Directory currently watched by Deft auto refresh.")

  (defconst +deft/profile-extensions
    '((org . ("org"))
      (tex . ("tex")))
    "Mapping from Deft profile to file extensions.")

  (defconst +deft/ignored-directory-names
    '("img" "ltximg" "pdf" "scripts" "roam")
    "Directory names excluded from unified Deft views.")

  (defconst +deft/ignored-file-regexps
    '("\\.sync-conflict-[^/]*\\.org\\'"
      "/[^/]+-beorg\\.org\\'"
      "/deft/archive/")
    "Additional file path regexps excluded from unified Deft views.")

  (defun +deft/org-root-directory ()
    "Return the normalized org root directory for Deft."
    (file-name-as-directory
     (file-truename (+emacs/ensure-directory +emacs/org-root-dir))))

  (defun +deft/new-file-directory ()
    "Return the normalized fallback directory for newly created Deft files."
    (let ((directory (if (file-name-absolute-p +deft/new-file-location)
                         +deft/new-file-location
                       (expand-file-name +deft/new-file-location
                                         (+deft/org-root-directory)))))
      (file-name-as-directory
       (file-truename (+emacs/ensure-directory directory)))))

  (defun +deft/group-directory (group)
    "Return the normalized directory backing GROUP."
    (if (or (null group)
            (string-empty-p group)
            (string= group "root"))
        (+deft/org-root-directory)
      (file-name-as-directory
       (file-truename
        (+emacs/ensure-directory
         (expand-file-name group (+deft/org-root-directory)))))))

  (defun +deft/group-at-point ()
    "Return the Deft group key at point, or nil when unavailable."
    (when (derived-mode-p 'deft-mode)
      (or (when-let ((file (deft-filename-at-point)))
            (+deft/group-key file))
          (let ((group (or (get-text-property (point) '+deft/group)
                           (and (> (point) (point-min))
                                (get-text-property (1- (point)) '+deft/group)))))
            (or group
                (save-excursion
                  (catch 'found
                    (while (> (point) (point-min))
                      (forward-line -1)
                      (setq group (or (get-text-property (point) '+deft/group)
                                      (and (> (point) (point-min))
                                           (get-text-property (1- (point))
                                                              '+deft/group))))
                      (when group
                        (throw 'found group))))))))))

  (defun +deft/current-new-file-directory ()
    "Return the directory where a new Deft file should be created."
    (if-let ((group (+deft/group-at-point)))
        (+deft/group-directory group)
      (+deft/new-file-directory)))

  (defun +deft/hidden-path-regexp (&optional trailing)
    "Return a regexp matching hidden paths.
When TRAILING is non-nil, also require a trailing slash."
    (concat "/\\.[^/]*"
            (if trailing
                "/"
              "\\(?:/\\|\\'\\)")))

  (defun +deft/ignored-directory-regexp ()
    "Return a regexp matching directories excluded from Deft recursion."
    (let ((named (regexp-opt +deft/ignored-directory-names)))
      (concat "\\(?:"
              (+deft/hidden-path-regexp nil)
              "\\|/\\(?:" named "\\)\\'"
              "\\|/deft/archive\\'\\)")))

  (defun +deft/ignored-file-regexp ()
    "Return a regexp matching files excluded from Deft views."
    (let ((named (regexp-opt +deft/ignored-directory-names))
          (root-readme
           (concat "\\`"
                   (regexp-quote
                    (expand-file-name "README.org" (+deft/org-root-directory)))
                   "\\'")))
      (concat "\\(?:"
              (+deft/hidden-path-regexp nil)
              "\\|/\\(?:" named "\\)/"
              "\\|" root-readme
              "\\|"
              (mapconcat #'identity +deft/ignored-file-regexps "\\|")
              "\\)")))

  (defun +deft/relative-file (file)
    "Return FILE relative to `+deft/org-root-directory'."
    (string-remove-prefix (+deft/org-root-directory)
                          (file-truename file)))

  (defun +deft/normalize-org-heading (heading)
    "Normalize org HEADING text for Deft titles."
    (let ((text (string-trim heading)))
      (setq text
            (replace-regexp-in-string
             "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" "" text))
      (string-trim (deft-strip-title text))))

  (defun +deft/parse-org-title (file contents)
    "Parse a useful Deft title for org FILE from CONTENTS."
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (let ((case-fold-search t)
            title)
        (when (re-search-forward "^#\\+title:[ \t]*\\(.+\\)$" nil t)
          (setq title (string-trim (match-string-no-properties 1))))
        (unless (and title (not (string-empty-p title)))
          (goto-char (point-min))
          (catch 'found
            (while (not (eobp))
              (cond
               ((looking-at-p "^[ \t]*$")
                (forward-line 1))
               ((looking-at-p "^:PROPERTIES:[ \t]*$")
                (if (re-search-forward "^:END:[ \t]*$" nil t)
                    (forward-line 1)
                  (goto-char (point-max))))
               ((looking-at-p "^#\\+[[:alpha:]_]+:.*$")
                (forward-line 1))
               ((looking-at "^\\*+[ \t]+\\(.*\\)$")
                (let ((heading (+deft/normalize-org-heading
                                (match-string-no-properties 1))))
                  (when (not (string-empty-p heading))
                    (setq title heading)
                    (throw 'found title)))
                (forward-line 1))
               (t
                (let ((line (string-trim
                             (deft-strip-title
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))))
                  (if (string-empty-p line)
                      (forward-line 1)
                    (setq title line)
                    (throw 'found title))))))))
        (or title
            (deft-base-filename file)))))

  (defun +deft/parse-title-a (orig-fn file contents)
    "Parse better titles for org FILE using CONTENTS, else delegate to ORIG-FN."
    (if (string= (downcase (or (file-name-extension file) "")) "org")
        (+deft/parse-org-title file contents)
      (funcall orig-fn file contents)))
  (advice-add #'deft-parse-title :around #'+deft/parse-title-a)

  (defun +deft/absolute-filename-a (orig-fn slug &optional extension)
    "Create new Deft files in `+deft/new-file-directory'."
    (let ((deft-directory (+deft/current-new-file-directory)))
      (funcall orig-fn slug extension)))
  (advice-add #'deft-absolute-filename :around #'+deft/absolute-filename-a)

  (defun +deft/group-key (file)
    "Return FILE's top-level org-root directory key."
    (let* ((relative (+deft/relative-file file))
           (segments (split-string relative "/" t)))
      (downcase
       (or (car segments)
           "root"))))

  (defun +deft/group-label (group)
    "Return display label for GROUP."
    (upcase group))

  (defun +deft/group-rank (group)
    "Return sort rank for GROUP."
    (or (cl-position group +deft/group-order :test #'string=)
        (+ (length +deft/group-order) 100)))

  (defun +deft/group-lessp (left right)
    "Return non-nil when LEFT should sort before RIGHT."
    (let ((left-rank (+deft/group-rank left))
          (right-rank (+deft/group-rank right)))
      (if (/= left-rank right-rank)
          (< left-rank right-rank)
        (string-lessp left right))))

  (defun +deft/pin-files-first (files)
    "Return FILES with pinned files moved to the front."
    (let (pinned rest)
      (dolist (file files)
        (if (member file +deft/pinned-files)
            (push file pinned)
          (push file rest)))
      (append (nreverse pinned) (nreverse rest))))

  (defun +deft/preferred-file-path (left right)
    "Return the preferred Deft path between LEFT and RIGHT."
    (let ((left-rel (+deft/relative-file left))
          (right-rel (+deft/relative-file right)))
      (cond
       ((< (length left-rel) (length right-rel)) left)
       ((> (length left-rel) (length right-rel)) right)
       ((string-lessp left-rel right-rel) left)
       (t right))))

  (defun +deft/dedupe-files (files)
    "Remove duplicate FILES that resolve to the same truename."
    (let ((choices (make-hash-table :test #'equal))
          ordered)
      (dolist (file files)
        (let* ((truth (file-truename file))
               (existing (gethash truth choices)))
          (unless existing
            (push truth ordered))
          (puthash truth
                   (if existing
                       (+deft/preferred-file-path existing file)
                     file)
                   choices)))
      (mapcar (lambda (truth)
                (gethash truth choices))
              (nreverse ordered))))

  ;; NOTE enable auto refresh
  ;; see: https://github.com/jrblevin/deft/pull/62/files
  (defvar deft-auto-refresh-descriptor nil)
  (defun deft-auto-refresh (event)
    (deft-refresh))

  (defun +deft/update-auto-refresh-watch ()
    "Update the file notification watch for the current `deft-directory'."
    (when (fboundp 'file-notify-add-watch)
      (let ((directory (file-name-as-directory
                        (expand-file-name deft-directory))))
        (unless (equal directory +deft/auto-refresh-watched-directory)
          (when deft-auto-refresh-descriptor
            (ignore-errors
              (file-notify-rm-watch deft-auto-refresh-descriptor)))
          (setq deft-auto-refresh-descriptor
                (file-notify-add-watch
                 directory
                 '(change attribute-change)
                 'deft-auto-refresh))
          (setq +deft/auto-refresh-watched-directory directory)))))

  (+deft/update-auto-refresh-watch)

  ;; NOTE pin files and optionally group them by source directory
  (defcustom +deft/pinned-files nil
    "List of pinned file paths shown before unpinned files in Deft."
    :type '(repeat string)
    :group 'deft)

  (defconst +deft/pin-prefix "* "
    "Prefix string shown before pinned file titles.")

  (defun +deft/sort-files-a (files)
    "Pin FILES first and optionally group them by top-level directory."
    (setq files (+deft/dedupe-files files))
    (if (not +deft/group-by-top-level-directory)
        (+deft/pin-files-first files)
      (let ((buckets (make-hash-table :test #'equal))
            seen-groups
            result)
        (dolist (file files)
          (let* ((group (+deft/group-key file))
                 (bucket (or (gethash group buckets)
                             (cons nil nil))))
            (unless (member group seen-groups)
              (push group seen-groups))
            (if (member file +deft/pinned-files)
                (setcar bucket (cons file (car bucket)))
              (setcdr bucket (cons file (cdr bucket))))
            (puthash group bucket buckets)))
        (dolist (group (sort (nreverse seen-groups) #'+deft/group-lessp))
          (let ((bucket (gethash group buckets)))
            (setq result
                  (nconc result
                         (nreverse (car bucket))
                         (nreverse (cdr bucket))))))
        result)))
  (advice-add #'deft-sort-files :filter-return #'+deft/sort-files-a)

  (defun +deft/reset-group-a (orig-fn &optional refresh)
    "Reset group state before calling ORIG-FN with REFRESH."
    (let ((+deft/current-group nil))
      (funcall orig-fn refresh)))
  (advice-add #'deft-buffer-setup :around #'+deft/reset-group-a)

  (defun +deft/file-button-a (orig-fn file)
    "Insert group headers and pin prefixes before calling ORIG-FN on FILE."
    (when (and file +deft/group-by-top-level-directory)
      (let ((group (+deft/group-key file)))
        (unless (equal group +deft/current-group)
          (when +deft/current-group
            (insert "\n"))
          (setq +deft/current-group group)
          (insert (propertize (concat (+deft/group-label group) "\n")
                              'face '+deft/group-header-face
                              '+deft/group group)))))
    (if (member file +deft/pinned-files)
        (let ((deft-window-width (- deft-window-width
                                    (string-width +deft/pin-prefix)))
              (start (point)))
          (funcall orig-fn file)
          (save-excursion
            (goto-char start)
            (insert +deft/pin-prefix)))
      (funcall orig-fn file)))
  (advice-add #'deft-file-button :around #'+deft/file-button-a)

  (defun +deft/toggle-pin ()
    "Toggle pinning of the file at point in the Deft browser."
    (interactive)
    (let ((file (deft-filename-at-point)))
      (if (not file)
          (message "No file at point")
        (if (member file +deft/pinned-files)
            (progn
              (setq +deft/pinned-files (delete file +deft/pinned-files))
              (message "Unpinned: %s" (file-name-nondirectory file)))
          (push file +deft/pinned-files)
          (message "Pinned: %s" (file-name-nondirectory file)))
        (customize-save-variable '+deft/pinned-files +deft/pinned-files)
        (deft-refresh))))
  (define-key deft-mode-map (kbd "C-c C-p") #'+deft/toggle-pin)

  (defun +deft/apply-profile (profile)
    "Apply Deft PROFILE settings."
    (setq +deft/current-profile profile
          deft-directory (+deft/org-root-directory)
          deft-recursive t
          deft-default-extension (if (eq profile 'tex) "tex" "org")
          deft-extensions (copy-sequence
                           (or (alist-get profile +deft/profile-extensions)
                               '("org")))
          deft-recursive-ignore-dir-regexp (+deft/ignored-directory-regexp)
          deft-ignore-file-regexp (+deft/ignored-file-regexp))
    (+deft/update-auto-refresh-watch))

  (defun +deft/update-mode-name ()
    "Update `mode-name' for the current Deft profile."
    (setq mode-name
          (pcase +deft/current-profile
            ('tex "Deft[tex]")
            (_ "Deft[org]"))))

  (add-hook 'deft-mode-hook #'+deft/update-mode-name)

  (defun +deft/open-profile (profile)
    "Open Deft with PROFILE."
    (+deft/apply-profile profile)
    (switch-to-buffer deft-buffer)
    (deft-mode)
    (+deft/update-mode-name)
    (force-mode-line-update))

  (defun org-deft-org ()
    "Open the org-focused unified Deft view."
    (interactive)
    (+deft/open-profile 'org))

  (defun org-deft-tex ()
    "Open the tex-focused unified Deft view."
    (interactive)
    (+deft/open-profile 'tex)))

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

(use-package xenops
  :ensure t
  :if (and window-system (not (eq system-type 'windows-nt))) ;; do not load xenops on termial emacs or windows nt
  :config
  (setq xenops-font-family "Maple Mono NL NF CN")
  (setq xenops-reveal-on-entry t)
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
