;;; init-org.el --- org support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)

;; defining your own
;; macos: ~/Library/Mobile Documents/com~apple~CloudDocs/org-root
;; linux: ~/org-root
;; it's recommended to symlink your remote file here
;; ln -s ~/Library/Mobile\ Documents/com\~apple\~CloudDocs/org-root .

(make-directory (concat jc-org-root-dir "/roam") t)
(make-directory (concat jc-org-root-dir "/journal") t)
(make-directory (concat jc-org-root-dir "/deft") t)
(make-directory (concat (file-name-directory user-init-file) "/bin") t)

(+ensure-packages-installed
 '(
   org-download
   org-superstar
   org-roam
   org-appear
   ;; better place to write diaries
   org-journal
   ;; deft for note taking
   deft
   ;; allow drawing
   plantuml-mode
   ;; presentation
   org-present
   visual-fill-column
   ))

(use-package visual-fill-column
  :ensure t)

(use-package org-present
  :ensure t
  :config
  (defun +org-present/start ()
    ;; Center the presentation and wrap lines
    (visual-fill-column-mode 1)
    (visual-line-mode 1))

  (defun +org-present/quit ()
    ;; Stop centering the document
    (visual-fill-column-mode 0)
    (visual-line-mode 0))

  (add-hook 'org-present-mode-hook 'org-present-big)
  (add-hook 'org-present-mode-hook 'org-display-inline-images)
  ;; (add-hook 'org-present-mode-hook 'org-present-hide-cursor)
  (add-hook 'org-present-mode-hook 'org-present-read-only)
  (add-hook 'org-present-mode-hook '+org-present/start)
  (add-hook 'org-present-mode-quit-hook 'org-present-small)
  (add-hook 'org-present-mode-quit-hook 'org-remove-inline-images)
  ;; (add-hook 'org-present-mode-quit-hook 'org-present-show-cursor)
  (add-hook 'org-present-mode-quit-hook 'org-present-read-write)
  (add-hook 'org-present-mode-quit-hook '+org-present/quit))

(use-package org
  :custom
  (org-image-actual-width nil)
  (org-export-dispatch-use-expert-ui t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil) ; don't ask, just do it
  (org-link-elisp-confirm-function nil)
  (org-startup-indented t)
  (org-log-done t)
  (org-src-window-setup 'other-window)
  (org-startup-with-inline-images t)
  :config
  (add-to-list 'org-export-backends 'beamer)

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
     )))

(use-package org-journal
  :ensure t
  :custom
  (org-journal-dir (concat jc-org-root-dir "/journal"))
  (org-journal-find-file-fn 'find-file)
  )

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
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

(use-package plantuml-mode
  :ensure t
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
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          )
        )

(setopt org-todo-keyword-faces
        '(("[-]" . +org-todo-active)
          ("[?]" . +org-todo-onhold))
        )

(provide 'init-org)
