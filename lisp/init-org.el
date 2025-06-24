;;; init-org.el --- org support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-funs)

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
   ))

(use-package org
  :custom
  (org-export-dispatch-use-expert-ui t)
  :config
  (setq org-log-done t)
  (setq org-src-window-setup 'other-window)
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

(provide 'init-org)
