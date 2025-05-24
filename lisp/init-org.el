;;; init-org.el --- org support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-utils)
(require 'org)
(require 'ox-latex)

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

(+ensure-packages-installed
 '(
   org-download
   org-superstar
   org-roam
   org-appear
   ;; better place to write diaries
   org-journal
   ;; enrich org mode
   citar
   ;; deft for note taking
   deft
   ;; preview org math
   xenops
   ;; allow drawing
   plantuml-mode
   ;; export org code in colors
   engrave-faces
   ;; previwing pdfs
   pdf-tools
   ))

(use-package org
  :custom
  (org-export-dispatch-use-expert-ui t)
  :config
  (add-to-list 'org-latex-packages-alist
               '("lambda, advantage, operators, sets, adversary, landau,\
 probability, notions, logic, ff, mm, primitives, events, complexity, oracles,\
 asymptotics, keys" "cryptocode" t))
  (add-to-list 'org-latex-packages-alist
               '("" "booktabs" t))
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

(use-package ox-latex
  :after (engrave-faces citar)
  :custom
  (org-export-with-toc nil)
  :config
  (setq org-latex-src-block-backend 'engraved)
  (setq org-latex-engraved-theme 't)
  ;; this var is used by org-export
  (add-to-list 'org-cite-global-bibliography (concat +my-org-root-dir "/zotero_all.bib"))
  )

(use-package org-journal
  :ensure t
  :custom
  (org-journal-dir (concat +my-org-root-dir "/journal"))
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

(use-package citar
  :ensure t
  :config
  (add-to-list 'citar-bibliography (concat +my-org-root-dir "/zotero_all.bib"))
  (add-to-list 'citar-notes-paths (concat +my-org-root-dir "/roam"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

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
  (deft-directory (concat +my-org-root-dir "/deft"))

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

;; (use-package xenops
;;   :ensure t
;;   :if window-system ;; do not load xenops on termial emacs
;;   :config
;;   (add-hook 'org-mode-hook #'xenops-mode)
;;   (setq xenops-math-image-current-scale-factor 1.2)
;;   (setq xenops-math-image-margin 0)
;;   ;; HACK error from xenops with org>9.7
;;   ;; https://github.com/syl20bnr/spacemacs/issues/16577
;;   ;; https://github.com/dandavison/xenops/pull/74/files
;;   ;; https://github.com/dandavison/xenops/issues/73
;;   (defun fn/xenops-src-parse-at-point ()
;;     (-if-let* ((element (xenops-parse-element-at-point 'src))
;;                (org-babel-info
;;                 (xenops-src-do-in-org-mode
;;                  (org-babel-get-src-block-info 'light (org-element-context)))))
;;         (xenops-util-plist-update
;;          element
;;          :type 'src
;;          :language (nth 0 org-babel-info)
;;          :org-babel-info org-babel-info)))

;;   (advice-add 'xenops-src-parse-at-point
;;               :override 'fn/xenops-src-parse-at-point)
;;   )

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

(use-package org-appear
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

(use-package engrave-faces
  :ensure t)

(use-package pdf-tools
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-loader-install)
  (add-hook
   'org-mode-hook
   '(lambda ()
      (delete '("\\.pdf\\'" . default) org-file-apps)
      (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdftools-open))))
  )

(provide 'init-org)
