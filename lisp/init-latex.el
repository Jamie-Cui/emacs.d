;;; init-latex.el --- latex support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-org)
(require 'reftex)

;; -----------------------------------------------------------
;; DONE bib
;;
;; ebib
;; pdf-tools
;; citar
;; citar-embark
;; -----------------------------------------------------------

;; default bib file
(add-to-list 'bibtex-files 
             (concat +emacs/org-root-dir "/all-ref.bib"))

(use-package ebib
  :ensure t
  :custom
  (ebib-layout 'index-only)
  (ebib-popup-entry-window nil)
  (ebib-window-vertical-split nil)
  :config
  (add-to-list 'ebib-preload-bib-files 
               (concat +emacs/org-root-dir "/all-ref.bib")))

(use-package pdf-tools
  :ensure t
  :if (not (eq system-type 'windows-nt)) 
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode) 
  :magic ("%PDF" . pdf-view-mode) 
  :custom
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  :init
  (pdf-tools-install)
  :config
  ;; pdf-tools have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions 
            #'TeX-revert-document-buffer)

  ;; Enable SyncTeX
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)

  ;; Set PDF-Tools as the default viewer
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list 
        '(("PDF Tools" TeX-pdf-tools-sync-view)))

  ;; Define forward search function
  (defun TeX-pdf-tools-sync-view ()
    (require 'pdf-sync)
    (let ((pdf-file (expand-file-name (TeX-master-file "pdf")))
          (tex-file buffer-file-name) ; unused
          (line (line-number-at-pos)))
      (pdf-sync-forward-search line)))

  ;; Enable Inverse Search in PDFs
  (setq pdf-sync-backward-search-method 'generic
        pdf-sync-generic-forward-search-command
        "emacsclient --no-wait +%l '%f'")

  (setq TeX-PDF-mode t) 
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))

(use-package citar
  :ensure t
  ;; :custom
  ;; (org-cite-insert-processor 'citar)
  ;; (org-cite-follow-processor 'citar)
  ;; (org-cite-activate-processor 'citar)
  :config
  (setq citar-at-point-function 'embark-act)
  (add-to-list 'citar-bibliography 
               (concat +emacs/org-root-dir "/all-ref.bib"))
  (add-to-list 'citar-library-paths 
               (concat +emacs/org-root-dir "/pdf"))
  (add-to-list 'citar-notes-paths 
               (concat +emacs/org-root-dir "/roam"))
  ;; NOTE this var is used by org-export
  (add-to-list 'org-cite-global-bibliography 
               (concat +emacs/org-root-dir "/all-ref.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :ensure t
  :after (citar embark)
  :no-require
  :config
  (citar-embark-mode))

;; utility function
(defun +latex/isolate-sentence ()
  "Replace '. ' with '.\n%\n' in the selected region, similar to Vim's :'<,'>s/\. /.\n%\n/g."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          (while (search-forward ". " end t)
            (replace-match ".\n%\n" nil t))))
    (message "No region selected!"))
  (message "Replacement done!"))

;; Configure uniquification (appends a/b/c for duplicates)
(setopt bibtex-autokey-name-case-convert-function #'upcase)
(setopt bibtex-autokey-names 4)
(setopt bibtex-autokey-additional-names "+")
(setopt bibtex-autokey-name-stretch 0)
(setopt bibtex-autokey-name-length 1)
(setopt bibtex-autokey-year-length 4)
(setopt bibtex-autokey-titlewords 0)
(setopt bibtex-autokey-titlewords-stretch 0)
(setopt bibtex-autokey-name-year-separator "")

;; HACK override this function from built-in
;; maybe report upstream?
(defun bibtex-autokey-abbrev (string len)
  "Return an abbreviation of STRING with at least LEN characters.
If LEN is positive the abbreviation is terminated only after a consonant
or at the word end.  If LEN is negative the abbreviation is strictly
enforced using abs (LEN) characters.  If LEN is not a number, STRING
is returned unchanged."
  (cond ((or (not (numberp len))
             (<= (length string) (abs len)))
         string)
        ((equal len 0)
         "")
        ((equal len 1)
         (substring string 0 1))
        ((< len 0)
         (substring string 0 (abs len)))
        (t (let* ((case-fold-search t)
                  (abort-char (string-match "[^aeiou]" string (1- len))))
             (message "%s" abort-char)
             (if abort-char
                 (substring string 0 (1+ abort-char))
               string)))))


(provide 'init-latex)
