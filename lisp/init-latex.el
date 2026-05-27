;;; init-latex.el --- latex support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-org)
(require 'reftex)

;; -----------------------------------------------------------
;; DONE latex
;;
;; auctex
;; ebib
;; pdf-tools
;; citar
;; citar-embark
;; -----------------------------------------------------------

(use-package auctex
  :ensure t)

(setq +ebib/org-root-dir +emacs/org-root-dir)
(require 'init-config-ebib)

(use-package pdf-tools
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (pdf-tools-install)
  :config
  ;; pdf-tools have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; Enable SyncTeX
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  (setq pdf-sync-forward-display-action nil)

  ;; Set PDF-Tools as the default viewer
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list
        '(("PDF Tools" TeX-pdf-tools-sync-view)))

  ;; Define forward search function
  (defun TeX-pdf-tools-sync-view ()
    (require 'pdf-sync)
    (let ((pdf-file (expand-file-name (TeX-master-file "pdf")))
          (line (line-number-at-pos)))
      (pdf-sync-forward-search line)))

  ;; Enable Inverse Search in PDFs
  (setq pdf-sync-backward-search-method 'generic
        pdf-sync-generic-forward-search-command
        "emacsclient --no-wait +%l '%f'")

  (setq TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))

(setq +citar/org-root-dir +emacs/org-root-dir)
(require 'init-config-citar)

(use-package citar-embark
  :ensure t
  :after (citar embark)
  :no-require
  :config
  (citar-embark-mode))

;; utility function
(defun +latex/isolate-sentence ()
  "Insert a TeX comment line between sentences in the selected region."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (copy-marker (region-end) t)))
        (save-excursion
          (goto-char start)
          (while (re-search-forward "\\.\\(?:[ \t]+\\|\n\\)" end t)
            (unless (save-excursion
                      (skip-chars-forward " \t")
                      (looking-at-p "%"))
              (replace-match ".\n%\n" nil t))))
        (set-marker end nil)
        (message "Replacement done!"))
    (message "No region selected!")))

(require 'init-config-bibtex)

(provide 'init-latex)
