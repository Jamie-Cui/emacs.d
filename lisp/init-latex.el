;;; init-latex.el --- latex support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-org)
(require 'cl-lib)
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
  (defun +latex/pdf-tools-sync-view ()
    "View the current TeX PDF with PDF Tools.

Suppress the noisy SyncTeX message shown when point is on a source
line, such as a preamble line, that has no corresponding PDF location."
    (let ((message-function (symbol-function 'message)))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (let ((text (and (stringp format-string)
                                    (apply #'format-message format-string args))))
                     (unless (and text
                                  (string= text "epdfinfo: Destination not found"))
                       (apply message-function format-string args))))))
        (TeX-pdf-tools-sync-view))))

  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list
        '(("PDF Tools" +latex/pdf-tools-sync-view)))

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
