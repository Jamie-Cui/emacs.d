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

(declare-function pdf-cache-lookup-image "pdf-cache")
(declare-function pdf-cache-number-of-pages "pdf-cache")
(declare-function pdf-cache-put-image "pdf-cache")
(declare-function pdf-info-renderpage "pdf-info")
(declare-function mwheel-event-window "mwheel")
(declare-function pdf-roll-scroll-backward "pdf-roll")
(declare-function pdf-roll-scroll-forward "pdf-roll")
(declare-function pdf-view-current-page "pdf-view")
(declare-function pdf-view-desired-image-size "pdf-view")

(defvar pdf-info-asynchronous)
(defvar pdf-view-roll-minor-mode)
(defvar pdf-view-use-scaling)

(defvar +pdf-tools/roll-prefetch-pages 2
  "Number of nearby pages to prefetch ahead and behind in PDF roll mode.")

(defvar +pdf-tools/roll-prefetch-max-pending 4
  "Maximum number of in-flight PDF roll page prefetch requests.")

(defvar-local +pdf-tools/roll-prefetch-pending nil
  "In-flight PDF roll prefetch keys for the current buffer.")

(defun +pdf-tools/roll-pre-redisplay-a (fn window)
  "Prefetch nearby pages after `pdf-roll-pre-redisplay'."
  (prog1 (funcall fn window)
    (+pdf-tools/roll-prefetch-nearby window)))

(defun +pdf-tools/roll-event-window (event)
  "Return the live window associated with wheel EVENT."
  (let ((window (ignore-errors (mwheel-event-window event))))
    (when (framep window)
      (setq window (frame-selected-window window)))
    (and (window-live-p window) window)))

(defun +pdf-tools/roll-event-delta (event)
  "Return vertical pixel delta from wheel EVENT, or nil."
  (or (when-let* ((delta (nth 4 event)))
        (round (cdr delta)))
      (let ((plist (nth 3 event)))
        (when (listp plist)
          (when-let* ((delta (or (plist-get plist :scrolling-delta-y)
                                 (when (plist-member plist :delta-y)
                                   (* (plist-get plist :delta-y)
                                      (frame-char-height))))))
            (round delta))))))

(defun +pdf-tools/roll-scroll-by-pixels (delta window)
  "Scroll PDF roll WINDOW by DELTA pixels using `pdf-roll' commands."
  (unless (zerop delta)
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        (if (> delta 0)
            (pdf-roll-scroll-backward delta window t)
          (pdf-roll-scroll-forward (- delta) window t))
        (+pdf-tools/roll-prefetch-nearby window)))))

(defun +pdf-tools/roll-ultra-scroll-a (fn event &optional arg)
  "Use `pdf-roll' scrolling instead of `ultra-scroll' in PDF roll buffers."
  (let* ((window (+pdf-tools/roll-event-window event))
         (delta (+pdf-tools/roll-event-delta event)))
    (if (and window
             delta
             (with-current-buffer (window-buffer window)
               (bound-and-true-p pdf-view-roll-minor-mode)))
        (+pdf-tools/roll-scroll-by-pixels delta window)
      (funcall fn event arg))))

(defun +pdf-tools/roll-prefetch-candidates (page max-page)
  "Return nearby roll prefetch candidates around PAGE up to MAX-PAGE."
  (let (pages)
    (dotimes (index +pdf-tools/roll-prefetch-pages)
      (let ((offset (1+ index)))
        (when (<= (+ page offset) max-page)
          (push (+ page offset) pages))
        (when (>= (- page offset) 1)
          (push (- page offset) pages))))
    (nreverse pages)))

(defun +pdf-tools/roll-prefetch-key (page width max-width)
  "Return the cache key used for prefetching PAGE at WIDTH/MAX-WIDTH."
  (list page width max-width))

(defun +pdf-tools/roll-prefetch-page (page window)
  "Asynchronously prefetch PAGE for PDF roll WINDOW."
  (let* ((size (pdf-view-desired-image-size page window))
         (width (car size))
         (max-width (if pdf-view-use-scaling (* 2 width) width))
         (key (+pdf-tools/roll-prefetch-key page width max-width)))
    (unless (or (pdf-cache-lookup-image page width max-width)
                (member key +pdf-tools/roll-prefetch-pending)
                (>= (length +pdf-tools/roll-prefetch-pending)
                    +pdf-tools/roll-prefetch-max-pending))
      (push key +pdf-tools/roll-prefetch-pending)
      (let* ((buffer (current-buffer))
             (pdf-info-asynchronous
              (lambda (status data)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (setq +pdf-tools/roll-prefetch-pending
                          (delete key +pdf-tools/roll-prefetch-pending))
                    (when (and (null status) data)
                      (pdf-cache-put-image page width data)))))))
        (condition-case nil
            (pdf-info-renderpage page width)
          (error
           (setq +pdf-tools/roll-prefetch-pending
                 (delete key +pdf-tools/roll-prefetch-pending))))))))

(defun +pdf-tools/roll-prefetch-nearby (window)
  "Prefetch pages near WINDOW's current PDF roll page."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (bound-and-true-p pdf-view-roll-minor-mode))
    (let* ((page (or (ignore-errors (pdf-view-current-page window)) 1))
           (page (if (integerp page) page (truncate page)))
           (max-page (pdf-cache-number-of-pages)))
      (dolist (candidate (+pdf-tools/roll-prefetch-candidates page max-page))
        (+pdf-tools/roll-prefetch-page candidate window)))))

(defun +pdf-tools/roll-setup ()
  "Enable continuous PDF scrolling with trackpad-friendly wheel handling."
  (pdf-view-roll-minor-mode 1)
  (setq-local mouse-wheel-scroll-amount '(5 ((shift) . 1)))
  (setq-local mouse-wheel-progressive-speed nil)
  (when (local-variable-p 'pixel-scroll-precision-mode)
    (kill-local-variable 'pixel-scroll-precision-mode))
  (when (boundp 'mwheel-coalesce-scroll-events)
    (setq-local mwheel-coalesce-scroll-events nil))
  (+pdf-tools/roll-prefetch-nearby (selected-window)))

(use-package pdf-tools
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (pdf-view-mode . +pdf-tools/roll-setup)
  :custom
  (pdf-view-use-scaling nil)
  :init
  (pdf-tools-install)
  :config
  (with-eval-after-load 'pdf-roll
    (when (advice-member-p #'+pdf-tools/roll-ultra-scroll-a
                           'pdf-roll-pre-redisplay)
      (advice-remove 'pdf-roll-pre-redisplay
                     #'+pdf-tools/roll-ultra-scroll-a))
    (unless (advice-member-p #'+pdf-tools/roll-pre-redisplay-a
                             'pdf-roll-pre-redisplay)
      (advice-add 'pdf-roll-pre-redisplay :around
                  #'+pdf-tools/roll-pre-redisplay-a)))

  (with-eval-after-load 'ultra-scroll
    (when (fboundp 'ultra-scroll)
      (unless (advice-member-p #'+pdf-tools/roll-ultra-scroll-a
                               'ultra-scroll)
        (advice-add 'ultra-scroll :around
                    #'+pdf-tools/roll-ultra-scroll-a)))
    (when (fboundp 'ultra-scroll-mac)
      (unless (advice-member-p #'+pdf-tools/roll-ultra-scroll-a
                               'ultra-scroll-mac)
        (advice-add 'ultra-scroll-mac :around
                    #'+pdf-tools/roll-ultra-scroll-a))))

  ;; pdf-tools have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (general-define-key
   :keymaps 'pdf-view-mode-map
   :states '(normal visual motion)
   "-" #'ignore
   "=" #'ignore
   )

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
