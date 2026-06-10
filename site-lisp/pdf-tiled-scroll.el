;;; pdf-tiled-scroll.el --- Tiled continuous view for pdf-tools -*- lexical-binding: t -*-

;; Author: Jamie's Emacs configuration
;; Package-Requires: ((emacs "30.1") (pdf-tools "1.0"))
;; Keywords: files, multimedia, pdf

;;; Commentary:

;; This package is a lightweight display patch for pdf-tools.  Unlike
;; `pdf-view-roll-minor-mode', it displays each PDF page as a sequence
;; of horizontal tiles, so the bottom of one page and the top of the
;; next page can be visible in the same window.
;;
;; This first version intentionally focuses on visual continuity.  Links,
;; annotations, text selection, and search overlays are not remapped to
;; tile coordinates yet.

;;; Code:

(require 'cl-lib)
(require 'image-mode)
(require 'pdf-cache)
(require 'pdf-view)

(defgroup pdf-tiled-scroll nil
  "Tiled continuous scrolling for pdf-tools."
  :group 'pdf-view)

(defcustom pdf-tiled-scroll-tile-height 512
  "Height in pixels of each rendered page tile."
  :type 'integer
  :group 'pdf-tiled-scroll)

(defcustom pdf-tiled-scroll-page-gap 2
  "Vertical gap in pixels between PDF pages."
  :type 'integer
  :group 'pdf-tiled-scroll)

(defcustom pdf-tiled-scroll-render-margin 4
  "Number of tile lines to render before and after the visible window."
  :type 'integer
  :group 'pdf-tiled-scroll)

(defcustom pdf-tiled-scroll-preload-pages 1
  "Number of pages before and after the visible page to render while idle."
  :type 'integer
  :group 'pdf-tiled-scroll)

(defvar-local pdf-tiled-scroll--overlays nil)
(defvar-local pdf-tiled-scroll--page-images nil)
(defvar-local pdf-tiled-scroll--page-starts nil)
(defvar-local pdf-tiled-scroll--window-width nil)
(defvar-local pdf-tiled-scroll--inhibit-redisplay nil)
(defvar-local pdf-tiled-scroll--pending-pages nil)
(defvar-local pdf-tiled-scroll--render-timer nil)

(defvar pdf-tiled-scroll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap pdf-view-next-page-command] #'pdf-tiled-scroll-next-page)
    (define-key map [remap pdf-view-previous-page-command] #'pdf-tiled-scroll-previous-page)
    (define-key map [remap pdf-view-goto-page] #'pdf-tiled-scroll-goto-page)
    (define-key map [remap pdf-view-scroll-up-or-next-page] #'scroll-up-command)
    (define-key map [remap pdf-view-scroll-down-or-previous-page] #'scroll-down-command)
    map)
  "Keymap used by `pdf-tiled-scroll-mode'.")

;;;###autoload
(define-minor-mode pdf-tiled-scroll-mode
  "Display a pdf-tools buffer as vertically tiled continuous pages."
  :lighter nil
  :keymap pdf-tiled-scroll-mode-map
  (if pdf-tiled-scroll-mode
      (pdf-tiled-scroll--enable)
    (pdf-tiled-scroll--disable)))

(setq minor-mode-alist
      (assq-delete-all 'pdf-tiled-scroll-mode minor-mode-alist))

(defun pdf-tiled-scroll--pdf-view-redisplay-a (window)
  "Handle `pdf-view-redisplay' for tiled buffers.
Return non-nil when the normal pdf-tools redisplay should be skipped."
  (when (and pdf-tiled-scroll-mode
             (not pdf-tiled-scroll--inhibit-redisplay))
    (if (eq window t)
        (dolist (win (get-buffer-window-list nil nil t))
          (when (window-live-p win)
            (pdf-tiled-scroll--rebuild win)))
      (pdf-tiled-scroll--rebuild (or window (selected-window))))
    t))

(advice-add #'pdf-view-redisplay :before-until
            #'pdf-tiled-scroll--pdf-view-redisplay-a)

(defun pdf-tiled-scroll--pdf-view-image-size-a
    (orig-fun &optional displayed-p window page)
  "Return tiled page size without reading overlay display specs.

`pdf-view-image-size' expects the normal pdf-tools single-image display
property.  Tiled buffers use `slice' display specs and placeholders, so
commands like `pdf-view-enlarge' need the page size computed from the
current display scale instead."
  (let ((buffer (and (windowp window) (window-buffer window))))
    (cond
     ((and buffer
           (buffer-live-p buffer))
      (with-current-buffer buffer
        (if (bound-and-true-p pdf-tiled-scroll-mode)
            (pdf-view-desired-image-size
             (or page (ignore-errors (pdf-view-current-page window)) 1)
             window)
          (funcall orig-fun displayed-p window page))))
     ((bound-and-true-p pdf-tiled-scroll-mode)
      (pdf-view-desired-image-size
       (or page (ignore-errors (pdf-view-current-page window)) 1)
       window))
     (t
      (funcall orig-fun displayed-p window page)))))

(advice-add #'pdf-view-image-size :around
            #'pdf-tiled-scroll--pdf-view-image-size-a)

(defun pdf-tiled-scroll--enable ()
  "Enable tiled display in the current PDF buffer."
  (unless (derived-mode-p 'pdf-view-mode)
    (user-error "pdf-tiled-scroll-mode only works in pdf-view-mode buffers"))
  (when (bound-and-true-p pdf-view-roll-minor-mode)
    (pdf-view-roll-minor-mode -1))
  (setq-local mwheel-scroll-up-function (default-value 'mwheel-scroll-up-function)
              mwheel-scroll-down-function (default-value 'mwheel-scroll-down-function))
  (when (local-variable-p 'pixel-scroll-precision-mode)
    (kill-local-variable 'pixel-scroll-precision-mode))
  (remove-hook 'window-configuration-change-hook
               #'image-mode-reapply-winprops t)
  (remove-hook 'window-configuration-change-hook
               #'pdf-view-redisplay-some-windows t)
  (remove-hook 'image-mode-new-window-functions
               #'pdf-view-new-window-function t)
  (setq-local pdf-tiled-scroll--page-images (make-hash-table :test #'eql))
  (setq-local pdf-tiled-scroll--pending-pages nil
              pdf-tiled-scroll--render-timer nil)
  (add-hook 'pre-redisplay-functions #'pdf-tiled-scroll--pre-redisplay nil t)
  (pdf-tiled-scroll--rebuild (selected-window)))

(defun pdf-tiled-scroll--disable ()
  "Disable tiled display and restore the normal pdf-tools view."
  (remove-hook 'pre-redisplay-functions #'pdf-tiled-scroll--pre-redisplay t)
  (pdf-tiled-scroll--cancel-render-timer)
  (setq pdf-tiled-scroll--inhibit-redisplay t)
  (unwind-protect
      (let ((inhibit-read-only t))
        (remove-overlays (point-min) (point-max) 'pdf-tiled-scroll t)
        (erase-buffer)
        (setq pdf-tiled-scroll--overlays nil
              pdf-tiled-scroll--page-images nil
              pdf-tiled-scroll--page-starts nil
              pdf-tiled-scroll--window-width nil
              pdf-tiled-scroll--pending-pages nil)
        (add-hook 'window-configuration-change-hook
                  #'image-mode-reapply-winprops nil t)
        (add-hook 'window-configuration-change-hook
                  #'pdf-view-redisplay-some-windows nil t)
        (add-hook 'image-mode-new-window-functions
                  #'pdf-view-new-window-function nil t)
        (image-mode-window-put 'displayed-pages nil)
        (pdf-view-new-window-function (list (selected-window)))
        (set-buffer-modified-p nil))
    (setq pdf-tiled-scroll--inhibit-redisplay nil)))

(defun pdf-tiled-scroll--pre-redisplay (window)
  "Render visible tiles for WINDOW."
  (with-current-buffer (window-buffer window)
    (when (and pdf-tiled-scroll-mode
               (not pdf-tiled-scroll--inhibit-redisplay)
               (eq (current-buffer) (window-buffer window)))
      (with-demoted-errors "Error in pdf-tiled-scroll redisplay: %S"
        (unless (equal pdf-tiled-scroll--window-width
                       (window-body-width window t))
          (pdf-tiled-scroll--rebuild window))
        (pdf-tiled-scroll--render-visible window)
        (pdf-tiled-scroll--update-current-page window)))))

(defun pdf-tiled-scroll--rebuild (&optional window)
  "Rebuild the tiled buffer layout for WINDOW."
  (setq window (or window (selected-window)))
  (let ((page (or (ignore-errors (pdf-view-current-page window)) 1))
        (inhibit-read-only t))
    (pdf-tiled-scroll--cancel-render-timer)
    (setq pdf-tiled-scroll--inhibit-redisplay t)
    (unwind-protect
        (progn
          (remove-overlays (point-min) (point-max) 'pdf-tiled-scroll t)
          (remove-overlays (point-min) (point-max) 'pdf-view t)
          (erase-buffer)
          (setq pdf-tiled-scroll--overlays nil
                pdf-tiled-scroll--page-images (make-hash-table :test #'eql)
                pdf-tiled-scroll--pending-pages nil
                pdf-tiled-scroll--window-width (window-body-width window t)
                pdf-tiled-scroll--page-starts
                (make-vector (pdf-cache-number-of-pages) nil))
          (dotimes (index (pdf-cache-number-of-pages))
            (pdf-tiled-scroll--insert-page (1+ index) window))
          (goto-char (point-min))
          (pdf-tiled-scroll--goto-page page window)
          (set-buffer-modified-p nil))
      (setq pdf-tiled-scroll--inhibit-redisplay nil))
    (pdf-tiled-scroll--render-visible window)))

(defun pdf-tiled-scroll--insert-page (page window)
  "Insert tiled placeholder rows for PAGE in WINDOW."
  (let* ((size (pdf-view-desired-image-size page window))
         (width (car size))
         (height (cdr size))
         (tile-height (pdf-tiled-scroll--effective-tile-height window))
         (tile-count (ceiling height tile-height)))
    (aset pdf-tiled-scroll--page-starts (1- page) (point))
    (dotimes (tile tile-count)
      (let* ((y (* tile tile-height))
             (display-height (min tile-height (- height y))))
        (pdf-tiled-scroll--insert-tile page y width display-height window)))
    (when (< page (pdf-cache-number-of-pages))
      (pdf-tiled-scroll--insert-gap width window))))

(defun pdf-tiled-scroll--effective-tile-height (window)
  "Return the tile height to use in WINDOW."
  (max 64
       (min (max 1 pdf-tiled-scroll-tile-height)
            (max 64 (/ (window-body-height window t) 2)))))

(defun pdf-tiled-scroll--insert-tile (page y width height window)
  "Insert one tile row for PAGE at vertical offset Y."
  (let* ((start (point))
         (end (progn (insert " \n") (1- (point))))
         (overlay (make-overlay start end nil t nil)))
    (put-text-property start end 'pdf-tiled-scroll-page page)
    (put-text-property start end 'pdf-tiled-scroll-y y)
    (put-text-property start end 'pdf-tiled-scroll-width width)
    (put-text-property start end 'pdf-tiled-scroll-height height)
    (put-text-property start end 'pdf-tiled-scroll-overlay overlay)
    (overlay-put overlay 'pdf-tiled-scroll t)
    (overlay-put overlay 'line-prefix
                 (pdf-tiled-scroll--line-prefix width window))
    (overlay-put overlay 'display
                 `(space :width (,width) :height (,height)))
    (push overlay pdf-tiled-scroll--overlays)))

(defun pdf-tiled-scroll--insert-gap (width window)
  "Insert a visual gap after a page."
  (let* ((start (point))
         (end (progn (insert " \n") (1- (point))))
         (overlay (make-overlay start end nil t nil)))
    (overlay-put overlay 'pdf-tiled-scroll t)
    (overlay-put overlay 'line-prefix
                 (pdf-tiled-scroll--line-prefix width window))
    (overlay-put overlay 'display
                 `(space :width (,width)
                         :height (,(max 0 pdf-tiled-scroll-page-gap))))
    (push overlay pdf-tiled-scroll--overlays)))

(defun pdf-tiled-scroll--line-prefix (width window)
  "Return a line prefix that centers WIDTH in WINDOW."
  (let ((margin (/ (max 0 (- (window-body-width window t) width)) 2)))
    (when (> margin 0)
      `(space :width (,margin)))))

(defun pdf-tiled-scroll--render-visible (window)
  "Render visible and near-visible tiles in WINDOW."
  (let ((start (pdf-tiled-scroll--line-near-window-start window))
        (end (pdf-tiled-scroll--line-near-window-end window))
        visible-pages)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (when-let* ((overlay (get-text-property (point) 'pdf-tiled-scroll-overlay))
                    (page (get-text-property (point) 'pdf-tiled-scroll-page)))
          (cl-pushnew page visible-pages)
          (if (pdf-tiled-scroll--page-image-cached-p page)
              (pdf-tiled-scroll--render-tile overlay)
            (pdf-tiled-scroll--request-page page)))
        (forward-line 1)))
    (dolist (page visible-pages)
      (pdf-tiled-scroll--request-nearby-pages page))))

(defun pdf-tiled-scroll--line-near-window-start (window)
  "Return a tile position before the visible start of WINDOW."
  (save-excursion
    (goto-char (window-start window))
    (forward-line (- pdf-tiled-scroll-render-margin))
    (line-beginning-position)))

(defun pdf-tiled-scroll--line-near-window-end (window)
  "Return a tile position after the visible end of WINDOW."
  (save-excursion
    (goto-char (window-end window t))
    (forward-line pdf-tiled-scroll-render-margin)
    (min (point-max) (line-end-position))))

(defun pdf-tiled-scroll--render-tile (overlay)
  "Render OVERLAY's tile image if its page is cached."
  (unless (overlay-get overlay 'pdf-tiled-scroll-rendered)
    (let* ((pos (overlay-start overlay))
           (page (get-text-property pos 'pdf-tiled-scroll-page))
           (y (get-text-property pos 'pdf-tiled-scroll-y))
           (width (get-text-property pos 'pdf-tiled-scroll-width))
           (height (get-text-property pos 'pdf-tiled-scroll-height))
           (image (gethash page pdf-tiled-scroll--page-images)))
      (when image
        (overlay-put overlay 'display
                     (list (cons 'slice (list 0 y width height)) image))
        (overlay-put overlay 'pdf-tiled-scroll-rendered t)))))

(defun pdf-tiled-scroll--page-image-cached-p (page)
  "Return non-nil if PAGE has already been rendered."
  (gethash page pdf-tiled-scroll--page-images))

(defun pdf-tiled-scroll--render-page (page window)
  "Render PAGE image for WINDOW and cache it."
  (unless (gethash page pdf-tiled-scroll--page-images)
    (puthash page
             (let* ((size (pdf-view-desired-image-size page window))
                    (data (pdf-cache-renderpage
                           page (car size)
                           (if pdf-view-use-scaling
                               (* 2 (car size))
                             (car size)))))
               (pdf-view-create-image data
                 :width (car size)
                 :rotation (or pdf-view--current-rotation 0)
                 :pointer 'arrow))
             pdf-tiled-scroll--page-images)))

(defun pdf-tiled-scroll--request-nearby-pages (page)
  "Request idle rendering for PAGE and nearby pages."
  (let ((first (max 1 (- page pdf-tiled-scroll-preload-pages)))
        (last (min (pdf-cache-number-of-pages)
                   (+ page pdf-tiled-scroll-preload-pages))))
    (cl-loop for page from first to last
             do (pdf-tiled-scroll--request-page page))))

(defun pdf-tiled-scroll--request-page (page)
  "Request idle rendering for PAGE."
  (unless (or (gethash page pdf-tiled-scroll--page-images)
              (memq page pdf-tiled-scroll--pending-pages))
    (push page pdf-tiled-scroll--pending-pages))
  (pdf-tiled-scroll--ensure-render-timer))

(defun pdf-tiled-scroll--ensure-render-timer ()
  "Ensure an idle timer exists for pending page renders."
  (unless (timerp pdf-tiled-scroll--render-timer)
    (setq pdf-tiled-scroll--render-timer
          (run-with-idle-timer
           0.05 nil #'pdf-tiled-scroll--render-next-page
           (current-buffer)))))

(defun pdf-tiled-scroll--cancel-render-timer ()
  "Cancel the pending render timer."
  (when (timerp pdf-tiled-scroll--render-timer)
    (cancel-timer pdf-tiled-scroll--render-timer))
  (setq pdf-tiled-scroll--render-timer nil))

(defun pdf-tiled-scroll--render-next-page (buffer)
  "Render one pending page for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq pdf-tiled-scroll--render-timer nil)
      (when (and pdf-tiled-scroll-mode pdf-tiled-scroll--pending-pages)
        (when-let* ((window (get-buffer-window buffer t)))
          (let ((page (pop pdf-tiled-scroll--pending-pages)))
            (when (and page
                       (<= 1 page)
                       (<= page (pdf-cache-number-of-pages)))
              (pdf-tiled-scroll--render-page page window)
              (pdf-tiled-scroll--render-cached-page-tiles page)
              (force-window-update window))))
        (when pdf-tiled-scroll--pending-pages
          (pdf-tiled-scroll--ensure-render-timer))))))

(defun pdf-tiled-scroll--render-cached-page-tiles (page)
  "Update all visible overlays for cached PAGE."
  (dolist (overlay pdf-tiled-scroll--overlays)
    (when (and (overlay-buffer overlay)
               (eq page (get-text-property (overlay-start overlay)
                                           'pdf-tiled-scroll-page)))
      (pdf-tiled-scroll--render-tile overlay))))

(defun pdf-tiled-scroll--update-current-page (window)
  "Update `pdf-view-current-page' from WINDOW's top tile."
  (when-let* ((page (pdf-tiled-scroll--page-at-pos (window-start window))))
    (setf (pdf-view-current-page window) page)))

(defun pdf-tiled-scroll--page-at-pos (pos)
  "Return the PDF page at POS or nearby."
  (or (get-text-property pos 'pdf-tiled-scroll-page)
      (save-excursion
        (goto-char pos)
        (or (get-text-property (line-beginning-position) 'pdf-tiled-scroll-page)
            (let ((prev (previous-single-property-change
                         (point) 'pdf-tiled-scroll-page)))
              (and prev
                   (get-text-property (max (point-min) (1- prev))
                                      'pdf-tiled-scroll-page)))))))

;;;###autoload
(defun pdf-tiled-scroll-goto-page (page)
  "Go to PAGE in `pdf-tiled-scroll-mode'."
  (interactive
   (list (read-number "Page: " (or (ignore-errors (pdf-view-current-page)) 1))))
  (pdf-tiled-scroll--goto-page page (selected-window)))

(defun pdf-tiled-scroll--goto-page (page window)
  "Go to PAGE in WINDOW."
  (unless pdf-tiled-scroll-mode
    (user-error "pdf-tiled-scroll-mode is not enabled"))
  (let* ((max-page (pdf-cache-number-of-pages))
         (page (min max-page (max 1 page)))
         (pos (aref pdf-tiled-scroll--page-starts (1- page))))
    (unless pos
      (user-error "Page %d has no tiled position" page))
    (goto-char pos)
    (set-window-start window pos t)
    (setf (pdf-view-current-page window) page)
    (pdf-tiled-scroll--render-visible window)))

;;;###autoload
(defun pdf-tiled-scroll-next-page (&optional n)
  "Move forward N pages in `pdf-tiled-scroll-mode'."
  (interactive "p")
  (pdf-tiled-scroll-goto-page
   (+ (or (pdf-view-current-page) 1) (or n 1))))

;;;###autoload
(defun pdf-tiled-scroll-previous-page (&optional n)
  "Move backward N pages in `pdf-tiled-scroll-mode'."
  (interactive "p")
  (pdf-tiled-scroll-next-page (- (or n 1))))

(provide 'pdf-tiled-scroll)

;;; pdf-tiled-scroll.el ends here
