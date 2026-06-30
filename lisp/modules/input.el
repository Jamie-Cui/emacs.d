;;; input.el --- input device and zoom adaptation -*- lexical-binding: t -*-
;;; Commentary:
;; PDF/text zoom, pinch and mouse-wheel DWIM handlers.  The keybindings that
;; invoke these live in the `keys' module.
;;; Code:

(declare-function cnfonts-mouse-wheel "cnfonts")
(declare-function image-mode-window-get "image-mode")
(declare-function pdf-cache-pagesize "pdf-cache")
(declare-function pdf-view-enlarge "pdf-view")
(declare-function pdf-view-image-size "pdf-view")
(declare-function pdf-view-redisplay "pdf-view")
(declare-function pdf-view-shrink "pdf-view")
(declare-function pdf-view-scroll-down-or-previous-page "pdf-view")
(declare-function pdf-view-scroll-up-or-next-page "pdf-view")
(declare-function text-scale-pinch "face-remap")

(defvar pdf-view-display-size)

(defvar-local +input/pdf-pinch-start-display-size nil
  "PDF display scale at the beginning of the current pinch gesture.")

(defvar +input/pdf-pinch-redisplay-delay 0.05
  "Minimum seconds between PDF redisplays during a pinch gesture.")

(defvar-local +input/pdf-pinch-redisplay-timer nil
  "Pending timer used to coalesce PDF pinch redisplays.")

(defvar-local +input/pdf-pinch-last-redisplay-time 0.0
  "Last time a PDF pinch redisplay was performed in this buffer.")

(defun +input/text-scale-decrease-dwim ()
  "Decrease buffer text scale, or shrink the PDF view in PDF buffers."
  (interactive)
  (if (and (derived-mode-p 'pdf-view-mode)
           (fboundp 'pdf-view-shrink))
      (call-interactively #'pdf-view-shrink)
    (call-interactively #'text-scale-decrease)))

(defun +input/text-scale-increase-dwim ()
  "Increase buffer text scale, or enlarge the PDF view in PDF buffers."
  (interactive)
  (if (and (derived-mode-p 'pdf-view-mode)
           (fboundp 'pdf-view-enlarge))
      (call-interactively #'pdf-view-enlarge)
    (call-interactively #'text-scale-increase)))

(defun +input/pdf-command-remap (command)
  "Return COMMAND's active remapping, or COMMAND itself."
  (or (command-remapping command) command))

(defun +input/evil-scroll-up-dwim ()
  "Scroll like `evil-scroll-up', using PDF-specific scrolling in PDF buffers."
  (interactive)
  (if (and (derived-mode-p 'pdf-view-mode)
           (fboundp 'pdf-view-scroll-down-or-previous-page))
      (call-interactively
       (+input/pdf-command-remap #'pdf-view-scroll-down-or-previous-page))
    (call-interactively #'evil-scroll-up)))

(defun +input/evil-scroll-down-dwim ()
  "Scroll like `evil-scroll-down', using PDF-specific scrolling in PDF buffers."
  (interactive)
  (if (and (derived-mode-p 'pdf-view-mode)
           (fboundp 'pdf-view-scroll-up-or-next-page))
      (call-interactively
       (+input/pdf-command-remap #'pdf-view-scroll-up-or-next-page))
    (call-interactively #'evil-scroll-down)))

(defun +input/mouse-event-window (event)
  "Return the window from mouse EVENT, or nil when unavailable."
  (let* ((start (or (ignore-errors (event-start event))
                    (and (consp event) (nth 1 event))))
         (window (and (consp start) (posn-window start))))
    (and (windowp window) window)))

(defun +input/pdf-current-page (&optional window)
  "Return WINDOW's current PDF page, or 1 when it is unavailable."
  (let ((page (ignore-errors
                (image-mode-window-get 'page window))))
    (if (integerp page) page 1)))

(defun +input/pdf-current-display-scale (&optional window)
  "Return the current PDF display scale for WINDOW."
  (let* ((window (or window (selected-window)))
         (page (+input/pdf-current-page window))
         (size (pdf-view-image-size nil window page))
         (pagesize (pdf-cache-pagesize page)))
    (/ (float (car size))
       (float (car pagesize)))))

(defun +input/pdf-pinch-redisplay (buffer)
  "Redisplay PDF BUFFER after a pinch zoom update."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq +input/pdf-pinch-redisplay-timer nil
            +input/pdf-pinch-last-redisplay-time (float-time))
      (when (derived-mode-p 'pdf-view-mode)
        (pdf-view-redisplay t)))))

(defun +input/pdf-pinch-queue-redisplay ()
  "Redisplay the current PDF buffer, coalescing rapid pinch events."
  (let* ((now (float-time))
         (delay (- +input/pdf-pinch-redisplay-delay
                   (- now +input/pdf-pinch-last-redisplay-time))))
    (if (<= delay 0)
        (+input/pdf-pinch-redisplay (current-buffer))
      (unless (timerp +input/pdf-pinch-redisplay-timer)
        (setq +input/pdf-pinch-redisplay-timer
              (run-at-time delay nil
                           #'+input/pdf-pinch-redisplay
                           (current-buffer)))))))

(defun +input/pdf-pinch (event)
  "Zoom the current PDF according to pinch EVENT."
  (let ((scale (nth 4 event))
        (dx (nth 2 event))
        (dy (nth 3 event))
        (angle (nth 5 event)))
    (when (and (numberp scale)
               (> scale 0))
      (when (or (not (numberp +input/pdf-pinch-start-display-size))
                (and (numberp dx)
                     (numberp dy)
                     (numberp angle)
                     (zerop dx)
                     (zerop dy)
                     (zerop angle)))
        (setq +input/pdf-pinch-start-display-size
              (+input/pdf-current-display-scale (selected-window))))
      (setq pdf-view-display-size
            (* +input/pdf-pinch-start-display-size scale))
      (+input/pdf-pinch-queue-redisplay))))

(defun +input/pinch-dwim (event)
  "Use touchpad zoom EVENT to zoom PDFs or resize text in normal buffers."
  (interactive "e")
  (let* ((window (or (+input/mouse-event-window event)
                     (selected-window)))
         (buffer (window-buffer window))
         (type (event-basic-type event)))
    (if (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (derived-mode-p 'pdf-view-mode))
             (fboundp 'pdf-view-image-size)
             (fboundp 'pdf-cache-pagesize)
             (fboundp 'pdf-view-redisplay)
             (fboundp 'pdf-view-enlarge)
             (fboundp 'pdf-view-shrink))
        (with-selected-window window
          (pcase type
            ('pinch
             (+input/pdf-pinch event))
            ('magnify-up
             (call-interactively #'pdf-view-enlarge))
            ('magnify-down
             (call-interactively #'pdf-view-shrink))))
      (pcase type
        ('pinch
         (text-scale-pinch event))
        ('magnify-up
         (call-interactively #'text-scale-increase))
        ('magnify-down
         (call-interactively #'text-scale-decrease))))))

(defun +input/cnfonts-mouse-wheel-dwim (event)
  "Use mouse wheel EVENT to zoom PDFs or resize fonts via cnfonts."
  (interactive (list last-input-event))
  (let* ((window (or (+input/mouse-event-window event)
                     (selected-window)))
         (buffer (window-buffer window))
         (type (event-basic-type event)))
    (if (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (derived-mode-p 'pdf-view-mode))
             (memq type '(wheel-up wheel-down mouse-4 mouse-5))
             (fboundp 'pdf-view-enlarge)
             (fboundp 'pdf-view-shrink))
        (with-selected-window window
          (pcase type
            ((or 'wheel-up 'mouse-4)
             (call-interactively #'pdf-view-enlarge))
            ((or 'wheel-down 'mouse-5)
             (call-interactively #'pdf-view-shrink))))
      (cnfonts-mouse-wheel event))))

(provide 'init-input)
;;; input.el ends here
