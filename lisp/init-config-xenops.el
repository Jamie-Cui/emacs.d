;;; init-config-xenops.el --- Xenops configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'subr-x)

(use-package aio
  :ensure t
  :demand t)
(require 'aio)

(declare-function f-read-bytes "f")
(declare-function f-write-bytes "f")
(declare-function fn/xenops-math-advance-buffer-generation "init-config-xenops")
(declare-function fn/xenops-math-buffer-generation "init-config-xenops")
(declare-function fn/xenops-math-buffer-has-overlays-p "init-config-xenops")
(declare-function fn/xenops-math-buffer-has-waiting-overlays-p
                  "init-config-xenops")
(declare-function fn/xenops-math-cancel-refresh-timer "init-config-xenops")
(declare-function fn/xenops-math-cleanup-buffer-h "init-config-xenops")
(declare-function fn/xenops-math-current-generation-p "init-config-xenops")
(declare-function fn/xenops-math-finish-scheduled-refresh
                  "init-config-xenops")
(declare-function fn/xenops-math-latex-create-image-a "init-config-xenops")
(declare-function fn/xenops-math-mode-state-h "init-config-xenops")
(declare-function fn/xenops-math-normalize-idle-semaphore
                  "init-config-xenops")
(declare-function fn/xenops-math-recover-stale-rendered-buffers
                  "init-config-xenops")
(declare-function fn/xenops-math-render-current-scale "init-config-xenops")
(declare-function fn/xenops-math-render-queue-idle-p "init-config-xenops")
(declare-function fn/xenops-math-schedule-current-scale-refresh
                  "init-config-xenops")
(declare-function smartparens-mode "smartparens")
(declare-function xenops-aio-subprocess "xenops-aio")
(declare-function xenops-aio-with-async-with-buffer "xenops-aio")
(declare-function xenops-math-deactivate-marker-on-element "xenops-math")
(declare-function xenops-math-display-error-badge "xenops-math")
(declare-function xenops-math-latex-make-commands "xenops-math-latex")
(declare-function xenops-math-latex-make-latex-document "xenops-math-latex")
(declare-function xenops-math-latex-process-get "xenops-math-latex")
(declare-function xenops-math-parse-element-at "xenops-math")
(declare-function xenops-math-parse-inline-element-at-point "xenops-math")
(declare-function xenops-math-set-marker-on-element "xenops-math")
(declare-function xenops-overlay-delete-overlays-in "xenops-overlay")
(declare-function xenops-png-set-phys-chunk "xenops-png")
(declare-function xenops-render "xenops")

(defvar xenops-apply-user-point)
(defvar xenops-math-latex-max-tasks-in-flight)
(defvar xenops-math-latex-tasks-semaphore)
(defvar xenops-mode)

(defconst fn/xenops-math-image-reference-font-size 14.0
  "Cnfonts font size matched by the reference Xenops image scale.")

(defconst fn/xenops-math-image-reference-scale-factor 0.8
  "Xenops image scale matched by the reference Cnfonts font size.")

(defconst fn/xenops-math-refresh-retry-delay 0.5
  "Seconds to wait before checking whether Xenops rendering is idle.")

(defvar fn/xenops-math-buffer-generations
  (make-hash-table :test #'eq :weakness 'key)
  "Generation numbers used to reject stale Xenops async results.")

(defvar-local fn/xenops-math-refresh-pending nil
  "Non-nil when this buffer needs rendering at the current image scale.")

(defvar-local fn/xenops-math-refresh-timer nil
  "Timer waiting to refresh this buffer at the current image scale.")

(defvar-local fn/xenops-math-smartparens-suspended nil
  "Non-nil when Smartparens is suspended while editing inline math.")

(defvar-local fn/xenops-math-inline-editing-begin nil
  "Beginning of the Xenops inline math element currently being edited.")

(defun fn/xenops-math-inline-editing-element ()
  "Return the Xenops inline math element being edited at point."
  (and (bound-and-true-p xenops-mode)
       (fboundp 'xenops-math-parse-inline-element-at-point)
       (ignore-errors
         (when-let* ((element (xenops-math-parse-inline-element-at-point))
                     (begin-content (plist-get element :begin-content))
                     (end-content (plist-get element :end-content)))
           (when (and (eq (plist-get element :type) 'inline-math)
                      (<= begin-content (point) end-content))
             element)))))

(defun fn/xenops-math-inline-editing-p ()
  "Return non-nil when point is inside a Xenops inline math element."
  (not (null (fn/xenops-math-inline-editing-element))))

(defun fn/xenops-math-sync-smartparens-h ()
  "Suspend Smartparens while editing Xenops inline math."
  (when (fboundp 'smartparens-mode)
    (if-let* ((element (fn/xenops-math-inline-editing-element)))
        (progn
          (setq fn/xenops-math-inline-editing-begin
                (plist-get element :begin))
          (when (bound-and-true-p smartparens-mode)
            (setq fn/xenops-math-smartparens-suspended t)
            (smartparens-mode -1)))
      (setq fn/xenops-math-inline-editing-begin nil)
      (when fn/xenops-math-smartparens-suspended
        (setq fn/xenops-math-smartparens-suspended nil)
        (smartparens-mode 1)))))

(defun fn/xenops-math-restore-smartparens-h ()
  "Restore Smartparens if inline math editing suspended it."
  (when (and fn/xenops-math-smartparens-suspended
             (fboundp 'smartparens-mode))
    (setq fn/xenops-math-smartparens-suspended nil)
    (smartparens-mode 1)))

(defun fn/xenops-math-ignore-repeated-reveal-a (orig-fn window oldpos event-type)
  "Avoid repeated Xenops reveal while editing the same inline math element."
  (if (and (eq event-type 'entered)
           fn/xenops-math-inline-editing-begin
           (when-let* ((element (fn/xenops-math-inline-editing-element)))
             (= (plist-get element :begin)
                fn/xenops-math-inline-editing-begin)))
      nil
    (funcall orig-fn window oldpos event-type)))

(defun fn/xenops-math-setup-smartparens-suspension-h ()
  "Install buffer-local Smartparens suspension for Xenops inline math."
  (add-hook 'pre-command-hook #'fn/xenops-math-sync-smartparens-h nil t)
  (add-hook 'post-command-hook #'fn/xenops-math-sync-smartparens-h nil t)
  (add-hook 'change-major-mode-hook
            #'fn/xenops-math-cleanup-buffer-h nil t)
  (add-hook 'kill-buffer-hook
            #'fn/xenops-math-cleanup-buffer-h nil t))

(use-package xenops
  :ensure t
  :if (and window-system (not (eq system-type 'windows-nt))) ;; do not load xenops on terminal emacs or windows nt
  :config
  (setq xenops-font-family "Maple Mono NL NF CN")
  (setq xenops-reveal-on-entry t)
  (setq xenops-math-latex-process-alist org-preview-latex-process-alist)
  (add-hook 'org-mode-hook #'xenops-mode)
  (setq xenops-math-latex-process 'xdvisvgm) ; HACK or, ximagemagick
  (defun fn/xenops-math-buffer-generation (&optional buffer)
    "Return the Xenops async generation for BUFFER or the current buffer."
    (gethash (or buffer (current-buffer))
             fn/xenops-math-buffer-generations
             0))
  (defun fn/xenops-math-advance-buffer-generation (&optional buffer)
    "Invalidate outstanding Xenops async work for BUFFER or the current buffer."
    (let* ((buffer (or buffer (current-buffer)))
           (generation (1+ (fn/xenops-math-buffer-generation buffer))))
      (puthash buffer generation fn/xenops-math-buffer-generations)
      generation))
  (defun fn/xenops-math-current-generation-p (buffer generation)
    "Return non-nil when BUFFER is live and still at GENERATION."
    (and (buffer-live-p buffer)
         (= generation (fn/xenops-math-buffer-generation buffer))
         (buffer-local-value 'xenops-mode buffer)))
  (aio-defun fn/xenops-math-latex-create-image-a
      (element latex colors cache-file display-image)
    "Create a Xenops math image without leaking work across buffer generations.
This replaces `xenops-math-latex-create-image'.  In particular, it
posts completion to the same semaphore it acquired, even if a major
mode restart has installed a new buffer-local semaphore."
    (let ((buffer (current-buffer))
          (generation (fn/xenops-math-buffer-generation))
          (semaphore xenops-math-latex-tasks-semaphore))
      (aio-await (aio-sem-wait semaphore))
      (condition-case error
          (when (fn/xenops-math-current-generation-p buffer generation)
            (with-current-buffer buffer
              (xenops-math-set-marker-on-element element))
            (let* ((dir (expand-file-name "xenops" temporary-file-directory))
                   (base-name (file-name-base cache-file))
                   (make-file-name
                    (lambda (extension)
                      (expand-file-name
                       (concat base-name "." extension)
                       dir)))
                   (tex-file (funcall make-file-name "tex"))
                   (image-input-file
                    (funcall make-file-name
                             (with-current-buffer buffer
                               (xenops-math-latex-process-get
                                :image-input-type))))
                   (image-output-file
                    (funcall make-file-name
                             (with-current-buffer buffer
                               (xenops-math-latex-process-get
                                :image-output-type))))
                   (commands
                    (with-current-buffer buffer
                      (xenops-math-latex-make-commands
                       element dir tex-file image-input-file
                       image-output-file))))
              (make-directory dir t)
              (aio-await
               (xenops-aio-with-async-with-buffer
                buffer
                (let ((latex-document
                       (xenops-math-latex-make-latex-document latex colors)))
                  (with-temp-file tex-file
                    (insert latex-document)))))
              (dolist (command commands)
                (aio-await (xenops-aio-subprocess command)))
              (aio-await
               (aio-with-async
                 (with-current-buffer buffer
                   (if (and
                        (equal
                         (xenops-math-latex-process-get
                          :image-output-type)
                         "png")
                        (xenops-math-latex-process-get
                         :image-output-ppi))
                       (let ((png-bytes
                              (xenops-png-set-phys-chunk
                               (f-read-bytes image-output-file)
                               (xenops-math-latex-process-get
                                :image-output-ppi))))
                         (f-write-bytes png-bytes cache-file))
                     (copy-file image-output-file cache-file t)))))
              (when (fn/xenops-math-current-generation-p
                     buffer generation)
                (aio-await
                 (xenops-aio-with-async-with-buffer
                  buffer
                  (if-let* ((marker (plist-get element :begin-marker))
                            (current-element
                             (xenops-math-parse-element-at marker)))
                      (funcall display-image current-element commands)
                    (when marker
                      (message "Failed to parse Xenops element at %S"
                               marker)))))
                (with-current-buffer buffer
                  (xenops-math-deactivate-marker-on-element element)))))
        (error
         (when (fn/xenops-math-current-generation-p buffer generation)
           (aio-await
            (xenops-aio-with-async-with-buffer
             buffer
             (when-let*
                 ((current-element
                   (xenops-math-parse-element-at
                    (plist-get element :begin-marker))))
               (xenops-math-display-error-badge
                current-element error
                (and (not xenops-apply-user-point)
                     (<= (xenops-math-latex-waiting-tasks-count)
                         0)))
               (xenops-math-deactivate-marker-on-element element)))))))
      ;; Never post to a newer buffer-local semaphore.  Doing so was the
      ;; source of the observed impossible 64/32 semaphore state.
      (aio-sem-post semaphore)))
  (when (advice-member-p #'fn/xenops-math-latex-create-image-a
                         'xenops-math-latex-create-image)
    (advice-remove 'xenops-math-latex-create-image
                   #'fn/xenops-math-latex-create-image-a))
  (advice-add 'xenops-math-latex-create-image
              :override #'fn/xenops-math-latex-create-image-a)
  (defun fn/xenops-math-current-cnfonts-size (&optional fontsizes-list)
    "Return current cnfonts English font size from FONTSIZES-LIST or active profile."
    (or (and (numberp (car-safe fontsizes-list))
             (float (car fontsizes-list)))
        (and (boundp 'cnfonts--config-info)
             (fboundp 'cnfonts--get-current-profile)
             (let ((size (cdr (assoc (cnfonts--get-current-profile t)
                                     cnfonts--config-info))))
               (and (numberp size) (float size))))
        (let ((height (face-attribute 'default :height nil 'default)))
          (cond
           ((integerp height) (/ height 10.0))
           ((floatp height) (* fn/xenops-math-image-reference-font-size height))))))
  (defun fn/xenops-math-sync-image-scale (&optional fontsizes-list)
    "Sync `xenops-math-image-scale-factor' with current cnfonts size.
Return non-nil when the scale changed."
    (when-let* ((font-size (fn/xenops-math-current-cnfonts-size fontsizes-list))
                (scale (* fn/xenops-math-image-reference-scale-factor
                          (/ font-size fn/xenops-math-image-reference-font-size))))
      (unless (equal xenops-math-image-scale-factor scale)
        (setq xenops-math-image-scale-factor scale)
        t)))
  (defun fn/xenops-math-buffer-has-overlays-p ()
    "Return non-nil when the current buffer has Xenops overlays."
    (save-restriction
      (widen)
      (catch 'found-xenops-overlay
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (when (overlay-get overlay 'xenops-overlay-type)
            (throw 'found-xenops-overlay t))))))
  (defun fn/xenops-math-buffer-has-waiting-overlays-p ()
    "Return non-nil when the current buffer has waiting Xenops overlays."
    (save-restriction
      (widen)
      (catch 'found-xenops-waiting-overlay
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (when (eq (overlay-get overlay 'xenops-overlay-type)
                    'xenops-math-waiting)
            (throw 'found-xenops-waiting-overlay t))))))
  (defun fn/xenops-math-render-queue-idle-p ()
    "Return non-nil when the current Xenops render queue is idle.
A semaphore value above the configured maximum is treated as an
idle but corrupted semaphore left by older Xenops code."
    (let ((semaphore xenops-math-latex-tasks-semaphore))
      (or (null semaphore)
          (and (>= (aio-sem-value semaphore)
                   xenops-math-latex-max-tasks-in-flight)
               (null (car (aio-sem-queue semaphore)))))))
  (defun fn/xenops-math-normalize-idle-semaphore ()
    "Restore the current idle Xenops semaphore to its configured maximum."
    (when (and (fn/xenops-math-render-queue-idle-p)
               (or (null xenops-math-latex-tasks-semaphore)
                   (/= (aio-sem-value xenops-math-latex-tasks-semaphore)
                       xenops-math-latex-max-tasks-in-flight)))
      (setq xenops-math-latex-tasks-semaphore
            (aio-sem xenops-math-latex-max-tasks-in-flight))))
  (defun fn/xenops-math-cancel-refresh-timer ()
    "Cancel the pending scale refresh timer in the current buffer."
    (when (timerp fn/xenops-math-refresh-timer)
      (cancel-timer fn/xenops-math-refresh-timer))
    (setq fn/xenops-math-refresh-timer nil
          fn/xenops-math-refresh-pending nil))
  (defun fn/xenops-math-cleanup-buffer-h ()
    "Invalidate Xenops work before killing or changing the current buffer."
    (fn/xenops-math-advance-buffer-generation)
    (fn/xenops-math-cancel-refresh-timer)
    (fn/xenops-math-restore-smartparens-h)
    (when (bound-and-true-p xenops-mode)
      (save-restriction
        (widen)
        (xenops-cancel-waiting-tasks)
        (xenops-overlay-delete-overlays-in (point-min) (point-max)))))
  (defun fn/xenops-math-mode-state-h ()
    "Invalidate outstanding work after Xenops is disabled manually."
    (unless (bound-and-true-p xenops-mode)
      (fn/xenops-math-advance-buffer-generation)
      (fn/xenops-math-cancel-refresh-timer)
      (fn/xenops-math-restore-smartparens-h)
      (save-restriction
        (widen)
        (xenops-overlay-delete-overlays-in (point-min) (point-max)))))
  (defun fn/xenops-math-render-current-scale ()
    "Replace all Xenops overlays and render using the current image scale."
    (fn/xenops-math-normalize-idle-semaphore)
    (setq fn/xenops-math-refresh-pending nil
          fn/xenops-math-refresh-timer nil)
    (save-excursion
      (save-restriction
        (widen)
        (xenops-overlay-delete-overlays-in (point-min) (point-max))
        (goto-char (point-min))
        (xenops-render))))
  (defun fn/xenops-math-finish-scheduled-refresh (buffer generation)
    "Refresh BUFFER at the current scale once GENERATION becomes idle."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq fn/xenops-math-refresh-timer nil)
        (cond
         ((or (not fn/xenops-math-refresh-pending)
              (not (bound-and-true-p xenops-mode))
              (/= generation (fn/xenops-math-buffer-generation)))
          (setq fn/xenops-math-refresh-pending nil))
         ((fn/xenops-math-render-queue-idle-p)
          (fn/xenops-math-render-current-scale))
         (t
          (setq fn/xenops-math-refresh-timer
                (run-at-time
                 fn/xenops-math-refresh-retry-delay nil
                 #'fn/xenops-math-finish-scheduled-refresh
                 buffer generation)))))))
  (defun fn/xenops-math-schedule-current-scale-refresh ()
    "Schedule a current-scale refresh for the current Xenops buffer."
    (setq fn/xenops-math-refresh-pending t)
    (unless (timerp fn/xenops-math-refresh-timer)
      (setq fn/xenops-math-refresh-timer
            (run-at-time
             0 nil #'fn/xenops-math-finish-scheduled-refresh
             (current-buffer)
             (fn/xenops-math-buffer-generation)))))
  (defun fn/xenops-refresh-rendered-buffers ()
    "Schedule rendered Xenops buffers to refresh after a scale change.
An active render queue is allowed to finish first.  This prevents
hundreds of old promises and waiting overlays from being orphaned
when a large document is refreshed."
    (when (featurep 'xenops)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (bound-and-true-p xenops-mode)
                     (fn/xenops-math-buffer-has-overlays-p))
            (fn/xenops-math-schedule-current-scale-refresh))))))
  (defun fn/xenops-math-recover-stale-rendered-buffers ()
    "Recover idle Xenops buffers that still contain waiting overlays."
    (when (featurep 'xenops)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (bound-and-true-p xenops-mode)
                     (fn/xenops-math-buffer-has-waiting-overlays-p)
                     (fn/xenops-math-render-queue-idle-p))
            (fn/xenops-math-schedule-current-scale-refresh))))))
  (defun fn/xenops-math-sync-image-scale-after-cnfonts (&optional fontsizes-list)
    "Update Xenops math previews after cnfonts applies FONTSIZES-LIST."
    (when (fn/xenops-math-sync-image-scale fontsizes-list)
      (fn/xenops-refresh-rendered-buffers)))
  (defun fn/xenops-math-sync-image-scale-before-render (&rest _)
    "Ensure Xenops uses the current cnfonts size before rendering."
    (fn/xenops-math-sync-image-scale))
  (defun fn/xenops-math-cache-key-include-scale (orig-fn &rest args)
    "Add `xenops-math-image-scale-factor' to Xenops math cache keys."
    (append (apply orig-fn args)
            (list (list 'xenops-math-image-scale-factor
                        xenops-math-image-scale-factor))))
  (fn/xenops-math-sync-image-scale)
  (add-hook 'cnfonts-set-font-finish-hook
            #'fn/xenops-math-sync-image-scale-after-cnfonts)
  (unless (advice-member-p #'fn/xenops-math-sync-image-scale-before-render
                           'xenops-math-render)
    (advice-add 'xenops-math-render
                :before #'fn/xenops-math-sync-image-scale-before-render))
  (unless (advice-member-p #'fn/xenops-math-cache-key-include-scale
                           'xenops-math-file-name-static-hash-data)
    (advice-add 'xenops-math-file-name-static-hash-data
                :around #'fn/xenops-math-cache-key-include-scale))
  (defun fn/xenops-math-display-image-set-svg-foreground (element &rest _)
    "Use SVG's standard initial foreground for Xenops math images."
    (let ((beg (plist-get element :begin))
          (end (plist-get element :end)))
      (when (and beg end)
        (dolist (ov (overlays-in beg end))
          (let ((display (overlay-get ov 'display)))
            (when (and (eq (overlay-get ov 'xenops-overlay-type) 'xenops-overlay)
                       (consp display)
                       (eq (car display) 'image)
                       (eq (plist-get (cdr display) :type) 'svg))
              (overlay-put
               ov 'display
               (cons 'image
                     (plist-put (copy-sequence (cdr display))
                                :foreground "black")))))))))
  (unless (advice-member-p #'fn/xenops-math-display-image-set-svg-foreground
                           'xenops-math-display-image)
    (advice-add 'xenops-math-display-image
                :after #'fn/xenops-math-display-image-set-svg-foreground))
  (add-hook 'xenops-mode-hook #'fn/xenops-math-mode-state-h)
  (add-hook 'org-mode-hook #'fn/xenops-math-setup-smartparens-suspension-h)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (fn/xenops-math-setup-smartparens-suspension-h))))
  (unless (advice-member-p #'fn/xenops-math-ignore-repeated-reveal-a
                           'xenops-math-handle-element-transgression)
    (advice-add 'xenops-math-handle-element-transgression
                :around #'fn/xenops-math-ignore-repeated-reveal-a))
  (fn/xenops-math-recover-stale-rendered-buffers)
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
  (unless (advice-member-p #'fn/xenops-src-parse-at-point
                           'xenops-src-parse-at-point)
    (advice-add 'xenops-src-parse-at-point
                :override #'fn/xenops-src-parse-at-point)))

(provide 'init-config-xenops)
;;; init-config-xenops.el ends here
