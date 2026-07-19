;;; agent-shell-permission-transient.el --- Transient permission UI for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Package-Requires: ((emacs "30.1") (agent-shell "0.60.2") (transient "0.11.0"))

;;; Commentary:

;; Replace agent-shell's inline permission card with a compact Transient menu.
;; Requests are queued globally, while responses continue to flow through
;; `agent-shell-permission-responder-function'.

;;; Code:

(require 'agent-shell)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defgroup agent-shell-permission-transient nil
  "Transient permission prompts for `agent-shell'."
  :group 'agent-shell)

(defcustom agent-shell-permission-transient-summary-width 88
  "Maximum display width of a permission summary."
  :type 'integer
  :group 'agent-shell-permission-transient)

(cl-defstruct (agent-shell-permission-transient--request
               (:constructor agent-shell-permission-transient--request-create))
  "A pending `agent-shell' permission request."
  id
  permission
  shell-buffer)

(defvar agent-shell-permission-transient--queue nil
  "Pending permission requests in arrival order.")

(defvar agent-shell-permission-transient--current nil
  "Permission request currently displayed by the transient.")

(defvar agent-shell-permission-transient--previous-responder nil
  "Responder installed before `agent-shell-permission-transient-mode'.")

(defvar agent-shell-permission-transient--popup-timer nil
  "Timer used to show a queued permission when interactive state is safe.")

(defvar agent-shell-permission-transient--subscriptions
  (make-hash-table :test #'eq)
  "Map shell buffers to `agent-shell' event subscription tokens.")

(defvar-local agent-shell-permission-transient--reminder-overlay nil
  "Overlay displaying this buffer's pending permission reminder.")

(defvar agent-shell-permission-transient-mode)

(defun agent-shell-permission-transient-pending-count ()
  "Return the number of pending permission requests."
  (length agent-shell-permission-transient--queue))

(defun agent-shell-permission-transient--buffer-pending-count (shell-buffer)
  "Return the number of pending requests belonging to SHELL-BUFFER."
  (seq-count
   (lambda (request)
     (eq shell-buffer
         (agent-shell-permission-transient--request-shell-buffer request)))
   agent-shell-permission-transient--queue))

(defun agent-shell-permission-transient--reminder-text (count)
  "Return the in-buffer permission reminder for COUNT requests."
  (concat
   (if (and (> (point-max) (point-min))
            (not (eq (char-before (point-max)) ?\n)))
       "\n"
     "")
   "  "
   (propertize
    (if (= count 1)
        "Permission pending"
      (format "%d permissions pending" count))
    'face 'warning)
   " — press "
   (propertize "C-c C-p" 'face 'help-key-binding)
   " to reopen the permission menu\n"))

(defun agent-shell-permission-transient--delete-reminder (shell-buffer)
  "Delete the pending permission reminder from SHELL-BUFFER."
  (when (buffer-live-p shell-buffer)
    (with-current-buffer shell-buffer
      (when (overlayp agent-shell-permission-transient--reminder-overlay)
        (delete-overlay agent-shell-permission-transient--reminder-overlay))
      (setq agent-shell-permission-transient--reminder-overlay nil))))

(defun agent-shell-permission-transient--sync-reminder (shell-buffer)
  "Synchronize SHELL-BUFFER's reminder with its pending requests."
  (when (buffer-live-p shell-buffer)
    (let ((count
           (agent-shell-permission-transient--buffer-pending-count
            shell-buffer)))
      (if (= count 0)
          (agent-shell-permission-transient--delete-reminder shell-buffer)
        (with-current-buffer shell-buffer
          (save-restriction
            (widen)
            (let ((position (point-max)))
              (unless (overlayp
                       agent-shell-permission-transient--reminder-overlay)
                (setq agent-shell-permission-transient--reminder-overlay
                      (make-overlay position position shell-buffer t t))
                (overlay-put agent-shell-permission-transient--reminder-overlay
                             'priority 1000)
                (overlay-put agent-shell-permission-transient--reminder-overlay
                             'agent-shell-permission-transient t))
              (move-overlay agent-shell-permission-transient--reminder-overlay
                            position position shell-buffer)
              (overlay-put
               agent-shell-permission-transient--reminder-overlay
               'after-string
               (agent-shell-permission-transient--reminder-text count)))))))))

(defun agent-shell-permission-transient--lighter ()
  "Return the mode-line lighter for pending permissions."
  (when-let* ((count (agent-shell-permission-transient-pending-count))
              ((> count 0)))
    (propertize (format " Perm:%d" count) 'face 'warning)))

(defun agent-shell-permission-transient--permission-id (permission)
  "Return the request identifier from PERMISSION."
  (map-elt (map-elt permission :tool-call) :permission-request-id))

(defun agent-shell-permission-transient--find (shell-buffer request-id)
  "Find the request for SHELL-BUFFER with REQUEST-ID."
  (seq-find
   (lambda (request)
     (and (eq shell-buffer
              (agent-shell-permission-transient--request-shell-buffer request))
          (equal request-id
                 (agent-shell-permission-transient--request-id request))))
   agent-shell-permission-transient--queue))

(defun agent-shell-permission-transient--remove (request)
  "Remove REQUEST from the pending queue and select its successor."
  (let ((shell-buffer
         (agent-shell-permission-transient--request-shell-buffer request)))
    (when (memq request agent-shell-permission-transient--queue)
      (let* ((index (seq-position agent-shell-permission-transient--queue
                                  request #'eq))
             (remaining (delq request
                              (copy-sequence
                               agent-shell-permission-transient--queue))))
        (setq agent-shell-permission-transient--queue remaining)
        (when (eq request agent-shell-permission-transient--current)
          (setq agent-shell-permission-transient--current
                (when remaining
                  (nth (min (or index 0) (1- (length remaining)))
                       remaining))))))
    (agent-shell-permission-transient--sync-reminder shell-buffer))
  request)

(defun agent-shell-permission-transient--remove-by-id (shell-buffer request-id)
  "Remove SHELL-BUFFER's permission REQUEST-ID from the queue."
  (when-let* ((request (agent-shell-permission-transient--find
                        shell-buffer request-id)))
    (agent-shell-permission-transient--remove request)
    (agent-shell-permission-transient--schedule-popup)))

(defun agent-shell-permission-transient--remove-shell (shell-buffer)
  "Remove all queued permissions belonging to SHELL-BUFFER."
  (dolist (request (copy-sequence agent-shell-permission-transient--queue))
    (when (eq shell-buffer
              (agent-shell-permission-transient--request-shell-buffer request))
      (agent-shell-permission-transient--remove request)))
  (agent-shell-permission-transient--delete-reminder shell-buffer)
  (remhash shell-buffer agent-shell-permission-transient--subscriptions)
  (agent-shell-permission-transient--schedule-popup))

(defun agent-shell-permission-transient--on-response (shell-buffer event)
  "Remove a resolved request for SHELL-BUFFER described by EVENT."
  (agent-shell-permission-transient--remove-by-id
   shell-buffer (map-nested-elt event '(:data :request-id))))

(defun agent-shell-permission-transient--ensure-subscriptions (shell-buffer)
  "Subscribe to request lifecycle events for SHELL-BUFFER once."
  (unless (gethash shell-buffer
                   agent-shell-permission-transient--subscriptions)
    (puthash
     shell-buffer
     (list
      (agent-shell-subscribe-to
       :shell-buffer shell-buffer
       :event 'permission-response
       :on-event
       (lambda (event)
         (agent-shell-permission-transient--on-response shell-buffer event)))
      (agent-shell-subscribe-to
       :shell-buffer shell-buffer
       :event 'clean-up
       :on-event
       (lambda (_event)
         (agent-shell-permission-transient--remove-shell shell-buffer))))
     agent-shell-permission-transient--subscriptions)))

(defun agent-shell-permission-transient--shell-buffer ()
  "Return the `agent-shell' buffer handling the current permission request."
  (cond
   ((derived-mode-p 'agent-shell-mode) (current-buffer))
   ((ignore-errors
      (agent-shell-shell-buffer :no-error t :no-create t)))))

(defun agent-shell-permission-transient--enqueue (permission shell-buffer)
  "Queue PERMISSION originating in SHELL-BUFFER and return non-nil."
  (let* ((request-id
          (agent-shell-permission-transient--permission-id permission))
         (existing
          (agent-shell-permission-transient--find shell-buffer request-id)))
    (if existing
        (setf (agent-shell-permission-transient--request-permission existing)
              permission)
      (let ((request
             (agent-shell-permission-transient--request-create
              :id request-id
              :permission permission
              :shell-buffer shell-buffer)))
        (setq agent-shell-permission-transient--queue
              (append agent-shell-permission-transient--queue (list request)))
        (unless agent-shell-permission-transient--current
          (setq agent-shell-permission-transient--current request))))
    (agent-shell-permission-transient--ensure-subscriptions shell-buffer)
    (agent-shell-permission-transient--sync-reminder shell-buffer)
    (agent-shell-permission-transient--schedule-popup)
    t))

(defun agent-shell-permission-transient--responder (permission)
  "Handle `agent-shell' PERMISSION or delegate to the previous responder."
  (or (and (functionp agent-shell-permission-transient--previous-responder)
           (not (eq agent-shell-permission-transient--previous-responder
                    #'agent-shell-permission-transient--responder))
           (funcall agent-shell-permission-transient--previous-responder
                    permission))
      (when-let* ((shell-buffer
                   (agent-shell-permission-transient--shell-buffer)))
        (agent-shell-permission-transient--enqueue permission shell-buffer))))

(defun agent-shell-permission-transient--target-window ()
  "Return the visible window for the current permission's agent shell."
  (when-let* ((request agent-shell-permission-transient--current)
              (shell-buffer
               (agent-shell-permission-transient--request-shell-buffer request))
              ((buffer-live-p shell-buffer)))
    (get-buffer-window shell-buffer t)))

(defun agent-shell-permission-transient--popup-safe-p ()
  "Return non-nil when showing a new transient will not disrupt input."
  (and agent-shell-permission-transient-mode
       agent-shell-permission-transient--queue
       (not noninteractive)
       (not executing-kbd-macro)
       (not (active-minibuffer-window))
       (not (transient-active-prefix))
       (window-live-p (agent-shell-permission-transient--target-window))))

(defun agent-shell-permission-transient--cancel-popup-timer ()
  "Cancel the pending permission popup timer, if any."
  (when (timerp agent-shell-permission-transient--popup-timer)
    (cancel-timer agent-shell-permission-transient--popup-timer))
  (setq agent-shell-permission-transient--popup-timer nil))

(defun agent-shell-permission-transient--schedule-popup (&optional delay)
  "Show a permission transient after DELAY when interactive state is safe."
  (unless (timerp agent-shell-permission-transient--popup-timer)
    (setq agent-shell-permission-transient--popup-timer
          (run-at-time
           (or delay 0)
           nil
           (lambda ()
             (setq agent-shell-permission-transient--popup-timer nil)
             (cond
              ((agent-shell-permission-transient--popup-safe-p)
               (agent-shell-permission-transient-menu))
              ((and agent-shell-permission-transient-mode
                    agent-shell-permission-transient--queue)
               (agent-shell-permission-transient--schedule-popup 0.25))))))))

(defun agent-shell-permission-transient--current-permission ()
  "Return the current permission payload."
  (when agent-shell-permission-transient--current
    (agent-shell-permission-transient--request-permission
     agent-shell-permission-transient--current)))

(defun agent-shell-permission-transient--current-tool-call ()
  "Return the current permission's tool-call alist."
  (map-elt (agent-shell-permission-transient--current-permission) :tool-call))

(defun agent-shell-permission-transient--command-string (value)
  "Convert command VALUE into a readable single-line string."
  (cond
   ((stringp value) value)
   ((vectorp value)
    (mapconcat (lambda (part) (format "%s" part)) value " "))
   ((and (listp value) (seq-every-p #'stringp value))
    (string-join value " "))
   (t nil)))

(defun agent-shell-permission-transient--summary ()
  "Return a compact summary of the current permission request."
  (let* ((tool-call (agent-shell-permission-transient--current-tool-call))
         (raw-input (map-elt tool-call :raw-input))
         (command
          (agent-shell-permission-transient--command-string
           (map-elt raw-input 'command)))
         (detail
          (or command
              (seq-some
               (lambda (key)
                 (when-let* ((value (map-elt raw-input key))
                             ((stringp value))
                             ((not (string-empty-p value))))
                   value))
               '(description filepath fileName path file_path))
              (map-elt tool-call :title)
              "Permission requested"))
         (single-line (replace-regexp-in-string
                       (rx (+ (or space "\n" "\r" "\t"))) " " detail)))
    (truncate-string-to-width
     single-line agent-shell-permission-transient-summary-width nil nil "…")))

(defun agent-shell-permission-transient--agent-name (request)
  "Return the display name of REQUEST's agent."
  (let ((shell-buffer
         (agent-shell-permission-transient--request-shell-buffer request)))
    (if (buffer-live-p shell-buffer)
        (or (map-elt (agent-shell-get-config shell-buffer) :mode-line-name)
            "Agent")
      "Agent")))

(defun agent-shell-permission-transient--working-directory (request)
  "Return the abbreviated working directory for REQUEST."
  (let ((shell-buffer
         (agent-shell-permission-transient--request-shell-buffer request)))
    (if (buffer-live-p shell-buffer)
        (with-current-buffer shell-buffer
          (directory-file-name (abbreviate-file-name default-directory)))
      "closed buffer")))

(defun agent-shell-permission-transient--field-line (label value)
  "Format LABEL and VALUE as a highlighted, single-line field."
  (let* ((prefix (format "%s: " label))
         (value-width
          (max 1 (- agent-shell-permission-transient-summary-width
                    (string-width prefix)))))
    (concat
     (propertize prefix 'face 'transient-heading)
     (propertize
      (truncate-string-to-width value value-width nil nil "…")
      'face 'transient-value))))

(defun agent-shell-permission-transient--heading ()
  "Return the dynamic heading for the permission transient."
  (if-let* ((request agent-shell-permission-transient--current)
            (tool-call (agent-shell-permission-transient--current-tool-call)))
      (let* ((index (1+ (or (seq-position
                             agent-shell-permission-transient--queue
                             request #'eq)
                            0)))
             (count (agent-shell-permission-transient-pending-count))
             (kind (or (map-elt tool-call :kind) "tool")))
        (concat
         (propertize
          (format "Permission %d/%d · %s · %s"
                  index count
                  (agent-shell-permission-transient--agent-name request)
                  kind)
          'face 'transient-heading)
         "\n"
         (agent-shell-permission-transient--field-line
          "Dir"
          (agent-shell-permission-transient--working-directory request))
         "\n"
         (agent-shell-permission-transient--field-line
          "Cmd"
          (agent-shell-permission-transient--summary))))
    "No pending permissions"))

(defun agent-shell-permission-transient--option (request kind)
  "Return REQUEST's permission option matching KIND."
  (seq-find
   (lambda (option) (equal (map-elt option :kind) kind))
   (map-elt (agent-shell-permission-transient--request-permission request)
            :options)))

(defun agent-shell-permission-transient--current-option-p (kind)
  "Return non-nil when the current request provides option KIND."
  (and agent-shell-permission-transient--current
       (agent-shell-permission-transient--option
        agent-shell-permission-transient--current kind)))

(defun agent-shell-permission-transient--request-pending-p (request)
  "Return non-nil when REQUEST remains in the queue."
  (memq request agent-shell-permission-transient--queue))

(defun agent-shell-permission-transient--respond (request kind)
  "Respond to REQUEST using the option matching KIND."
  (unless (agent-shell-permission-transient--request-pending-p request)
    (user-error "Permission is no longer pending"))
  (let* ((permission
          (agent-shell-permission-transient--request-permission request))
         (option (agent-shell-permission-transient--option request kind))
         (respond (map-elt permission :respond))
         (shell-buffer
          (agent-shell-permission-transient--request-shell-buffer request)))
    (unless option
      (user-error "Permission does not provide %s" kind))
    (unless (functionp respond)
      (user-error "Permission has no response callback"))
    (funcall respond (map-elt option :option-id))
    ;; The response event normally removes the request synchronously.  Keep
    ;; this idempotent fallback for synthetic responders and older versions.
    (agent-shell-permission-transient--remove request)
    (when (and (equal kind "reject_once")
               (buffer-live-p shell-buffer))
      (with-current-buffer shell-buffer
        (agent-shell-interrupt t)))
    (agent-shell-permission-transient--schedule-popup)
    t))

(defun agent-shell-permission-transient--respond-current (kind)
  "Respond to the current permission with option KIND."
  (unless agent-shell-permission-transient--current
    (user-error "No pending permission"))
  (agent-shell-permission-transient--respond
   agent-shell-permission-transient--current kind))

(defun agent-shell-permission-transient--allow-once ()
  "Allow the current permission once."
  (interactive)
  (agent-shell-permission-transient--respond-current "allow_once"))

(defun agent-shell-permission-transient--allow-always ()
  "Always allow the current permission."
  (interactive)
  (agent-shell-permission-transient--respond-current "allow_always"))

(defun agent-shell-permission-transient--reject ()
  "Reject the current permission and interrupt its agent shell."
  (interactive)
  (agent-shell-permission-transient--respond-current "reject_once"))

(defun agent-shell-permission-transient--has-diffs-p ()
  "Return non-nil when the current permission contains diffs."
  (map-elt (agent-shell-permission-transient--current-tool-call) :diffs))

(defun agent-shell-permission-transient--view-diff ()
  "View diffs for the current permission using agent-shell's diff UI."
  (interactive)
  (let* ((request agent-shell-permission-transient--current)
         (tool-call (agent-shell-permission-transient--current-tool-call))
         (diffs (map-elt tool-call :diffs))
         (shell-buffer
          (and request
               (agent-shell-permission-transient--request-shell-buffer request))))
    (unless (and request diffs)
      (user-error "Permission has no diff"))
    (unless (buffer-live-p shell-buffer)
      (user-error "Permission's agent-shell buffer is no longer live"))
    (with-current-buffer shell-buffer
      (agent-shell-diff
       :diffs diffs
       :title (agent-shell-permission-transient--summary)
       :on-accept
       (lambda ()
         (agent-shell-permission-transient--respond request "allow_once"))
       :on-reject
       (lambda ()
         (agent-shell-permission-transient--respond request "reject_once"))
       :on-exit
       (lambda ()
         (when (agent-shell-permission-transient--request-pending-p request)
           (agent-shell-permission-transient--schedule-popup)))))))

(defun agent-shell-permission-transient--cycle (offset)
  "Move OFFSET entries through the pending permission queue."
  (unless (> (agent-shell-permission-transient-pending-count) 1)
    (user-error "Only one permission is pending"))
  (let* ((count (agent-shell-permission-transient-pending-count))
         (index (or (seq-position agent-shell-permission-transient--queue
                                  agent-shell-permission-transient--current
                                  #'eq)
                    0)))
    (setq agent-shell-permission-transient--current
          (nth (mod (+ index offset) count)
               agent-shell-permission-transient--queue))))

(defun agent-shell-permission-transient--previous ()
  "Show the previous pending permission."
  (interactive)
  (agent-shell-permission-transient--cycle -1))

(defun agent-shell-permission-transient--next ()
  "Show the next pending permission."
  (interactive)
  (agent-shell-permission-transient--cycle 1))

(defun agent-shell-permission-transient--multiple-p ()
  "Return non-nil when multiple permissions are pending."
  (> (agent-shell-permission-transient-pending-count) 1))

(defun agent-shell-permission-transient--defer ()
  "Close the permission menu without responding."
  (interactive))

(defun agent-shell-permission-transient--display-buffer (buffer alist)
  "Display transient BUFFER below its agent shell window using ALIST."
  (when-let* ((window (agent-shell-permission-transient--target-window)))
    (display-buffer-in-direction
     buffer
     (append `((direction . below) (window . ,window)) alist))))

(transient-define-prefix agent-shell-permission-transient--menu ()
  "Respond to queued `agent-shell' permission requests."
  :display-action
  '(agent-shell-permission-transient--display-buffer
    (dedicated . t)
    (inhibit-same-window . t))
  [(:info (lambda () (agent-shell-permission-transient--heading))
          :format "%d")]
  [["Respond"
    ("y" "Allow once" agent-shell-permission-transient--allow-once
     :if (lambda ()
           (agent-shell-permission-transient--current-option-p "allow_once")))
    ("!" "Always allow" agent-shell-permission-transient--allow-always
     :if (lambda ()
           (agent-shell-permission-transient--current-option-p "allow_always")))
    ("n" "Reject" agent-shell-permission-transient--reject
     :if (lambda ()
           (agent-shell-permission-transient--current-option-p "reject_once")))]
   ["Queue"
    ("p" "Previous" agent-shell-permission-transient--previous
     :transient t :if agent-shell-permission-transient--multiple-p)
    ("N" "Next" agent-shell-permission-transient--next
     :transient t :if agent-shell-permission-transient--multiple-p)
    ("q" "Defer" agent-shell-permission-transient--defer)]
   ["Inspect"
    ("v" "View diff" agent-shell-permission-transient--view-diff
     :if agent-shell-permission-transient--has-diffs-p)]])

;;;###autoload
(defun agent-shell-permission-transient-menu ()
  "Open the menu for pending `agent-shell' permissions."
  (interactive)
  (unless agent-shell-permission-transient--queue
    (user-error "No pending agent-shell permissions"))
  (unless (memq agent-shell-permission-transient--current
                agent-shell-permission-transient--queue)
    (setq agent-shell-permission-transient--current
          (car agent-shell-permission-transient--queue)))
  (call-interactively #'agent-shell-permission-transient--menu))

(defun agent-shell-permission-transient--unsubscribe-all ()
  "Remove event subscriptions from all live `agent-shell' buffers."
  (maphash
   (lambda (shell-buffer tokens)
     (when (buffer-live-p shell-buffer)
       (with-current-buffer shell-buffer
         (dolist (token tokens)
           (ignore-errors
             (agent-shell-unsubscribe :subscription token)))))
     (agent-shell-permission-transient--delete-reminder shell-buffer))
   agent-shell-permission-transient--subscriptions)
  (clrhash agent-shell-permission-transient--subscriptions))

(defun agent-shell-permission-transient--enable ()
  "Install the transient permission responder."
  (unless (eq agent-shell-permission-responder-function
              #'agent-shell-permission-transient--responder)
    (setq agent-shell-permission-transient--previous-responder
          agent-shell-permission-responder-function
          agent-shell-permission-responder-function
          #'agent-shell-permission-transient--responder)))

(defun agent-shell-permission-transient--disable ()
  "Restore the permission responder active before this package."
  (when agent-shell-permission-transient--queue
    (setq agent-shell-permission-transient-mode t)
    (user-error "Resolve pending permissions before disabling the mode"))
  (agent-shell-permission-transient--cancel-popup-timer)
  (agent-shell-permission-transient--unsubscribe-all)
  (when (eq agent-shell-permission-responder-function
            #'agent-shell-permission-transient--responder)
    (setq agent-shell-permission-responder-function
          agent-shell-permission-transient--previous-responder))
  (setq agent-shell-permission-transient--previous-responder nil))

;;;###autoload
(define-minor-mode agent-shell-permission-transient-mode
  "Use a Transient menu for `agent-shell' permission requests."
  :global t
  :group 'agent-shell-permission-transient
  :lighter (:eval (agent-shell-permission-transient--lighter))
  (if agent-shell-permission-transient-mode
      (agent-shell-permission-transient--enable)
    (agent-shell-permission-transient--disable)))

(provide 'agent-shell-permission-transient)
;;; agent-shell-permission-transient.el ends here
