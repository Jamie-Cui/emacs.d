;;; agent-shell-permission-transient-test.el --- Tests for permission transient -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused queue and response tests for agent-shell-permission-transient.

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (expand-file-name "../site-lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))

(require 'agent-shell-permission-transient)

(defun agent-shell-permission-transient-test--permission
    (request-id &optional respond diffs)
  "Return a synthetic permission with REQUEST-ID, RESPOND, and DIFFS."
  `((:tool-call
     . ((:title . "Run tests")
        (:kind . "execute")
        (:permission-request-id . ,request-id)
        (:raw-input . ((command . ["make" "test"])))
        ,@(when diffs `((:diffs . ,diffs)))))
    (:options
     . (((:kind . "allow_once") (:option-id . "allow"))
        ((:kind . "allow_always") (:option-id . "always"))
        ((:kind . "reject_once") (:option-id . "reject"))))
    (:respond . ,(or respond #'ignore))))

(defmacro agent-shell-permission-transient-test--isolated (&rest body)
  "Run BODY with isolated package state."
  (declare (indent 0) (debug t))
  `(let ((agent-shell-permission-transient--queue nil)
         (agent-shell-permission-transient--current nil)
         (agent-shell-permission-transient--popup-timer nil)
         (agent-shell-permission-transient--subscriptions
          (make-hash-table :test #'eq)))
     ,@body
     (when (timerp agent-shell-permission-transient--popup-timer)
       (cancel-timer agent-shell-permission-transient--popup-timer))))

(ert-deftest agent-shell-permission-transient-enqueues-in-arrival-order ()
  (agent-shell-permission-transient-test--isolated
    (let ((shell (generate-new-buffer " *permission-shell*")))
      (unwind-protect
          (cl-letf (((symbol-function
                      'agent-shell-permission-transient--ensure-subscriptions)
                     #'ignore)
                    ((symbol-function
                      'agent-shell-permission-transient--schedule-popup)
                     #'ignore))
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission 1) shell)
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission 2) shell)
            (should
             (equal
              (mapcar #'agent-shell-permission-transient--request-id
                      agent-shell-permission-transient--queue)
              '(1 2)))
            (should
             (eq agent-shell-permission-transient--current
                 (car agent-shell-permission-transient--queue))))
        (kill-buffer shell)))))

(ert-deftest agent-shell-permission-transient-reminder-does-not-edit-buffer ()
  (agent-shell-permission-transient-test--isolated
    (let ((shell (generate-new-buffer " *permission-shell*")))
      (unwind-protect
          (cl-letf (((symbol-function
                      'agent-shell-permission-transient--ensure-subscriptions)
                     #'ignore)
                    ((symbol-function
                      'agent-shell-permission-transient--schedule-popup)
                     #'ignore))
            (with-current-buffer shell
              (insert "Agent output"))
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission 1) shell)
            (with-current-buffer shell
              (let ((overlay
                     agent-shell-permission-transient--reminder-overlay))
                (should (overlayp overlay))
                (should (= (overlay-start overlay) (point-max)))
                (should (= (overlay-end overlay) (point-max)))
                (should (equal (buffer-string) "Agent output"))
                (should
                 (string-match-p
                  "Permission pending"
                  (overlay-get overlay 'after-string)))
                (should
                 (string-match-p
                  "C-c C-p"
                  (overlay-get overlay 'after-string))))))
        (kill-buffer shell)))))

(ert-deftest agent-shell-permission-transient-reminder-tracks-count ()
  (agent-shell-permission-transient-test--isolated
    (let ((shell (generate-new-buffer " *permission-shell*"))
          first-overlay)
      (unwind-protect
          (cl-letf (((symbol-function
                      'agent-shell-permission-transient--ensure-subscriptions)
                     #'ignore)
                    ((symbol-function
                      'agent-shell-permission-transient--schedule-popup)
                     #'ignore))
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission 1) shell)
            (with-current-buffer shell
              (setq first-overlay
                    agent-shell-permission-transient--reminder-overlay))
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission 2) shell)
            (with-current-buffer shell
              (should
               (eq first-overlay
                   agent-shell-permission-transient--reminder-overlay))
              (should
               (string-match-p
                "2 permissions pending"
                (overlay-get first-overlay 'after-string))))
            (agent-shell-permission-transient--remove
             (car agent-shell-permission-transient--queue))
            (with-current-buffer shell
              (should
               (string-match-p
                "Permission pending"
                (overlay-get first-overlay 'after-string))))
            (agent-shell-permission-transient--remove
             (car agent-shell-permission-transient--queue))
            (with-current-buffer shell
              (should-not agent-shell-permission-transient--reminder-overlay))
            (should-not (overlay-buffer first-overlay)))
        (kill-buffer shell)))))

(ert-deftest agent-shell-permission-transient-reminders-are-buffer-local ()
  (agent-shell-permission-transient-test--isolated
    (let ((first-shell (generate-new-buffer " *permission-shell-1*"))
          (second-shell (generate-new-buffer " *permission-shell-2*")))
      (unwind-protect
          (cl-letf (((symbol-function
                      'agent-shell-permission-transient--ensure-subscriptions)
                     #'ignore)
                    ((symbol-function
                      'agent-shell-permission-transient--schedule-popup)
                     #'ignore))
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission 1)
             first-shell)
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission 2)
             second-shell)
            (let ((first-overlay
                   (buffer-local-value
                    'agent-shell-permission-transient--reminder-overlay
                    first-shell))
                  (second-overlay
                   (buffer-local-value
                    'agent-shell-permission-transient--reminder-overlay
                    second-shell)))
              (should (overlayp first-overlay))
              (should (overlayp second-overlay))
              (should-not (eq first-overlay second-overlay))
              (agent-shell-permission-transient--remove-shell first-shell)
              (should-not (overlay-buffer first-overlay))
              (should (overlay-buffer second-overlay))))
        (kill-buffer first-shell)
        (kill-buffer second-shell)))))

(ert-deftest agent-shell-permission-transient-reminder-follows-output ()
  (agent-shell-permission-transient-test--isolated
    (let ((shell (generate-new-buffer " *permission-shell*")))
      (unwind-protect
          (cl-letf (((symbol-function
                      'agent-shell-permission-transient--ensure-subscriptions)
                     #'ignore)
                    ((symbol-function
                      'agent-shell-permission-transient--schedule-popup)
                     #'ignore))
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission 1) shell)
            (with-current-buffer shell
              (goto-char (point-max))
              (insert "Later output")
              (should
               (= (overlay-start
                   agent-shell-permission-transient--reminder-overlay)
                  (point-max)))
              (should
               (= (overlay-end
                   agent-shell-permission-transient--reminder-overlay)
                  (point-max)))))
        (kill-buffer shell)))))

(ert-deftest agent-shell-permission-transient-deduplicates-request-id ()
  (agent-shell-permission-transient-test--isolated
    (let ((shell (generate-new-buffer " *permission-shell*")))
      (unwind-protect
          (cl-letf (((symbol-function
                      'agent-shell-permission-transient--ensure-subscriptions)
                     #'ignore)
                    ((symbol-function
                      'agent-shell-permission-transient--schedule-popup)
                     #'ignore))
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission 1) shell)
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission 1) shell)
            (should (= (length agent-shell-permission-transient--queue) 1)))
        (kill-buffer shell)))))

(ert-deftest agent-shell-permission-transient-responds-with-selected-option ()
  (agent-shell-permission-transient-test--isolated
    (let ((shell (generate-new-buffer " *permission-shell*"))
          selected)
      (unwind-protect
          (cl-letf (((symbol-function
                      'agent-shell-permission-transient--ensure-subscriptions)
                     #'ignore)
                    ((symbol-function
                      'agent-shell-permission-transient--schedule-popup)
                     #'ignore))
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission
              1 (lambda (option-id) (setq selected option-id)))
             shell)
            (agent-shell-permission-transient--respond-current "allow_always")
            (should (equal selected "always"))
            (should-not agent-shell-permission-transient--queue))
        (kill-buffer shell)))))

(ert-deftest agent-shell-permission-transient-rejects-and-interrupts-shell ()
  (agent-shell-permission-transient-test--isolated
    (let ((shell (generate-new-buffer " *permission-shell*"))
          selected
          interrupted)
      (unwind-protect
          (cl-letf (((symbol-function
                      'agent-shell-permission-transient--ensure-subscriptions)
                     #'ignore)
                    ((symbol-function
                      'agent-shell-permission-transient--schedule-popup)
                     #'ignore)
                    ((symbol-function 'agent-shell-interrupt)
                     (lambda (&optional force) (setq interrupted force))))
            (agent-shell-permission-transient--enqueue
             (agent-shell-permission-transient-test--permission
              1 (lambda (option-id) (setq selected option-id)))
             shell)
            (agent-shell-permission-transient--respond-current "reject_once")
            (should (equal selected "reject"))
            (should interrupted))
        (kill-buffer shell)))))

(ert-deftest agent-shell-permission-transient-delegates-first ()
  (agent-shell-permission-transient-test--isolated
    (let ((agent-shell-permission-transient--previous-responder
           (lambda (_permission) :handled)))
      (cl-letf (((symbol-function
                  'agent-shell-permission-transient--shell-buffer)
                 (lambda () (ert-fail "fallback should not run"))))
        (should
         (eq (agent-shell-permission-transient--responder
              (agent-shell-permission-transient-test--permission 1))
             :handled))))))

(ert-deftest agent-shell-permission-transient-defer-keeps-request-pending ()
  (agent-shell-permission-transient-test--isolated
    (let* ((permission
            (agent-shell-permission-transient-test--permission 1))
           (request
            (agent-shell-permission-transient--request-create
             :id 1 :permission permission :shell-buffer (current-buffer))))
      (setq agent-shell-permission-transient--queue (list request)
            agent-shell-permission-transient--current request)
      (agent-shell-permission-transient--defer)
      (should (eq request agent-shell-permission-transient--current))
      (should (equal agent-shell-permission-transient--queue (list request))))))

(ert-deftest agent-shell-permission-transient-diff-visibility-follows-payload ()
  (agent-shell-permission-transient-test--isolated
    (let* ((request
            (agent-shell-permission-transient--request-create
             :id 1
             :permission
             (agent-shell-permission-transient-test--permission 1)
             :shell-buffer (current-buffer))))
      (setq agent-shell-permission-transient--queue (list request)
            agent-shell-permission-transient--current request)
      (should-not (agent-shell-permission-transient--has-diffs-p))
      (setf (agent-shell-permission-transient--request-permission request)
            (agent-shell-permission-transient-test--permission
             1 nil '(((:old . "a") (:new . "b") (:file . "x")))))
      (should (agent-shell-permission-transient--has-diffs-p)))))

(ert-deftest agent-shell-permission-transient-formats-command-summary ()
  (agent-shell-permission-transient-test--isolated
    (let* ((request
            (agent-shell-permission-transient--request-create
             :id 1
             :permission
             (agent-shell-permission-transient-test--permission 1)
             :shell-buffer (current-buffer))))
      (setq agent-shell-permission-transient--queue (list request)
            agent-shell-permission-transient--current request)
      (should (equal (agent-shell-permission-transient--summary)
                     "make test")))))

(ert-deftest agent-shell-permission-transient-labels-dir-and-command ()
  (agent-shell-permission-transient-test--isolated
    (let* ((request
            (agent-shell-permission-transient--request-create
             :id 1
             :permission
             (agent-shell-permission-transient-test--permission 1)
             :shell-buffer (current-buffer))))
      (setq agent-shell-permission-transient--queue (list request)
            agent-shell-permission-transient--current request)
      (cl-letf (((symbol-function
                  'agent-shell-permission-transient--agent-name)
                 (lambda (_request) "Codex"))
                ((symbol-function
                  'agent-shell-permission-transient--working-directory)
                 (lambda (_request) "~/opt/emacs.d")))
        (let* ((heading (agent-shell-permission-transient--heading))
               (command-position (string-match "Cmd: " heading)))
          (should
           (equal
            (substring-no-properties heading)
            (concat "Permission 1/1 · Codex · execute\n"
                    "Dir: ~/opt/emacs.d\n"
                    "Cmd: make test")))
          (should (eq (get-text-property command-position 'face heading)
                      'transient-heading))
          (should
           (eq (get-text-property (+ command-position (length "Cmd: "))
                                  'face heading)
               'transient-value)))))))

(ert-deftest agent-shell-permission-transient-renders-heading-block ()
  (agent-shell-permission-transient-test--isolated
    (let* ((shell-buffer (current-buffer))
           (request
            (agent-shell-permission-transient--request-create
             :id 1
             :permission
             (agent-shell-permission-transient-test--permission 1)
             :shell-buffer shell-buffer)))
      (setq agent-shell-permission-transient--queue (list request)
            agent-shell-permission-transient--current request)
      (cl-letf (((symbol-function
                  'agent-shell-permission-transient--agent-name)
                 (lambda (_request) "Codex"))
                ((symbol-function
                  'agent-shell-permission-transient--working-directory)
                 (lambda (_request) "~/opt/emacs.d")))
        (let ((transient--prefix nil)
              (transient--layout nil)
              (transient--suffixes nil)
              (transient--editp nil))
          (transient--init-objects
           'agent-shell-permission-transient--menu)
          (with-temp-buffer
            (let ((transient--shadowed-buffer shell-buffer))
              (transient--insert-groups)
              (should
               (string-match-p
                (regexp-quote
                 (concat "Permission 1/1 · Codex · execute\n"
                         "Dir: ~/opt/emacs.d\n"
                         "Cmd: make test"))
                (buffer-string))))))))))

(ert-deftest agent-shell-permission-transient-schedules-after-active-menu ()
  (agent-shell-permission-transient-test--isolated
    (let ((agent-shell-permission-transient--queue '(pending)))
      (cl-letf (((symbol-function 'transient-active-prefix)
                 (lambda (&optional _prefixes) :active)))
        (agent-shell-permission-transient--schedule-popup 60)
        (should (timerp agent-shell-permission-transient--popup-timer))))))

(ert-deftest agent-shell-permission-transient-requires-visible-shell-window ()
  (agent-shell-permission-transient-test--isolated
    (let* ((agent-shell-permission-transient-mode t)
           (noninteractive nil)
           (request
            (agent-shell-permission-transient--request-create
             :id 1
             :permission
             (agent-shell-permission-transient-test--permission 1)
             :shell-buffer (current-buffer)))
           target-window)
      (setq agent-shell-permission-transient--queue (list request)
            agent-shell-permission-transient--current request)
      (cl-letf (((symbol-function 'active-minibuffer-window) #'ignore)
                ((symbol-function 'transient-active-prefix) #'ignore)
                ((symbol-function
                  'agent-shell-permission-transient--target-window)
                 (lambda () target-window)))
        (should-not (agent-shell-permission-transient--popup-safe-p))
        (setq target-window (selected-window))
        (should (agent-shell-permission-transient--popup-safe-p))))))

(ert-deftest agent-shell-permission-transient-displays-below-shell-window ()
  (agent-shell-permission-transient-test--isolated
    (let ((target-window (selected-window))
          displayed-buffer
          display-alist)
      (cl-letf (((symbol-function
                  'agent-shell-permission-transient--target-window)
                 (lambda () target-window))
                ((symbol-function 'display-buffer-in-direction)
                 (lambda (buffer alist)
                   (setq displayed-buffer buffer
                         display-alist alist)
                   :menu-window)))
        (should
         (eq (agent-shell-permission-transient--display-buffer
              (current-buffer) '((dedicated . t)))
             :menu-window))
        (should (eq displayed-buffer (current-buffer)))
        (should (eq (alist-get 'window display-alist) target-window))
        (should (eq (alist-get 'direction display-alist) 'below))))))

(ert-deftest agent-shell-permission-transient-omits-open-shell-suffix ()
  (should-error
   (transient-get-suffix 'agent-shell-permission-transient--menu "o")))

(provide 'agent-shell-permission-transient-test)
;;; agent-shell-permission-transient-test.el ends here
