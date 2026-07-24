;;; init-config-xenops-test.el --- Tests for Xenops configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused regression tests for Xenops scale refresh and async lifecycle state.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'init-config-xenops)

(ert-deftest init-config-xenops-normalizes-corrupt-idle-semaphore ()
  (let ((xenops-math-latex-max-tasks-in-flight 2))
    (with-temp-buffer
      (setq-local xenops-math-latex-tasks-semaphore (aio-sem 2))
      (setf (aio-sem-value xenops-math-latex-tasks-semaphore) 4)
      (should (fn/xenops-math-render-queue-idle-p))
      (fn/xenops-math-normalize-idle-semaphore)
      (should (= (aio-sem-value xenops-math-latex-tasks-semaphore) 2))
      (should (null (car (aio-sem-queue
                          xenops-math-latex-tasks-semaphore)))))))

(ert-deftest init-config-xenops-does-not-refresh-a-busy-queue ()
  (let ((xenops-math-latex-max-tasks-in-flight 2)
        scheduled
        rendered)
    (with-temp-buffer
      (setq-local xenops-mode t)
      (setq-local fn/xenops-math-refresh-pending t)
      (setq-local xenops-math-latex-tasks-semaphore (aio-sem 2))
      (aio-sem-wait xenops-math-latex-tasks-semaphore)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _)
                   (setq scheduled t)
                   'test-timer))
                ((symbol-function 'fn/xenops-math-render-current-scale)
                 (lambda () (setq rendered t))))
        (fn/xenops-math-finish-scheduled-refresh
         (current-buffer)
         (fn/xenops-math-buffer-generation)))
      (should scheduled)
      (should-not rendered)
      (should fn/xenops-math-refresh-pending))))

(ert-deftest init-config-xenops-rejects-an-old-buffer-generation ()
  (with-temp-buffer
    (setq-local xenops-mode t)
    (let ((generation (fn/xenops-math-buffer-generation)))
      (should (fn/xenops-math-current-generation-p
               (current-buffer) generation))
      (fn/xenops-math-advance-buffer-generation)
      (should-not (fn/xenops-math-current-generation-p
                   (current-buffer) generation)))))

(ert-deftest init-config-xenops-posts-to-the-acquired-semaphore ()
  (let ((old-semaphore (aio-sem 1))
        (new-semaphore (aio-sem 1))
        displayed)
    (with-temp-buffer
      (setq-local xenops-mode t)
      (setq-local xenops-math-latex-tasks-semaphore old-semaphore)
      (cl-letf (((symbol-function 'xenops-math-set-marker-on-element)
                 #'ignore)
                ((symbol-function 'xenops-math-latex-process-get)
                 (lambda (key)
                   (pcase key
                     (:image-input-type "xdv")
                     (:image-output-type "svg")
                     (:image-output-ppi nil))))
                ((symbol-function 'xenops-math-latex-make-commands)
                 (lambda (&rest _) nil))
                ((symbol-function 'xenops-math-latex-make-latex-document)
                 (lambda (&rest _) ""))
                ((symbol-function 'copy-file) #'ignore)
                ((symbol-function 'xenops-math-parse-element-at)
                 (lambda (&rest _) '(:begin 1 :end 1)))
                ((symbol-function 'xenops-math-deactivate-marker-on-element)
                 #'ignore))
        (let ((promise
               (fn/xenops-math-latex-create-image-a
                '(:begin-marker marker)
                "x" '("0" "1") "/tmp/xenops-test.svg"
                (lambda (&rest _) (setq displayed t)))))
          ;; Simulate a major-mode restart installing a new buffer-local
          ;; semaphore while the old generation is still completing.
          (setq-local xenops-math-latex-tasks-semaphore new-semaphore)
          (aio-wait-for promise)))
      (should displayed)
      (should (= (aio-sem-value old-semaphore) 1))
      (should (= (aio-sem-value new-semaphore) 1)))))

(ert-deftest init-config-xenops-installs-async-lifecycle-override ()
  (should
   (advice-member-p #'fn/xenops-math-latex-create-image-a
                    'xenops-math-latex-create-image)))

(provide 'init-config-xenops-test)
;;; init-config-xenops-test.el ends here
