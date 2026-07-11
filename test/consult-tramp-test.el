;;; consult-tramp-test.el --- Tests for consult-tramp -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for the local consult-tramp package.

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (expand-file-name "../site-lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))

(require 'consult-tramp)

(ert-deftest consult-tramp-locations-composes-sources-in-order ()
  (let ((consult-tramp-sources
         (list (lambda () '("/ssh:alpha:" "/ssh:alpha:/srv"))
               (lambda () '("/ssh:alpha:" "/ssh:beta:")))))
    (should
     (equal (consult-tramp-locations)
            '("/ssh:alpha:" "/ssh:alpha:/srv" "/ssh:beta:")))))

(ert-deftest consult-tramp-source-history-keeps-location-and-prefix ()
  (let ((file-name-history
         '("/ssh:alpha:/var/log/messages" "/tmp/local")))
    (should
     (equal (consult-tramp-source-history)
            '("/ssh:alpha:/var/log/messages" "/ssh:alpha:")))))

(ert-deftest consult-tramp-source-methods-reuses-completion-and-filters-globs ()
  (let ((consult-tramp-sources '(consult-tramp-source-methods))
        (tramp-methods '(("docker") ("ssh")))
        (calls 0))
    (cl-letf (((symbol-function 'tramp-get-completion-function)
               (lambda (_method)
                 '((consult-tramp-test--shared-completion "shared"))))
              ((symbol-function 'consult-tramp-test--shared-completion)
               (lambda (_argument)
                 (cl-incf calls)
                 '((nil "alpha") (nil "prod-*") (nil "host?")))))
      (should
       (equal (consult-tramp-locations)
              '("/docker:alpha:" "/ssh:alpha:")))
      (should (= calls 1)))))

(ert-deftest consult-tramp-sort-locations-prefers-default-method-and-source-order ()
  (let ((tramp-default-method "ssh"))
    (should
     (equal
      (consult-tramp--sort-locations
       '("/docker:box:" "/ssh:recent:/srv" "/ssh:discovered:" "/adb:phone:"))
      '("/ssh:recent:/srv" "/ssh:discovered:" "/adb:phone:" "/docker:box:")))))

(ert-deftest consult-tramp-locations-keeps-working-after-source-failure ()
  (let ((consult-tramp-sources
         (list (lambda () (error "broken source"))
               (lambda () '("/ssh:alpha:")))))
    (should (equal (consult-tramp-locations) '("/ssh:alpha:")))))

(ert-deftest consult-tramp-allows-an-arbitrary-valid-location ()
  (let ((consult-tramp-sources nil)
        selected
        options)
    (cl-letf (((symbol-function 'consult--read)
               (lambda (_candidates &rest args)
                 (setq options args)
                 "/ssh:alpha:/srv"))
              ((symbol-function 'consult-tramp--open-location)
               (lambda (location) (setq selected location))))
      (consult-tramp)
      (should (equal selected "/ssh:alpha:/srv"))
      (should-not (plist-get options :require-match))
      (should (eq (plist-get options :category) 'file))
      (should-not (plist-get options :preview-key)))))

(ert-deftest consult-tramp-rejects-a-local-location ()
  (let ((consult-tramp-sources nil))
    (cl-letf (((symbol-function 'consult--read)
               (lambda (&rest _) "/tmp/local")))
      (should-error (consult-tramp) :type 'user-error))))

(ert-deftest consult-tramp-open-location-opens-a-directory-in-dired ()
  (let (opened)
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (_location &optional _identification _connected)
                 "/ssh:alpha:"))
              ((symbol-function 'file-directory-p) (lambda (_location) t))
              ((symbol-function 'dired)
               (lambda (location) (setq opened location) 'dired-result)))
      (should (eq (consult-tramp--open-location "/ssh:alpha:/srv/")
                  'dired-result))
      (should (equal opened "/ssh:alpha:/srv/")))))

(ert-deftest consult-tramp-open-location-shows-a-file-in-its-parent ()
  (let ((buffer (generate-new-buffer " *consult-tramp-test-dired*"))
        opened
        target)
    (unwind-protect
        (cl-letf (((symbol-function 'file-remote-p)
                   (lambda (_location &optional _identification _connected)
                     "/ssh:alpha:"))
                  ((symbol-function 'file-directory-p) (lambda (_location) nil))
                  ((symbol-function 'file-exists-p) (lambda (_location) t))
                  ((symbol-function 'dired)
                   (lambda (location) (setq opened location) buffer))
                  ((symbol-function 'dired-goto-file)
                   (lambda (location) (setq target location) t)))
          (should
           (eq (consult-tramp--open-location "/ssh:alpha:/srv/app.log")
               buffer))
          (should (equal opened "/ssh:alpha:/srv/"))
          (should (equal target "/ssh:alpha:/srv/app.log")))
      (kill-buffer buffer))))

(ert-deftest consult-tramp-open-location-falls-back-to-remote-home ()
  (let (opened)
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (_location &optional _identification _connected)
                 "/ssh:alpha:"))
              ((symbol-function 'file-directory-p) (lambda (_location) nil))
              ((symbol-function 'file-exists-p) (lambda (_location) nil))
              ((symbol-function 'dired)
               (lambda (location) (setq opened location) 'home-buffer)))
      (should
       (eq (consult-tramp--open-location "/ssh:alpha:/missing")
           'home-buffer))
      (should (equal opened "/ssh:alpha:")))))

(ert-deftest consult-tramp-open-location-falls-back-after-dired-error ()
  (let (opened)
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (_location &optional _identification _connected)
                 "/ssh:alpha:"))
              ((symbol-function 'file-directory-p) (lambda (_location) t))
              ((symbol-function 'dired)
               (lambda (location)
                 (if (equal location "/ssh:alpha:/private/")
                     (error "Permission denied")
                   (setq opened location)
                   'home-buffer))))
      (should
       (eq (consult-tramp--open-location "/ssh:alpha:/private/")
           'home-buffer))
      (should (equal opened "/ssh:alpha:")))))

(provide 'consult-tramp-test)
;;; consult-tramp-test.el ends here
