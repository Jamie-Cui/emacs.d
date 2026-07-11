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

(ert-deftest consult-tramp-source-methods-honors-configured-methods ()
  (let ((consult-tramp-methods '(ssh))
        (consult-tramp-sources '(consult-tramp-source-methods))
        (tramp-methods '(("docker") ("ssh")))
        called)
    (cl-letf (((symbol-function 'tramp-get-completion-function)
               (lambda (method)
                 (push method called)
                 '((consult-tramp-test--configured-completion "config"))))
              ((symbol-function 'consult-tramp-test--configured-completion)
               (lambda (_argument) '((nil "alpha")))))
      (should (equal (consult-tramp-locations) '("/ssh:alpha:")))
      (should (equal called '("ssh"))))))

(ert-deftest consult-tramp-source-methods-replaces-sudo-passwd-users ()
  (let ((consult-tramp-methods '(sudo))
        (consult-tramp-sources '(consult-tramp-source-methods))
        (tramp-methods '(("sudo"))))
    (cl-letf (((symbol-function 'tramp-get-completion-function)
               (lambda (_method) '((tramp-parse-passwd "/etc/passwd"))))
              ((symbol-function 'tramp-parse-passwd)
               (lambda (_file)
                 '(("daemon" "localhost") ("_www" "localhost"))))
              ((symbol-function 'consult-tramp--local-login-users)
               (lambda () '("root" "jamie"))))
      (should
       (equal (consult-tramp-locations)
              '("/sudo:root@localhost:" "/sudo:jamie@localhost:"))))))

(ert-deftest consult-tramp-local-login-users-filters-macos-accounts ()
  (let ((system-type 'darwin)
        (consult-tramp-extra-privileged-users nil))
    (cl-letf (((symbol-function 'user-login-name) (lambda () "jamie"))
              ((symbol-function 'consult-tramp--login-shells)
               (lambda () '("/bin/sh" "/bin/zsh")))
              ((symbol-function 'consult-tramp--local-user-records)
               (lambda ()
                 '(("root" 0 "/bin/sh")
                   ("alice" 501 "/bin/zsh")
                   ("_service" 502 "/bin/zsh")
                   ("daemon" 1 "/usr/bin/false")))))
      (should
       (equal (consult-tramp--local-login-users)
              '("root" "jamie" "alice"))))))

(ert-deftest consult-tramp-local-login-users-filters-linux-accounts ()
  (let ((system-type 'gnu/linux)
        (consult-tramp-extra-privileged-users nil))
    (cl-letf (((symbol-function 'user-login-name) (lambda () "admin"))
              ((symbol-function 'consult-tramp--linux-uid-min)
               (lambda () 1000))
              ((symbol-function 'consult-tramp--login-shells)
               (lambda () '("/bin/bash")))
              ((symbol-function 'consult-tramp--local-user-records)
               (lambda ()
                 '(("root" 0 "/bin/bash")
                   ("admin" 500 "/bin/bash")
                   ("alice" 1000 "/bin/bash")
                   ("postgres" 999 "/bin/bash")
                   ("service" 1001 "/usr/sbin/nologin")))))
      (should
       (equal (consult-tramp--local-login-users)
              '("root" "admin" "alice"))))))

(ert-deftest consult-tramp-parses-native-user-databases ()
  (should
   (equal
    (consult-tramp--parse-passwd-users
     "alice:x:1000:1000:Alice:/home/alice:/bin/bash\n")
    '(("alice" 1000 "/bin/bash"))))
  (should
   (equal
    (consult-tramp--parse-dscache-users
     (concat "name: alice\nuid: 501\ngid: 20\ndir: /Users/alice\n"
             "shell: /bin/zsh\n\n"
             "name: _service\nuid: 250\nshell: /usr/bin/false\n"))
    '(("alice" 501 "/bin/zsh") ("_service" 250 "/usr/bin/false")))))

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
