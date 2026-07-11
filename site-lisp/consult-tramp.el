;;; consult-tramp.el --- Consult TRAMP locations -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1") (consult "2.0"))
;; Keywords: files, matching, tools

;;; Commentary:

;; `consult-tramp' collects active, historical, and discoverable TRAMP
;; locations, lets Consult select or enter one, and opens it in Dired.
;; Candidate sources are ordinary functions returning lists of strings, so
;; users can extend the command without replacing its selection UI.

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'dired)
(require 'seq)
(require 'subr-x)
(require 'tramp)
(require 'tramp-cache)

(defgroup consult-tramp nil
  "Select TRAMP locations with Consult."
  :group 'consult
  :group 'tramp)

(defcustom consult-tramp-sources
  '(consult-tramp-source-active
    consult-tramp-source-history
    consult-tramp-source-methods)
  "Functions used to collect candidates for `consult-tramp'.
Each function is called without arguments and must return a list of
TRAMP location strings.  Sources are evaluated in list order; the first
occurrence of an identical location wins.  An error in one source does
not prevent later sources from contributing candidates."
  :type '(repeat function)
  :group 'consult-tramp)

(defvar consult-tramp--completion-cache nil
  "Invocation-local cache of TRAMP completion function results.")

(defvar consult-tramp--diagnostics nil
  "Diagnostics collected during the current candidate collection.")

(defconst consult-tramp--cache-miss (make-symbol "consult-tramp-cache-miss"))

(defun consult-tramp--record-diagnostic (context problem)
  "Record a candidate collection PROBLEM associated with CONTEXT."
  (push (format "%s: %s"
                context
                (if (stringp problem)
                    problem
                  (error-message-string problem)))
        consult-tramp--diagnostics))

(defun consult-tramp--report-diagnostics ()
  "Report candidate collection diagnostics in a compact message."
  (when consult-tramp--diagnostics
    (let* ((diagnostics (delete-dups (nreverse consult-tramp--diagnostics)))
           (shown (seq-take diagnostics 3))
           (remaining (- (length diagnostics) (length shown))))
      (message "consult-tramp: skipped %d item%s: %s%s"
               (length diagnostics)
               (if (= (length diagnostics) 1) "" "s")
               (string-join shown "; ")
               (if (> remaining 0)
                   (format "; and %d more" remaining)
                 "")))))

(defun consult-tramp--wildcard-host-p (host)
  "Return non-nil when HOST is a pattern rather than a concrete endpoint."
  (and (stringp host)
       (or (string-prefix-p "!" host)
           (string-match-p (regexp-opt '("*" "?" "[") t) host))))

(defun consult-tramp--location-vector (location)
  "Return LOCATION's parsed TRAMP vector when it is a concrete location."
  (when (and (stringp location) (tramp-tramp-file-p location))
    (condition-case nil
        (let* ((vector (tramp-dissect-file-name location 'noexpand))
               (method (tramp-file-name-method vector))
               (host (tramp-file-name-host vector)))
          (and (stringp method)
               (assoc method tramp-methods)
               (stringp host)
               (not (string-empty-p host))
               (not (consult-tramp--wildcard-host-p host))
               vector))
      (error nil))))

(defun consult-tramp--valid-location-p (location)
  "Return non-nil when LOCATION is a concrete TRAMP location."
  (and (consult-tramp--location-vector location) t))

(defun consult-tramp-source-active ()
  "Return currently active TRAMP connection prefixes."
  (let ((minibuffer-completing-file-name t)
        (tramp-show-ad-hoc-proxies t))
    (mapcar (lambda (vector)
              (tramp-make-tramp-file-name vector 'no-localname))
            (tramp-list-connections))))

(defun consult-tramp-source-history ()
  "Return remote locations and connection prefixes from file name history."
  (let (locations)
    (dolist (item (and (boundp 'file-name-history) file-name-history))
      (when (stringp item)
        (let* ((location (substring-no-properties item))
               (prefix (ignore-errors (file-remote-p location))))
          (when (stringp prefix)
            (push location locations)
            (unless (equal location prefix)
              (push (substring-no-properties prefix) locations))))))
    (nreverse locations)))

(defun consult-tramp--completion-result (specification)
  "Run a TRAMP completion SPECIFICATION once in the current invocation.
Return a cons whose car is non-nil on success and whose cdr is either the
completion rows or the captured error."
  (let ((cached (gethash specification consult-tramp--completion-cache
                         consult-tramp--cache-miss)))
    (if (not (eq cached consult-tramp--cache-miss))
        cached
      (setq cached
            (condition-case error-data
                (cons t (apply (car specification) (cdr specification)))
              (error
               (consult-tramp--record-diagnostic
                (format "completion %S" specification) error-data)
               (cons nil error-data))))
      (puthash (copy-tree specification) cached
               consult-tramp--completion-cache)
      cached)))

(defun consult-tramp--source-methods-1 ()
  "Collect concrete locations from registered TRAMP method completion hooks."
  (let (locations)
    (dolist (method (sort (delete-dups
                           (delq nil
                                 (mapcar (lambda (entry)
                                           (and (stringp (car-safe entry))
                                                (car entry)))
                                         tramp-methods)))
                          #'string<))
      (unless (equal method tramp-default-method-marker)
        (condition-case error-data
            (dolist (specification (tramp-get-completion-function method))
              (pcase (consult-tramp--completion-result specification)
                (`(t . ,rows)
                 (dolist (row rows)
                   (let ((user (nth 0 row))
                         (host (nth 1 row)))
                     (when (and (or (null user) (stringp user))
                                (stringp host)
                                (not (string-empty-p host))
                                (not (consult-tramp--wildcard-host-p host)))
                       (push (tramp-completion-make-tramp-file-name
                              method user host "")
                             locations)))))))
          (error
           (consult-tramp--record-diagnostic
            (format "method %s" method) error-data)))))
    (nreverse locations)))

(defun consult-tramp-source-methods ()
  "Return locations discovered by all registered TRAMP methods.
The method-specific completion functions registered with TRAMP are the
only discovery mechanism used.  Repeated completion specifications are
evaluated once per call to `consult-tramp-locations'."
  (if (hash-table-p consult-tramp--completion-cache)
      (consult-tramp--source-methods-1)
    (let ((consult-tramp--completion-cache (make-hash-table :test #'equal))
          (consult-tramp--diagnostics nil))
      (prog1 (consult-tramp--source-methods-1)
        (consult-tramp--report-diagnostics)))))

(defun consult-tramp--source-name (source)
  "Return a compact diagnostic name for SOURCE."
  (if (symbolp source) (symbol-name source) "anonymous source"))

(defun consult-tramp-locations ()
  "Return concrete TRAMP locations collected from `consult-tramp-sources'.
Sources and method completion hooks fail independently.  Results are
deduplicated without reordering, and no remote path is contacted to
validate its existence."
  (let ((consult-tramp--completion-cache (make-hash-table :test #'equal))
        (consult-tramp--diagnostics nil)
        (seen (make-hash-table :test #'equal))
        locations)
    (dolist (source consult-tramp-sources)
      (condition-case error-data
          (dolist (candidate (funcall source))
            (if (consult-tramp--valid-location-p candidate)
                (let ((location (substring-no-properties candidate)))
                  (unless (gethash location seen)
                    (puthash location t seen)
                    (push location locations)))
              (consult-tramp--record-diagnostic
               (consult-tramp--source-name source)
               (format "ignored invalid location %S" candidate))))
        (error
         (consult-tramp--record-diagnostic
          (consult-tramp--source-name source) error-data))))
    (consult-tramp--report-diagnostics)
    (nreverse locations)))

(defun consult-tramp--location-method (location)
  "Return the TRAMP method name used by LOCATION."
  (when-let* ((vector (consult-tramp--location-vector location)))
    (tramp-file-name-method vector)))

(defun consult-tramp--sort-locations (locations)
  "Group LOCATIONS by method while preserving source priority per method."
  (let ((default-method tramp-default-method))
    (cl-stable-sort
     (copy-sequence locations)
     (lambda (left right)
       (let ((left-method (consult-tramp--location-method left))
             (right-method (consult-tramp--location-method right)))
         (cond
          ((equal left-method right-method) nil)
          ((equal left-method default-method) t)
          ((equal right-method default-method) nil)
          (t (string< left-method right-method))))))))

(defun consult-tramp--group (candidate transform)
  "Group CANDIDATE by TRAMP method, leaving it unchanged for TRANSFORM."
  (if transform
      candidate
    (or (consult-tramp--location-method candidate) "Tramp")))

(defun consult-tramp--visit-location (location)
  "Open an existing remote LOCATION in Dired and return its buffer.
Directories are opened directly.  Files are shown in their parent Dired
buffer and selected.  Signal `file-missing' when LOCATION does not exist."
  (cond
   ((file-directory-p location)
    (dired location))
   ((file-exists-p location)
    (let* ((directory (file-name-directory location))
           (buffer (dired directory)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dired-goto-file location)))
      buffer))
   (t
    (signal 'file-missing (list "Remote location does not exist" location)))))

(defun consult-tramp--open-location (location)
  "Open remote LOCATION in Dired, falling back to its remote home.
If LOCATION names a file, show its parent directory and select the file.
If LOCATION cannot be opened, try its connection prefix, which TRAMP
resolves to the remote default directory.  Never fall back locally."
  (let ((home (file-remote-p location)))
    (unless (and (stringp home) (consult-tramp--valid-location-p location))
      (user-error "Not a valid TRAMP location: %s" location))
    (condition-case original-error
        (consult-tramp--visit-location location)
      (error
       (if (equal location home)
           (signal (car original-error) (cdr original-error))
         (condition-case home-error
             (let ((buffer (dired home)))
               (message "Cannot open %s (%s); opened remote home %s"
                        location (error-message-string original-error) home)
               buffer)
           (error
            (error "Cannot open %s (%s); remote home %s also failed (%s)"
                   location (error-message-string original-error)
                   home (error-message-string home-error)))))))))

;;;###autoload
(defun consult-tramp ()
  "Select or enter a TRAMP location and open it in Dired.
Candidates are collected from `consult-tramp-sources' and grouped by
method.  Arbitrary concrete TRAMP locations are accepted.  Remote access
only begins after a location is submitted; candidate preview is disabled."
  (interactive)
  (let* ((locations
          (consult--slow-operation "Collecting TRAMP locations..."
            (consult-tramp-locations)))
         (location
          (consult--read
           (consult-tramp--sort-locations locations)
           :prompt "TRAMP location: "
           :require-match nil
           :category 'file
           :history 'file-name-history
           :sort nil
           :group #'consult-tramp--group
           :preview-key nil)))
    (unless (consult-tramp--valid-location-p location)
      (user-error "Not a valid TRAMP location: %s" location))
    (consult-tramp--open-location location)))

(provide 'consult-tramp)
;;; consult-tramp.el ends here
