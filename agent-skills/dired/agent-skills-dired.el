(require 'cl-lib)
(require 'dired)

(cl-defun agent-skill-dired (&key dir files)
  "Open a dired buffer at DIR with FILES marked.

When all files share the same parent directory, DIR is that
directory and FILES are basenames shown in context.

When files span multiple directories, DIR is the common ancestor
and FILES are relative paths.  A curated `*agent-files*' buffer
is created instead."
  (let ((same-dir-p (cl-every (lambda (f) (not (string-match-p "/" f))) files)))
    (if same-dir-p
        (progn
          (dired dir)
          (dired-unmark-all-marks)
          (dolist (file files)
            (dired-goto-file (expand-file-name file dir))
            (dired-mark 1)))
      (let ((default-directory (file-name-as-directory dir)))
        (dired (cons "*agent-files*" files))
        (dired-unmark-all-marks)
        (dired-toggle-marks)))))

(provide 'agent-skill-dired)
