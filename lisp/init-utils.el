;;; init-utils.el --- core functionality support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun +ensure-packages-installed (packages-alist)
  "Make sure the given package is installed."
  (dolist (p packages-alist)
    (unless (package-installed-p p)
      (package-install p))))

;;;###autoload
(cl-defun +vertico-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory.
:args LIST
  Arguments to be appended to `consult-ripgrep-args'."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'consult)
  (setq deactivate-mark t)
  (let* ((project-root (or (doom-modeline--project-root) default-directory))
         (directory (or in project-root))
         (consult-ripgrep-args
          (concat "rg "
                  (if all-files "-uu ")
                  (unless recursive "--maxdepth 1 ")
                  "--null --line-buffered --color=never --max-columns=1000 "
                  "--smart-case --no-heading "
                  "--with-filename --line-number --search-zip "
                  "--hidden -g !.git -g !.svn -g !.hg "
                  (mapconcat #'identity args " ")))
         (prompt (if (stringp prompt) (string-trim prompt) "Search"))
         (query query)
         (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial _function)
          (alist-get consult-async-split-style consult-async-split-styles-alist)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                     (concat "\\" (char-to-string separator))
                                     query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                            unless (string-match-p char query)
                                            return char)
                                   "%")
                              :type perl)
                   consult-async-split-style 'perlalt))))))
    (consult--grep prompt #'consult--ripgrep-make-builder directory query)))

;;;###autoload
(defun +vertico/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+vertico-file-search :query initial-query :in directory :all-files arg))

(defun +revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

;; HACK setup environment
;; see: https://www.emacswiki.org/emacs/ExecPath
(defun +set-emacs-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
  that used by the user's shell.

  This is particularly useful under Mac OS X and macOS, where GUI
  apps are not started from a shell."
  (interactive)
  (when (not (eq system-type 'windows-nt))
    (let ((path-from-shell
           (replace-regexp-in-string
            "[ \t\n]*$" "" (shell-command-to-string
                            "$SHELL --login -c 'echo $PATH'"
                            ))))
      ;; (message path-from-shell)
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  )

(defun +persp/move-buffer-prev ()
  "Like persp-prev, but move current."
  (interactive)
  (let ((tmp-buffer (current-buffer)))
    (persp-forget-buffer tmp-buffer)
    (persp-prev)
    (persp-set-buffer tmp-buffer)
    (persp-switch-to-buffer tmp-buffer))
  )

(defun +persp/move-buffer-next ()
  "Like persp-next, but move current."
  (interactive)
  (let ((tmp-buffer (current-buffer)))
    (persp-forget-buffer tmp-buffer)
    (persp-next)
    (persp-set-buffer tmp-buffer)
    (persp-switch-to-buffer tmp-buffer))
  )

(provide 'init-utils)
