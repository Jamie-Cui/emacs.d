;;; init-config-citar.el --- Citar configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)

(defcustom +citar/org-root-dir
  (expand-file-name "~/opt/org-root")
  "Root directory for Citar bibliography, notes, and PDFs."
  :type 'directory
  :group 'citar)

(defun +citar/ensure-directory (dir)
  "Ensure DIR exists and return it."
  (make-directory dir t)
  dir)

(defun +citar/org-subdir (name)
  "Return org root subdirectory NAME, creating it when needed."
  (+citar/ensure-directory
   (expand-file-name name +citar/org-root-dir)))

(defun +citar/all-ref-file ()
  "Return the main bibliography file path."
  (expand-file-name "all-ref.bib"
                    (+citar/ensure-directory +citar/org-root-dir)))

(use-package citar
  :ensure t
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :custom
  (citar-select-multiple nil)
  ;; (org-cite-insert-processor 'citar)
  ;; (org-cite-follow-processor 'citar)
  ;; (org-cite-activate-processor 'citar)
  :config
  (setq citar-at-point-function 'embark-act)
  (add-to-list 'citar-bibliography
               (+citar/all-ref-file))
  (add-to-list 'citar-library-paths
               (+citar/org-subdir "pdf"))
  (add-to-list 'citar-notes-paths
               (+citar/org-subdir "roam"))
  (setf (alist-get 'note citar-templates) "${=key=}")
  ;; NOTE this var is used by org-export
  (add-to-list 'org-cite-global-bibliography
               (+citar/all-ref-file))

  ;; Make citar-open-entry show item in ebib
  ;; NOTE only global bib works, we do not support project-wise bib
  (defun citar-open-entry-in-ebib (citekey)
    "Open entry for CITEKEY in ebib."
    ;; Adapted from 'bibtex-completion-show-entry'.
    (unless (fboundp 'ebib)
      (user-error "Ebib is not available"))
    (ebib (+citar/all-ref-file) citekey))
  (setopt citar-open-entry-function #'citar-open-entry-in-ebib)

  ;; Extend citar-open popup with "Copy Title" and "Open in Ebib" options.
  ;; Candidates are encoded as "[Copy Title] KEY" / "[Open in Ebib] KEY" so
  ;; the key survives citar--select-resource stripping text properties.
  (defun +citar--get-resource-extras-a (orig citekeys &rest args)
    "Inject per-field copy and open-in-ebib candidates into `citar--get-resource-candidates'.
Also simplifies create-note candidates to show only the note filename."
    (let* ((orig-result (apply orig citekeys args))
           ;; Cache entries - citar-get-entry is otherwise called twice per key.
           (entry-cache (let ((tbl (make-hash-table :test #'equal :size (length citekeys))))
                          (dolist (k citekeys tbl) (puthash k (citar-get-entry k) tbl))))
           ;; Replace create-note candidates' verbose display with just the note filename.
           (result (when orig-result
                     (cons (car orig-result)
                           (mapcar (lambda (cand)
                                     (let* ((mc    (get-text-property 0 'multi-category cand))
                                            (inner (and mc (cdr mc)))
                                            (rtype (and inner (get-text-property 0 'citar--resource inner))))
                                       (if (eq rtype 'create-note)
                                           (let* ((key  (substring-no-properties inner))
                                                  (name (when-let* (((fboundp 'citar-file--get-note-filename))
                                                                    (path (citar-file--get-note-filename key)))
                                                          (file-name-nondirectory path)))
                                                  (name (or name key))
                                                  ;; Keep the hidden citekey prefix and outer resource
                                                  ;; type so `citar--select-resource' and Embark can
                                                  ;; still dispatch create-note correctly.
                                                  (display (citar--prepend-candidate-citekey key name)))
                                             (propertize display
                                                         'citar--resource rtype
                                                         'multi-category mc))
                                         cand)))
                                   (cdr orig-result)))))
           (extra-cands
            (mapcan (lambda (key)
                      (let* ((entry   (gethash key entry-cache))
                             (fields  (seq-remove (lambda (c) (string-prefix-p "=" (car c))) entry))
                             (max-len (apply #'max (length "key")
                                             (mapcar (lambda (c) (length (car c))) fields)))
                             (fmt     (format "[%%-%ds] %%s" max-len))
                             (key-cand (propertize (format fmt "key" key)
                                                   'citar--resource 'copy-field))
                             (copy-cands (mapcar (lambda (cell)
                                                   (propertize (format fmt (car cell) (cdr cell))
                                                               'citar--resource 'copy-field))
                                                 fields)))
                        (append (list key-cand) copy-cands
                                (list (propertize key 'citar--resource 'open-in-ebib)))))
                    citekeys)))
      (cons 'multi-category (append (when result (cdr result)) extra-cands))))

  (defun +citar--open-resource-extras-a (orig resource &optional type)
    "Handle copy-field and open-in-ebib in `citar--open-resource'."
    (pcase (or type (get-text-property 0 'citar--resource resource))
      ('copy-field
       (let ((val (and (string-match "\\`\\[[^]]+\\] \\(\\(?:.\\|\n\\)*\\)\\'" resource)
                       (match-string 1 resource))))
         (kill-new (or val resource))
         (message "Copied: %s" (or val resource))))
      ('open-in-ebib
       (citar-open-entry-in-ebib (substring-no-properties resource)))
      (_ (funcall orig resource type))))

  (defun +citar--select-group-extras-a (orig resource transform)
    "Add group labels for copy-field and open-in-ebib in completing-read.
Also fixes create-note grouping: after display replacement the outer string
has no direct `citar--resource', so we look it up via `multi-category'."
    (let* ((direct (get-text-property 0 'citar--resource resource))
           (rtype  (or direct
                       (when-let* ((mc    (get-text-property 0 'multi-category resource))
                                   (inner (cdr mc)))
                         (get-text-property 0 'citar--resource inner)))))
      (pcase rtype
        ('copy-field   (if transform resource "Copy Field"))
        ('open-in-ebib (if transform resource "Open in Ebib"))
        (_
         ;; Pass the resource with citar--resource set so the original
         ;; group function correctly labels create-note (and other) types.
         (funcall orig
                  (if (and rtype (not direct))
                      (propertize resource 'citar--resource rtype)
                    resource)
                  transform)))))

  (advice-add 'citar--get-resource-candidates :around #'+citar--get-resource-extras-a)
  (advice-add 'citar--open-resource :around #'+citar--open-resource-extras-a)
  (advice-add 'citar--select-group-related-resources :around #'+citar--select-group-extras-a)

  (advice-add 'citar-insert-citation :around
              (lambda (orig-fun &rest args)
                (when (fboundp '+evil/smart-insert)
                  (+evil/smart-insert))
                (apply orig-fun args))))

(provide 'init-config-citar)
;;; init-config-citar.el ends here
