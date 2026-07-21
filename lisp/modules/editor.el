;;; editor.el --- editing defaults and editor packages -*- lexical-binding: t -*-
;;; Commentary:
;; Editing defaults and editor packages: smartparens, yasnippet, iedit, wgrep,
;; hl-todo and the global text-editing commands.
;;; Code:


(require 'init-config-evil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setopt text-mode-ispell-word-completion nil)

(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

(global-visual-line-mode 1)
(setq word-wrap-by-category t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq tab-always-indent t) ;  TAB just indents the current line

(setq enable-recursive-minibuffers t)

(setq use-short-answers t)

(setopt fill-column 80)

(require 'hideshow)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(require 'savehist)
(savehist-mode 1)

(setopt comint-scroll-to-bottom-on-output t)
(setopt ansi-color-for-comint-mode t)
(setopt comint-prompt-read-only t)
(setopt comint-buffer-maximum-size 2048)

(setopt duplicate-line-final-position 1) ; move point to the first newline

(put 'narrow-to-region 'disabled nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-highlight-punctuation ":")
  :config
  (global-hl-todo-mode 1))

(use-package pangu-spacing
  :ensure t
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor nil)
  (add-hook 'org-mode-hook
            #'(lambda ()
                (set (make-local-variable
                      'pangu-spacing-real-insert-separtor) t))))

(use-package smartparens
  :ensure t  ;; install the package
  :hook (prog-mode text-mode markdown-mode org-mode)
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package yasnippet
  :ensure t
  :config
  (let ((my-yas-dir (concat +emacs/repo-directory "/snippets")))
    (add-to-list 'yas-snippet-dirs my-yas-dir))
  ;; start mode globally
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :ensure t)

(use-package wgrep
  :ensure t)

(defun +evil/fix-path-separator-syntax-h ()
  "Treat path separators as punctuation in output buffers."
  (set-syntax-table (copy-syntax-table (syntax-table)))
  (modify-syntax-entry ?/ "."))

(dolist (hook '(grep-mode-hook
                compilation-mode-hook
                comint-mode-hook))
  (add-hook hook #'+evil/fix-path-separator-syntax-h))

(use-package iedit
  :ensure t
  :init
  ;; Fix conflict with embark.
  (setq iedit-toggle-key-default nil))

(use-package ansi-color
  :ensure t
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package helpful
  :ensure t)

(use-package crux
  :ensure t)

(defun +editor/save-all-buffers ()
  (interactive)
  (let* ((current-prefix-arg '(4)))
    (call-interactively 'save-some-buffers)))

(defun +editor/copy-ref-dwim ()
  "Copy the current file location and text at point to the kill ring.

When a region is active, copy the file with the selected line range.
Otherwise, copy the current line."
  (interactive)
  (let* ((file (or buffer-file-name
                   (user-error "Buffer is not associated with a file")))
         (has-region (use-region-p))
         (beg (if has-region (region-beginning) (line-beginning-position)))
         (raw-end (if has-region (region-end) (line-end-position)))
         (end (if (> raw-end beg) (1- raw-end) raw-end))
         (start-line (line-number-at-pos beg t))
         (end-line (line-number-at-pos end t))
         (line-range
          (format "%s:%d-%d" file start-line end-line))
         (content
          (if (= start-line end-line)
              (let ((content-end (if (and (> raw-end beg)
                                          (eq (char-before raw-end) ?\n))
                                     (1- raw-end)
                                   raw-end)))
                (buffer-substring-no-properties beg content-end))
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- start-line))
              (let ((line start-line)
                    lines)
                (while (<= line end-line)
                  (push (format "   %d: %s"
                                line
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                        lines)
                  (forward-line 1)
                  (setq line (1+ line)))
                (mapconcat #'identity (nreverse lines) "\n")))))
         (text (format "%s\n\n%s" line-range content)))
    (kill-new text)
    (setq deactivate-mark t)
    (message "Yanked: %s" line-range)))

(defun +editor/copy-buffer-file-name ()
  "Copy the current buffer file name to clipboard."
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message "Copied: %s" name)))

(defun +editor/copy-buffer-identifier ()
  "Copy an evaluable identifier for the current buffer.

The identifier is an Emacs Lisp `get-buffer' form, so an agent with
`emacs_eval' access can resolve file-backed and non-file buffers alike."
  (interactive)
  (let ((identifier (format "(get-buffer %S)" (buffer-name))))
    (kill-new identifier)
    (message "Copied: %s" identifier)))

(defun +editor/wc-non-ascii (&optional start end)
  "Count lines, non-ASCII characters, and characters in region or buffer."
  (interactive)
  (let ((start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char start)
        (message "lines: %3d non ascii words: %3d chars: %3d"
                 (count-lines start end)
                 (count-matches "[^[:ascii:]]")
                 (- end start))))))

(use-package speed-type
  :ensure t
  :defer t)

(use-package keyfreq
  :ensure t
  :hook (after-init . keyfreq-mode)
  :config
  (keyfreq-autosave-mode 1))


(provide 'init-editor)
;;; editor.el ends here
