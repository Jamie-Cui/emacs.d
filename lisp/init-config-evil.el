;;; init-config-evil.el --- evil mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Vim emulation setup using evil mode with related extensions
;;; Code:

(require 'subr-x)

(defvar-local +evil--syntax-ppss-memo-last-point nil)
(defvar-local +evil--syntax-ppss-memo-last-result nil)

(defun +evil--syntax-ppss-memo-reset-h (&rest _ignored)
  "Reset evil syntax memoization after buffer changes."
  (setq +evil--syntax-ppss-memo-last-point nil
        +evil--syntax-ppss-memo-last-result nil))

(defun +evil--syntax-ppss (&optional p)
  "Memoize the last `syntax-ppss' result for P."
  (let ((p (or p (point)))
        (mem-p +evil--syntax-ppss-memo-last-point))
    (if (and (eq p (nth 0 mem-p))
             (eq (point-min) (nth 1 mem-p))
             (eq (point-max) (nth 2 mem-p)))
        +evil--syntax-ppss-memo-last-result
      (unless +evil--syntax-ppss-memo-last-point
        (add-hook 'before-change-functions
                  #'+evil--syntax-ppss-memo-reset-h t t))
      (setq +evil--syntax-ppss-memo-last-point
            (list p (point-min) (point-max))
            +evil--syntax-ppss-memo-last-result
            (syntax-ppss p)))))

(defun +evil--point-in-comment-p (&optional pt)
  "Return non-nil if PT, or point, is in a comment."
  (let ((pt (or pt (point))))
    (ignore-errors
      (save-excursion
        (unless (nth 3 (+evil--syntax-ppss pt))
          (or (nth 4 (+evil--syntax-ppss pt))
              (and (< pt (point-max))
                   (memq (char-syntax (char-after pt)) '(?< ?>))
                   (not (eq (char-after pt) ?\n)))
              (when-let* ((s (car (syntax-after pt))))
                (or (and (/= 0 (logand (ash 1 16) s))
                         (nth 4 (syntax-ppss (+ pt 2))))
                    (and (/= 0 (logand (ash 1 17) s))
                         (nth 4 (syntax-ppss (+ pt 1))))
                    (and (/= 0 (logand (ash 1 18) s))
                         (nth 4 (syntax-ppss (- pt 1))))
                    (and (/= 0 (logand (ash 1 19) s))
                         (nth 4 (syntax-ppss (- pt 2))))))))))))

;; -----------------------------------------------------------
;; DONE evil
;;
;; expand-region
;; undo-tree
;; move-text
;; evil
;; evil-collection
;; evil-args
;; evil-nerd-commenter
;; evil-mc
;; evil-multiedit
;; evil-surround
;; evil-escape
;; evil-terminal-cursor-changer (for terminal)
;; -----------------------------------------------------------

(use-package expand-region
  :ensure t
  :custom
  (expand-region-smart-cursor t))

(use-package undo-tree
  :ensure t
  :custom (undo-tree-history-directory-alist
           `(("." . ,(concat user-emacs-directory "undo-tree-hist/"))))
  :config
  (setopt undo-tree-map nil)
  (global-undo-tree-mode 1))

;; (use-package move-text
;;   :ensure t
;;   :config
;;   (defun indent-region-advice (&rest ignored)
;;     (let ((deactivate deactivate-mark))
;;       (if (region-active-p)
;;           (indent-region (region-beginning) (region-end))
;;         (indent-region (line-beginning-position) (line-end-position)))
;;       (setq deactivate-mark deactivate)))

;;   (advice-add 'move-text-up :after 'indent-region-advice)
;;   (advice-add 'move-text-down :after 'indent-region-advice))

(use-package evil
  :ensure t
  :preface
  (setq sentence-end-double-space nil) ; single space for sentence
  (setq evil-overriding-maps nil)
  (setq evil-want-keybinding nil)
  (setq evil-auto-indent nil)
  (setq evil-want-C-h-delete t)
  (setq evil-want-Y-yank-to-eol t) ; this need to be set before evil
  (setq evil-want-C-g-bindings t) ; this need to be set before evil
  ;; whether to use emacs bindings in insert-mode
  (setq evil-disable-insert-state-bindings nil)
  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; FIXME why upgrading evil breaks cursor drawing?
  (setq evil-default-cursor 'box
        evil-motion-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-emacs-state-cursor 'hbar)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t)
  (evil-set-undo-system 'undo-tree)
  (evil-mode 1)
  ;; make sure org-agenda start with normal mode
  (evil-set-initial-state 'org-agenda-mode 'normal)
  ;; HACK: Fix joining commented lines with J (evil-join).
  (defun +evil-join-a (fn beg end)
    "Join the selected lines.

This advice improves on `evil-join' by removing comment delimiters when joining
commented lines, without `fill-region-as-paragraph'.

Adapted from https://github.com/emacs-evil/evil/issues/606"
    (if-let* (((not (= (line-end-position) (point-max))))
              (cend (save-excursion (goto-char end) (line-end-position)))
              (cbeg (save-excursion
                      (goto-char beg)
                      (and (+evil--point-in-comment-p
                            (save-excursion
                              (goto-char (line-beginning-position 2))
                              (skip-syntax-forward " \t")
                              (point)))
                           (or (comment-search-backward (line-beginning-position) t)
                               (comment-search-forward  (line-end-position) t)
                               (and (+evil--point-in-comment-p beg)
                                    (stringp comment-continue)
                                    (or (search-forward comment-continue (line-end-position) t)
                                        beg)))))))
        (let* ((count (count-lines beg end))
               (count (if (> count 1) (1- count) count)))
          (uncomment-region (line-beginning-position 2)
                            (save-excursion
                              (goto-char cend)
                              (line-end-position 0)))
          (dotimes (_ count)
            (join-line 1)
            (save-match-data
              (when (or (and comment-continue
                             (not (string-empty-p comment-continue))
                             (looking-at (concat "\\(\\s-*" (regexp-quote comment-continue) "\\) ")))
                        (and comment-start-skip
                             (not (string-empty-p comment-start-skip))
                             (looking-at (concat "\\(\\s-*" comment-start-skip "\\)"))))
                (replace-match "" t nil nil 1)
                (just-one-space)))))
      ;; But revert to the default we're not in a comment, where
      ;; `fill-region-as-paragraph' is too greedy.
      (funcall fn beg end)))
  (advice-add #'evil-join :around #'+evil-join-a)
  )

(use-package evil-collection
  :ensure t
  :after (:and evil evil-mc)
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  ;; make sure the follwing key bindings always work
  (evil-collection-init)
  ;; NOTE re-bind keys
  (evil-collection-define-key '(normal visual) 'evil-mc-key-map (kbd "gz") evil-mc-cursors-map)
  )

(use-package evil-args
  :ensure t
  :after evil
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(use-package evil-nerd-commenter
  :ensure t
  :after evil)

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil ; too slow
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  )

(use-package evil-mc
  :ensure t
  :after evil
  :config
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  (global-evil-mc-mode  1) ;; enable
  )

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode 1))

;; HACK https://www.reddit.com/r/emacs/comments/45w9mv/comment/d3ud03t/
(defun normal-escape-pre-command-handler ()
  (interactive)
  (pcase this-command
    (_ (when (and (string= "C-g" (key-description (this-command-keys)))
                  (bound-and-true-p evil-mode)
                  (or (evil-insert-state-p)
                      (evil-emacs-state-p)))
         (evil-force-normal-state)))))
(add-hook 'pre-command-hook 'normal-escape-pre-command-handler)

(use-package evil-terminal-cursor-changer
  :ensure t
  :after evil
  :if (not window-system) ;; do not load on gui
  :config
  (evil-terminal-cursor-changer-activate)) ; or (etcc-on)

;;;###autoload
(evil-define-command +evil/window-split-a (&optional count file)
  "Same as `evil-window-split', but correctly updates the window history."
  :repeat nil
  (interactive "P<f>")
  ;; HACK: This ping-ponging between the destination and source windows is to
  ;;   update the window focus history, so that, if you close either split
  ;;   afterwards you won't be sent to some random window.
  (let ((origwin (selected-window))
        window-selection-change-functions)
    (select-window (split-window origwin count 'below))
    (unless evil-split-window-below
      (select-window origwin)))
  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload
(evil-define-command +evil/window-vsplit-a (&optional count file)
  "Same as `evil-window-split', but correctly updates the window history."
  :repeat nil
  (interactive "P<f>")
  ;; HACK: This ping-ponging between the destination and source windows is to
  ;;   update the window focus history, so that, if you close either split
  ;;   afterwards you won't be sent to some random window.
  (let ((origwin (selected-window))
        window-selection-change-functions)
    (select-window (split-window origwin count 'right))
    (unless evil-vsplit-window-right
      (select-window origwin)))
  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

(advice-add #'evil-window-split  :override #'+evil/window-split-a)
(advice-add #'evil-window-vsplit :override #'+evil/window-vsplit-a)

;;;###autoload
(defun +evil/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

;;;###autoload
(defun +evil/window-vsplit-and-follow ()
  "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

;; NOTE Since I'm using evil, i want an additional space when inserting
;;###autoload
(defun +evil/smart-insert ()
  "Enter insert mode smartly: if current char is whitespace, append after word."
  (interactive)
  (let ((c (char-after (point)))
        (eow (and (not (eobp)) (looking-at "\\b")))
        (blank-char-p (and (not (eobp)) (looking-at "\\s-"))))
    (message "current char: %c" c)
    (when (and (not eow) (not blank-char-p))
      (message "moving to the end of word")
      (call-interactively 'evil-forward-word-end))
    (call-interactively 'evil-append) ;; append
    ))

(provide 'init-config-evil)
