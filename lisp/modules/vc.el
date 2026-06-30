;;; vc.el --- version control -*- lexical-binding: t -*-
;;; Commentary:
;; Version control: magit and diff-hl.
;;; Code:


(setq-default vc-handled-backends '(Git))

(use-package diff-hl
  :ensure t
  :config
  (diff-hl-margin-mode 1)
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package magit
  :ensure t
  :config
  (setq magit-show-long-lines-warning nil)
  (setq magit-tramp-pipe-stty-settings 'pty)
  (setq magit-commit-show-diff nil)
  (setq magit-branch-direct-configure nil)
  (setq magit-refresh-status-buffer nil)
  (setq magit-log-section-commit-count 20)  ; Show fewer commits
  (setq magit-auto-revert-mode nil)
  (setq magit-diff-refine-hunk t)
  (setq magit-save-repository-buffers nil)
  (setq magit-revision-insert-related-refs nil)
  (setq magit-uniquify-buffer-names nil)
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes))
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; Install magit-auto-commit
  (with-eval-after-load 'transient
    (transient-append-suffix 'magit-commit #'magit-commit-create
      '("W" "Commit with WIP" (lambda (&optional args)
                                (interactive (list (magit-commit-arguments)))
                                (let ((message "chore: stale - work still in progress"))
                                  (magit-commit-create (append args `("--message" ,message "--edit")))))))
    )
  )


(provide 'init-vc)
;;; vc.el ends here
