;;; project.el --- projects and workspaces -*- lexical-binding: t -*-
;;; Commentary:
;; Projects and workspaces: projectile, perspective and envrc.
;;; Code:


;; -----------------------------------------------------------
;; DONE Workspaces
;;
;; persp-projectile (site-lisp)
;; perspective
;; projectile
;; -----------------------------------------------------------

(use-package persp-projectile
  :after (perspective projectile)
  :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/")))

(use-package perspective
  :ensure t
  :custom
  (persp-suppress-no-prefix-key-warning t)
  (persp-sort 'created)
  (persp-modestring-dividers '("(Proj:" ")" ""))
  (persp-show-modestring nil)
  (persp-modestring-short nil)
  :config
  (persp-mode)
  (put 'persp-selected-face 'face-alias 'success))

(use-package projectile
  :ensure t
  :custom
  (projectile-indexing-method 'hybrid)
  ;; DO NOT add / remove project automatically
  (projectile-track-known-projects-automatically nil)
  ;; DO NOT enable cache
  (projectile-enable-caching nil)
  ;; each project has a separate compilation buffer
  (projectile-per-project-compilation-buffer t)
  ;; remote cache is avaliable for 5 min
  (projectile-file-exists-remote-cache-expire (* 5 60))
  ;; only git as project identifier
  (projectile-project-root-files-bottom-up '(".git"))
  ;; auto update cache when files are opened or deleted
  (projectile-auto-update-cache t)
  ;; I prefer citre, do not use built-in tag systm
  (projectile-tags-backend nil)
  :config
  ;; see: https://github.com/syl20bnr/spacemacs/issues/11381#issuecomment-481239700
  ;; (defadvice projectile-project-root (around ignore-remote first activate)
  ;;   (unless (file-remote-p default-directory) ad-do-it))
  ;; Projectile 2.10 auto-adds project type manifests such as CMakeLists.txt to
  ;; the bottom-up root markers during load, so enforce this after registration.
  (setopt projectile-project-root-files-bottom-up '(".git"))
  (projectile-discard-root-cache)
  (projectile-mode +1)

  ;; see: https://metaredux.com/posts/2025/02/03/projectile-introduces-significant-caching-improvements.html
  ;; Defer cache loading to after-init for faster startup
  (add-hook 'after-init-hook
            (lambda ()
              ;; initialize the projects cache if needed
              (unless projectile-projects-cache
                (setq projectile-projects-cache
                      (or (projectile-unserialize projectile-cache-file)
                          (make-hash-table :test 'equal))))
              (unless projectile-projects-cache-time
                (setq projectile-projects-cache-time
                      (make-hash-table :test 'equal)))
              ;; load the known projects
              (projectile-load-known-projects)
              ;; update the list of known projects
              (projectile--cleanup-known-projects)
              (when projectile-auto-discover
                (projectile-discover-projects-in-search-path))))
  )

(defun +project/format-name-as-in-echo (name)
  "Format the perspective name given by NAME for display in the echo area."
  (if (equal name (persp-current-name))
      (setq name (format "[%s]" name))
    (setq name (format " %s " name))))

(defun +project/show-name-in-echo ()
  "Show persp names in the echo area."
  (let ((message-log-max nil))
    (message (mapconcat 'identity
                        (mapcar '+project/format-name-as-in-echo
                                (persp-names))))))

(defun +project/move-buffer-prev ()
  "Like persp-prev, but move current."
  (interactive)
  (let ((tmp-buffer (current-buffer)))
    (persp-forget-buffer tmp-buffer)
    (persp-prev)
    (persp-set-buffer tmp-buffer)
    (persp-switch-to-buffer tmp-buffer))
  (+project/show-name-in-echo))

(defun +project/move-buffer-next ()
  "Like persp-next, but move current."
  (interactive)
  (let ((tmp-buffer (current-buffer)))
    (persp-forget-buffer tmp-buffer)
    (persp-next)
    (persp-set-buffer tmp-buffer)
    (persp-switch-to-buffer tmp-buffer))
  (+project/show-name-in-echo))

;; let 'quit signal to show the persp names
(advice-add 'keyboard-quit :before #'(lambda () (put 'quit 'error-message (+project/show-name-in-echo))))

;; install dir: https://direnv.net/
;; brew install direnv
;; sudo dnf install direnv
(use-package envrc
  :ensure t
  :custom
  (envrc-remote t)
  :hook (after-init . envrc-global-mode))


(provide 'init-project)
;;; project.el ends here
