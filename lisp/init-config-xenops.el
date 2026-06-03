;;; init-config-xenops.el --- Xenops configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-element)
(require 'subr-x)

(declare-function smartparens-mode "smartparens")
(declare-function xenops-math-parse-inline-element-at-point "xenops-math")

(defvar-local fn/xenops-math-smartparens-suspended nil
  "Non-nil when Smartparens is suspended while editing inline math.")

(defvar-local fn/xenops-math-inline-editing-begin nil
  "Beginning of the Xenops inline math element currently being edited.")

(defun fn/xenops-math-inline-editing-element ()
  "Return the Xenops inline math element being edited at point."
  (and (bound-and-true-p xenops-mode)
       (fboundp 'xenops-math-parse-inline-element-at-point)
       (ignore-errors
         (when-let* ((element (xenops-math-parse-inline-element-at-point))
                     (begin-content (plist-get element :begin-content))
                     (end-content (plist-get element :end-content)))
           (when (and (eq (plist-get element :type) 'inline-math)
                      (<= begin-content (point) end-content))
             element)))))

(defun fn/xenops-math-inline-editing-p ()
  "Return non-nil when point is inside a Xenops inline math element."
  (not (null (fn/xenops-math-inline-editing-element))))

(defun fn/xenops-math-sync-smartparens-h ()
  "Suspend Smartparens while editing Xenops inline math."
  (when (fboundp 'smartparens-mode)
    (if-let* ((element (fn/xenops-math-inline-editing-element)))
        (progn
          (setq fn/xenops-math-inline-editing-begin
                (plist-get element :begin))
          (when (bound-and-true-p smartparens-mode)
            (setq fn/xenops-math-smartparens-suspended t)
            (smartparens-mode -1)))
      (setq fn/xenops-math-inline-editing-begin nil)
      (when fn/xenops-math-smartparens-suspended
        (setq fn/xenops-math-smartparens-suspended nil)
        (smartparens-mode 1)))))

(defun fn/xenops-math-restore-smartparens-h ()
  "Restore Smartparens if inline math editing suspended it."
  (when (and fn/xenops-math-smartparens-suspended
             (fboundp 'smartparens-mode))
    (setq fn/xenops-math-smartparens-suspended nil)
    (smartparens-mode 1)))

(defun fn/xenops-math-ignore-repeated-reveal-a (orig-fn window oldpos event-type)
  "Avoid repeated Xenops reveal while editing the same inline math element."
  (if (and (eq event-type 'entered)
           fn/xenops-math-inline-editing-begin
           (when-let* ((element (fn/xenops-math-inline-editing-element)))
             (= (plist-get element :begin)
                fn/xenops-math-inline-editing-begin)))
      nil
    (funcall orig-fn window oldpos event-type)))

(defun fn/xenops-math-setup-smartparens-suspension-h ()
  "Install buffer-local Smartparens suspension for Xenops inline math."
  (add-hook 'pre-command-hook #'fn/xenops-math-sync-smartparens-h nil t)
  (add-hook 'post-command-hook #'fn/xenops-math-sync-smartparens-h nil t)
  (add-hook 'change-major-mode-hook
            #'fn/xenops-math-restore-smartparens-h nil t)
  (add-hook 'kill-buffer-hook
            #'fn/xenops-math-restore-smartparens-h nil t))

(use-package xenops
  :ensure t
  :if (and window-system (not (eq system-type 'windows-nt))) ;; do not load xenops on terminal emacs or windows nt
  :config
  (setq xenops-font-family "Maple Mono NL NF CN")
  (setq xenops-reveal-on-entry t)
  (setq xenops-math-latex-process-alist org-preview-latex-process-alist)
  (add-hook 'org-mode-hook #'xenops-mode)
  (setq xenops-math-latex-process 'xdvisvgm) ; HACK or, ximagemagick
  (defvar fn/xenops-math-image-reference-font-size 14.0
    "Cnfonts English font size matched by `fn/xenops-math-image-reference-scale-factor'.")
  (defvar fn/xenops-math-image-reference-scale-factor 0.8
    "Xenops image scale that visually matches `fn/xenops-math-image-reference-font-size'.")
  (defun fn/xenops-math-current-cnfonts-size (&optional fontsizes-list)
    "Return current cnfonts English font size from FONTSIZES-LIST or active profile."
    (or (and (numberp (car-safe fontsizes-list))
             (float (car fontsizes-list)))
        (and (boundp 'cnfonts--config-info)
             (fboundp 'cnfonts--get-current-profile)
             (let ((size (cdr (assoc (cnfonts--get-current-profile t)
                                     cnfonts--config-info))))
               (and (numberp size) (float size))))
        (let ((height (face-attribute 'default :height nil 'default)))
          (cond
           ((integerp height) (/ height 10.0))
           ((floatp height) (* fn/xenops-math-image-reference-font-size height))))))
  (defun fn/xenops-math-sync-image-scale (&optional fontsizes-list)
    "Sync `xenops-math-image-scale-factor' with current cnfonts size.
Return non-nil when the scale changed."
    (when-let* ((font-size (fn/xenops-math-current-cnfonts-size fontsizes-list))
                (scale (* fn/xenops-math-image-reference-scale-factor
                          (/ font-size fn/xenops-math-image-reference-font-size))))
      (unless (equal xenops-math-image-scale-factor scale)
        (setq xenops-math-image-scale-factor scale)
        t)))
  (defun fn/xenops-refresh-rendered-buffers ()
    "Refresh rendered Xenops buffers after math image scale changes."
    (when (featurep 'xenops)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (bound-and-true-p xenops-mode)
                     (catch 'found-xenops-overlay
                       (dolist (ov (overlays-in (point-min) (point-max)))
                         (when (eq (overlay-get ov 'xenops-overlay-type) 'xenops-overlay)
                           (throw 'found-xenops-overlay t)))))
            (save-excursion
              (save-restriction
                (widen)
                (xenops-overlay-delete-overlays-in (point-min) (point-max))
                (xenops-render))))))))
  (defun fn/xenops-math-sync-image-scale-after-cnfonts (&optional fontsizes-list)
    "Update Xenops math previews after cnfonts applies FONTSIZES-LIST."
    (when (fn/xenops-math-sync-image-scale fontsizes-list)
      (fn/xenops-refresh-rendered-buffers)))
  (defun fn/xenops-math-sync-image-scale-before-render (&rest _)
    "Ensure Xenops uses the current cnfonts size before rendering."
    (fn/xenops-math-sync-image-scale))
  (defun fn/xenops-math-cache-key-include-scale (orig-fn &rest args)
    "Add `xenops-math-image-scale-factor' to Xenops math cache keys."
    (append (apply orig-fn args)
            (list (list 'xenops-math-image-scale-factor
                        xenops-math-image-scale-factor))))
  (fn/xenops-math-sync-image-scale)
  (add-hook 'cnfonts-set-font-finish-hook
            #'fn/xenops-math-sync-image-scale-after-cnfonts)
  (unless (advice-member-p #'fn/xenops-math-sync-image-scale-before-render
                           'xenops-math-render)
    (advice-add 'xenops-math-render
                :before #'fn/xenops-math-sync-image-scale-before-render))
  (unless (advice-member-p #'fn/xenops-math-cache-key-include-scale
                           'xenops-math-file-name-static-hash-data)
    (advice-add 'xenops-math-file-name-static-hash-data
                :around #'fn/xenops-math-cache-key-include-scale))
  (defun fn/xenops-math-display-image-set-svg-foreground (element &rest _)
    "Use SVG's standard initial foreground for Xenops math images."
    (let ((beg (plist-get element :begin))
          (end (plist-get element :end)))
      (when (and beg end)
        (dolist (ov (overlays-in beg end))
          (let ((display (overlay-get ov 'display)))
            (when (and (eq (overlay-get ov 'xenops-overlay-type) 'xenops-overlay)
                       (consp display)
                       (eq (car display) 'image)
                       (eq (plist-get (cdr display) :type) 'svg))
              (overlay-put
               ov 'display
               (cons 'image
                     (plist-put (copy-sequence (cdr display))
                                :foreground "black")))))))))
  (unless (advice-member-p #'fn/xenops-math-display-image-set-svg-foreground
                           'xenops-math-display-image)
    (advice-add 'xenops-math-display-image
                :after #'fn/xenops-math-display-image-set-svg-foreground))
  (add-hook 'org-mode-hook #'fn/xenops-math-setup-smartparens-suspension-h)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (fn/xenops-math-setup-smartparens-suspension-h))))
  (unless (advice-member-p #'fn/xenops-math-ignore-repeated-reveal-a
                           'xenops-math-handle-element-transgression)
    (advice-add 'xenops-math-handle-element-transgression
                :around #'fn/xenops-math-ignore-repeated-reveal-a))
  (defun fn/xenops-src-parse-at-point ()
    (-if-let*
        ((element (xenops-parse-element-at-point 'src))
         (org-babel-info
          (xenops-src-do-in-org-mode
           (org-babel-get-src-block-info 'light (org-element-context)))))
        (xenops-util-plist-update
         element
         :type 'src
         :language (nth 0 org-babel-info)
         :org-babel-info org-babel-info)))

  ;; NOTE error from xenops with org>9.7
  ;; https://github.com/syl20bnr/spacemacs/issues/16577
  ;; https://github.com/dandavison/xenops/pull/74/files
  ;; https://github.com/dandavison/xenops/issues/73
  (advice-add 'xenops-src-parse-at-point
              :override 'fn/xenops-src-parse-at-point))

(provide 'init-config-xenops)
;;; init-config-xenops.el ends here
