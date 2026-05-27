;;; init-config-eshell.el --- Eshell configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(defcustom +eshell/proxy
  "127.0.0.1:10808"
  "Proxy used by local Eshell proxy helpers."
  :type 'string
  :group 'eshell)

(defun +shell/zsh ()
  "Call /bin/zsh in shell mode"
  (interactive)
  (let ((explicit-shell-file-name "/bin/zsh") )
    (shell)))

(defun +shell/bash ()
  "Call /bin/bash in shell mode"
  (interactive)
  (let ((explicit-shell-file-name "/bin/bash") )
    (shell)))

(defun +shell/sh ()
  "Call /bin/sh in shell mode"
  (interactive)
  (let ((explicit-shell-file-name "/bin/sh") )
    (shell)))

(setopt eshell-scroll-show-maximum-output nil
        eshell-highlight-prompt nil
        eshell-destroy-buffer-when-process-dies t)

(setopt eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input)))
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

(setopt eshell-prompt-function
        (lambda nil
          (let* ((cwd (abbreviate-file-name (eshell/pwd))))
            (concat (propertize
                     ;; the above line
                     (format "%s [%s]"
                             (propertize (user-login-name) 'font-lock-face 'font-lock-comment-face)
                             (propertize cwd 'font-lock-face 'font-lock-constant-face)
                             )
                     'read-only t
                     'front-sticky   '(font-lock-face read-only)
                     'rear-nonsticky '(font-lock-face read-only))
                    ;; input line
                    " $ "
                    ))))

(setopt eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face)))

;; always get a new eshell
(defun +eshell/new ()
  (interactive)
  (let ((current-prefix-arg ""))
    (call-interactively 'eshell)))

;; (defun +eshell/project-new ()
;;   (interactive)
;;   (let ((current-prefix-arg (1+ (cl-position (persp-current-name) (persp-names) :test 'equal))))
;;     (call-interactively 'eshell)))

(defun +eshell/set-proxy (proxy)
  "Set proxy environment variables and git proxy configuration."
  ;; Set environment variables
  (setenv "http_proxy" proxy)
  (setenv "https_proxy" proxy)
  (setenv "ftp_proxy" proxy)
  (setenv "HTTP_PROXY" proxy)
  (setenv "HTTPS_PROXY" proxy)
  (setenv "FTP_PROXY" proxy)

  ;; Set git proxy configuration
  (if proxy
      (progn (shell-command (format "git config --global http.proxy %s" proxy))
             (shell-command (format "git config --global https.proxy %s" proxy)))
    (progn (shell-command (format "git config --global --unset http.proxy"))
           (shell-command (format "git config --global --unset https.proxy")))))

(defun eshell/set-proxy ()
  "Set proxy environment variables and git proxy configuration."
  (interactive)
  (let ((my-proxy +eshell/proxy))
    (when my-proxy
      ;; Set proxy
      (+eshell/set-proxy my-proxy)
      ;; Show proxy settings
      (eshell/show-proxy))))

(defun eshell/unset-proxy ()
  "Set proxy environment variables and git proxy configuration."
  (interactive)
  (+eshell/set-proxy nil)
  ;; Show proxy settings
  (eshell/show-proxy))

(defun +eshell/format-shell-command (command)
  (let* ((str
          (replace-regexp-in-string "\n$" ""
                                    (shell-command-to-string command))))
    (if (string-empty-p str) nil str)))

(defun eshell/show-proxy ()
  "Display current proxy settings."
  (interactive)
  (eshell-printn (format "[env] http_proxy  : %s" (getenv "http_proxy")))
  (eshell-printn (format "[env] https_proxy : %s" (getenv "https_proxy")))
  (eshell-printn (format "[env] ftp_proxy   : %s" (getenv "ftp_proxy")))
  (eshell-printn (format "[git] http_proxy  : %s" (+eshell/format-shell-command "git config --global --get http.proxy")))
  (eshell-printn (format "[git] https_proxy : %s" (+eshell/format-shell-command "git config --global --get https.proxy"))))

;; HACK redefine eshell/clear function using advice
(defun +eshell/clear-buffer-a (&rest _)
  "Clear the eshell buffer by erasing its contents."
  (let ((inhibit-read-only t))
    (erase-buffer)))
(advice-add 'eshell/clear :override #'+eshell/clear-buffer-a)

(provide 'init-config-eshell)
;;; init-config-eshell.el ends here
