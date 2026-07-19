;;; email.el --- Local Outlook mail with mu4e and mbsync -*- lexical-binding: t -*-
;;; Commentary:
;; mbsync mirrors Outlook.com mail from the loopback-only DavMail IMAP gateway
;; into a local Maildir.  mu4e reads and searches the local mu index, so normal
;; navigation never waits for Microsoft Graph.  Message and smtpmail keep using
;; DavMail's local SMTP gateway for outgoing mail.
;;
;; External dependencies (keep this list in sync with the setup note):
;; - DavMail >= 6.8.1 and a current OpenJDK (`davmail' and `java' executables).
;; - isync >= 1.5 (`mbsync') for IMAP <-> Maildir synchronization.
;; - maildir-utils >= 1.12 (`mu' plus the bundled mu4e Emacs frontend).
;; - The `davmail' GNU Stow package from ~/opt/dotfiles, which deploys the user
;;   service and gateway settings below ~/.config.
;; - The `isync' GNU Stow package from ~/opt/dotfiles, which deploys
;;   ~/.config/isyncrc and reads the local DavMail login from authinfo.
;; - Temporary credentials in ~/.authinfo with mode 0600:
;;     machine 127.0.0.1 login you@outlook.com password LOCAL_KEY port 1143
;;     machine 127.0.0.1 login you@outlook.com password LOCAL_KEY port 1025
;;   In device-code mode LOCAL_KEY encrypts the stored OAuth refresh token; it
;;   is not the Microsoft account password.  Use the same random value twice.
;;   Migrate these entries to ~/.authinfo.gpg when GnuPG is configured.
;;
;; Useful service commands:
;;   systemctl --user enable --now davmail.service
;;   mbsync outlook
;;   mu index
;;; Code:

(require 'subr-x)

(declare-function mu4e "mu4e" (&optional background))
(defvar smtpmail-smtp-user)

(defconst +email/smtp-host "127.0.0.1"
  "Loopback address of the local DavMail SMTP gateway.")

(defconst +email/smtp-port 1025
  "Port of the local DavMail SMTP gateway.")

(defun +email/configured-p ()
  "Return non-nil when the machine-local email address is configured."
  (and (stringp +emacs/email-address)
       (not (string-empty-p +emacs/email-address))))

(defun +email--setup-mu4e-load-path ()
  "Add the machine-local mu4e directory to `load-path' when configured."
  (when (and (stringp +emacs/mu4e-load-path)
             (file-directory-p +emacs/mu4e-load-path))
    (add-to-list 'load-path (directory-file-name +emacs/mu4e-load-path))))

(defun +email/apply-account-settings ()
  "Apply machine-local Outlook identity and transport settings."
  (when (+email/configured-p)
    (setq user-mail-address +emacs/email-address
          smtpmail-smtp-user +emacs/email-address)
    (when (and (stringp +emacs/email-full-name)
               (not (string-empty-p +emacs/email-full-name)))
      (setq user-full-name +emacs/email-full-name))))

(defun +email/open ()
  "Open the local Outlook Maildir with mu4e."
  (interactive)
  (unless (+email/configured-p)
    (user-error "Set +emacs/email-address in ~/.emacs.d/init.el first"))
  (unless (executable-find "mu")
    (user-error "Install maildir-utils so the mu executable is available"))
  (+email--setup-mu4e-load-path)
  (unless (locate-library "mu4e")
    (user-error "Cannot find mu4e; set +emacs/mu4e-load-path"))
  (unless (file-directory-p +emacs/email-maildir)
    (user-error "Maildir does not exist yet; run: mbsync outlook"))
  (+email/apply-account-settings)
  (mu4e))

(+email--setup-mu4e-load-path)

(use-package mu4e
  :ensure nil
  :no-require t
  :commands mu4e
  :defines (gnus-inhibit-images
            mu4e-change-filenames-when-moving
            mu4e-compose-format-flowed
            mu4e-confirm-quit
            mu4e-drafts-folder
            mu4e-get-mail-command
            mu4e-maildir-shortcuts
            mu4e-refile-folder
            mu4e-sent-folder
            mu4e-sent-messages-behavior
            mu4e-trash-folder
            mu4e-update-interval)
  :config
  (+email/apply-account-settings)
  (setq mu4e-get-mail-command "mbsync outlook"
        mu4e-update-interval 300
        mu4e-change-filenames-when-moving t
        mu4e-sent-messages-behavior 'delete
        mu4e-drafts-folder "/草稿"
        mu4e-refile-folder "/存档"
        mu4e-sent-folder "/已发送邮件"
        mu4e-trash-folder "/已删除邮件"
        mu4e-maildir-shortcuts
        '((:maildir "/INBOX" :key ?i)
          (:maildir "/已发送邮件" :key ?s)
          (:maildir "/草稿" :key ?d)
          (:maildir "/存档" :key ?a)
          (:maildir "/已删除邮件" :key ?t))
        mu4e-compose-format-flowed t
        mu4e-confirm-quit nil
        ;; Do not fetch tracking images while rendering otherwise-local mail.
        gnus-inhibit-images t))

(use-package message
  :ensure nil
  :defer t
  :init
  (setq mail-user-agent 'mu4e-user-agent
        message-send-mail-function #'smtpmail-send-it
        message-kill-buffer-on-exit t))

(use-package smtpmail
  :ensure nil
  :defer t
  :init
  (setq smtpmail-smtp-server +email/smtp-host
        smtpmail-smtp-service +email/smtp-port
        smtpmail-stream-type 'plain)
  (+email/apply-account-settings))

(provide 'init-email)
;;; email.el ends here
