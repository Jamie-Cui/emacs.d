;;; init-email.el --- email and mu4e configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(defgroup +email nil
  "Personal email configuration."
  :group 'applications)

(defcustom +email/outlook-address (getenv "OUTLOOK_EMAIL")
  "Microsoft personal account email address used by mu4e."
  :type '(choice (const :tag "Unset" nil) string)
  :group '+email)

(defcustom +email/outlook-full-name (or (getenv "OUTLOOK_FULL_NAME") user-full-name)
  "Display name used when sending mail from the Outlook account."
  :type 'string
  :group '+email)

(defcustom +email/maildir (expand-file-name "~/.maildir")
  "Root Maildir directory used by mu4e."
  :type 'directory
  :group '+email)

(defcustom +email/outlook-mu4e-prefix "/outlook"
  "mu4e maildir prefix for the Outlook account."
  :type 'string
  :group '+email)

(defcustom +email/outlook-sync-command "mbsync outlook"
  "Command used by mu4e to fetch Outlook mail."
  :type 'string
  :group '+email)

(defcustom +email/outlook-inbox-folder-name "INBOX"
  "Outlook inbox folder name as seen by mbsync."
  :type 'string
  :group '+email)

(defcustom +email/outlook-drafts-folder-name "Drafts"
  "Outlook drafts folder name as seen by mbsync."
  :type 'string
  :group '+email)

(defcustom +email/outlook-sent-folder-name "Sent Items"
  "Outlook sent folder name as seen by mbsync."
  :type 'string
  :group '+email)

(defcustom +email/outlook-trash-folder-name "Deleted Items"
  "Outlook trash folder name as seen by mbsync."
  :type 'string
  :group '+email)

(defcustom +email/outlook-refile-folder-name "Archive"
  "Outlook archive folder name as seen by mbsync."
  :type 'string
  :group '+email)

(defcustom +email/mu4e-update-interval 300
  "Seconds between automatic mu4e mail updates."
  :type 'integer
  :group '+email)

(defun +email/outlook-configured-p ()
  "Return non-nil when the Outlook account identity is configured."
  (and (stringp +email/outlook-address)
       (not (string-empty-p +email/outlook-address))))

(defun +email/outlook-folder (folder)
  "Return mu4e folder path for Outlook FOLDER."
  (concat (string-remove-suffix "/" +email/outlook-mu4e-prefix)
          "/"
          folder))

(defun +email/add-mu4e-load-paths ()
  "Add common mu4e install directories to `load-path'."
  (dolist (dir '("/usr/share/emacs/site-lisp/mu4e"
                 "/usr/local/share/emacs/site-lisp/mu4e"
                 "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
                 "/usr/local/share/emacs/site-lisp/mu/mu4e"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(+email/add-mu4e-load-paths)

(declare-function make-mu4e-context "mu4e-context")
(declare-function mu4e-message-field "mu4e")

(use-package mu4e
  :ensure nil
  :commands (mu4e mu4e-compose-new)
  :init
  (setq mail-user-agent 'mu4e-user-agent)
  :custom
  (mu4e-maildir +email/maildir)
  (mu4e-get-mail-command +email/outlook-sync-command)
  (mu4e-update-interval +email/mu4e-update-interval)
  (mu4e-change-filenames-when-moving t)
  (mu4e-attachment-dir "~/Downloads")
  ;; Outlook.com normally stores sent mail itself; avoid duplicate sent copies.
  (mu4e-sent-messages-behavior 'delete)
  :config
  (setq message-send-mail-function #'message-send-mail-with-sendmail
        send-mail-function #'sendmail-send-it
        sendmail-program "msmtp"
        message-sendmail-f-is-evil t
        message-sendmail-envelope-from 'header
        message-sendmail-extra-arguments '("--read-envelope-from"))

  (setq mu4e-drafts-folder (+email/outlook-folder +email/outlook-drafts-folder-name)
        mu4e-sent-folder (+email/outlook-folder +email/outlook-sent-folder-name)
        mu4e-trash-folder (+email/outlook-folder +email/outlook-trash-folder-name)
        mu4e-refile-folder (+email/outlook-folder +email/outlook-refile-folder-name))

  (when (+email/outlook-configured-p)
    (setq user-mail-address +email/outlook-address
          user-full-name +email/outlook-full-name)
    (setq mu4e-context-policy 'pick-first
          mu4e-compose-context-policy 'always-ask
          mu4e-contexts
          (list
           (make-mu4e-context
            :name "Outlook"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p
                 +email/outlook-mu4e-prefix
                 (or (mu4e-message-field msg :maildir) ""))))
            :vars
            `((user-mail-address . ,+email/outlook-address)
              (user-full-name . ,+email/outlook-full-name)
              (mu4e-drafts-folder . ,(+email/outlook-folder +email/outlook-drafts-folder-name))
              (mu4e-sent-folder . ,(+email/outlook-folder +email/outlook-sent-folder-name))
              (mu4e-trash-folder . ,(+email/outlook-folder +email/outlook-trash-folder-name))
              (mu4e-refile-folder . ,(+email/outlook-folder +email/outlook-refile-folder-name))))))))

(with-eval-after-load 'general
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   :prefix my-leader
   "E" #'mu4e))

(provide 'init-email)
