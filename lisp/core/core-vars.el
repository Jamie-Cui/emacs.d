;;; core-vars.el --- User-tunable variables -*- lexical-binding: t -*-
;;; Commentary:
;; Defines the `+emacs' customization group and the small set of user-tunable
;; variables shared across the configuration.  Machine-local values belong in
;; the installed `~/.emacs.d/init.el', not here.
;;; Code:

(defgroup +emacs nil
  "Personal Emacs configuration."
  :group 'convenience
  :prefix "+emacs/")

(defcustom +emacs/repo-directory (expand-file-name "~/.emacs.d")
  "Path to the emacs.d configuration repository.
Normally set by the installed `~/.emacs.d/init.el' or derived from the
repository's own `init.el' location."
  :type 'directory
  :group '+emacs)

(defcustom +emacs/org-root-dir (expand-file-name "~/opt/org-root")
  "Path to the org-root folder."
  :type 'directory
  :group '+emacs)

(defcustom +emacs/proxy "127.0.0.1:10808"
  "HTTP/HTTPS proxy host:port used for URL access."
  :type 'string
  :group '+emacs)

(defcustom +emacs/email-address nil
  "Primary email address used by the email module.
Set this in the installed `~/.emacs.d/init.el'; keep account details out of
the shared configuration."
  :type '(choice (const :tag "Not configured" nil) string)
  :group '+emacs)

(defcustom +emacs/email-full-name nil
  "Full name used in messages sent by the email module.
Set this in the installed `~/.emacs.d/init.el'."
  :type '(choice (const :tag "Not configured" nil) string)
  :group '+emacs)

(defcustom +emacs/email-maildir
  (expand-file-name "mail/outlook" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Local Maildir root used by mbsync and mu4e.
Set this in the installed `~/.emacs.d/init.el' when the machine uses a
different local mail location."
  :type 'directory
  :group '+emacs)

(defcustom +emacs/mu4e-load-path nil
  "Optional directory containing the system-installed mu4e Lisp files.
Set this in the installed `~/.emacs.d/init.el' when mu4e is installed outside
Emacs' default `load-path'."
  :type '(choice (const :tag "Use default load-path" nil) directory)
  :group '+emacs)

(defcustom +emacs/disabled-modules nil
  "Modules to skip when loading the manifest.
Each entry is a manifest module name string, e.g. \"llm\" or
\"lang/cmake\".  The loader performs no dependency inference, so disabling
a module others rely on may break them."
  :type '(repeat string)
  :group '+emacs)

(provide 'core-vars)
;;; core-vars.el ends here
