;; example configuration for mu4e

;; make sure mu4e is in your load-path
;; the exact path may differ --- check it
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-maildir "~/Documents/Maildir")

(setq mu4e-mu-binary "/usr/local/bin/mu")

(setq mu4e-get-mail-command "offlineimap")

;; these must start with a "/", and must exist
;; (i.e.. /home/user/Maildir/sent must exist)
;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
;; already exist

;; below are the defaults; if they do not exist yet, mu4e offers to
;; create them. they can also functions; see their docstrings.
(setq mu4e-sent-folder   "/INBOX.Sent"
      mu4e-drafts-folder "/INBOX.Drafts"
      mu4e-trash-folder  "/INBOX.Trash")

;; smtp mail setting; these are the same that `gnus' uses.
(setq
 message-send-mail-function   'smtpmail-send-it
 smtpmail-default-smtp-server "mail.runtime-revolution.com"
 smtpmail-smtp-server         "mail.runtime-revolution.com"
 smtpmail-local-domain        "runtime-revolution.com")

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; save attachment to my desktop (this can also be a function)
(setq mu4e-attachment-dir "~/Desktop")

;; attempt to show images when viewing messages
(setq mu4e-view-show-images t)

;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses t)

(setq mu4e-confirm-quit nil)
