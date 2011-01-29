(setq mew-name "Bruno Jacquet")
(setq mew-user "bruno.jacquet")           ; from name@domain
(setq mew-mail-domain "gmail.com")

;; smtp
(setq mew-smtp-user "bruno.jacquet@gmail.com"
      mew-smtp-server "smtp.gmail.com"
      mew-smtp-auth t
      mew-smtp-ssl t
      mew-smtp-ssl-port "465")

;; i use fetchmail to get mail from all boxes to /var/mail/banan
;; (setq mew-mailbox-type 'mbox)
;; (setq mew-mbox-command "incm")
;; (setq mew-mbox-command-arg "-u -d /var/mail/username")
;; (setf mew-mail-path "~/.mail")

;; imap
(setq mew-proto "%"
      mew-imap-server "imap.gmail.com"
      mew-imap-ssl t
      mew-imap-ssl-port 993
      mew-imap-user mew-user
      mew-imap-trash-folder "%[Google Mail]/bin")

;; some customization
(setq mew-use-unread-mark t
      mew-use-full-window t
      mew-use-cached-passwd t)
(setq mew-summary-form '(type " " (5 date) "/" (4 year) " " (5 time) "  " (30 from) " " t (0 subj)))
(setq mew-summary-form-extract-rule '(nickname address))
;; make mew to show date in "dd/mm" format in summary instead of "mm/dd"
(defun mew-summary-form-date ()
  "A function to return a date, DD/MM/YYYY."
  (let ((s (MEW-DATE)))
    (when (or (string= s "")
 	      (not (string-match mew-time-rfc-regex s)))
      (setq s (mew-time-ctz-to-rfc
 	       (mew-file-get-time (mew-expand-msg (MEW-FLD) (MEW-NUM))))))
    (if (string-match mew-time-rfc-regex s)
 	(format "%02d/%02d"
 		(mew-time-rfc-day)
 		(mew-time-mon-str-to-int (mew-time-rfc-mon)))
      "")))
