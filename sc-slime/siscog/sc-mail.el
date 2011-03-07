;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2005, SISCOG - Sistemas Cognitivos Lda.
;;;                           All rights reserved
;;;
;;;-----------------------------------------------------------------------------
;;;
;;;                         RESTRICTED RIGHTS LEGEND
;;;
;;;-----------------------------------------------------------------------------
;;;
;;;     Use, duplication or disclosure is subject to authorization by
;;;
;;;                 SISCOG - Sistemas Cognitivos Lda.
;;;                      Campo Grande 378, 3º
;;;                        1700-097 LISBOA
;;;                           PORTUGAL
;;;
;;;-----------------------------------------------------------------------------
;;; Description
;;;	SISCOG's main file for the mail tool used by the Change Request Manager
;;;	in EMACS.
;;;	
;;;	For the mail tool to work correctly, the following variables must be
;;;	set:
;;;	
;;;	user-full-name, 
;;;	user-mail-address
;;;
;;;	These are best set as customisation variables, using the
;;;	command `customize-variable'.
;;;	
;;; History
;;;	Date		Author		Description
;;;	05/08/05	Fernando	Added definitions
;;;					  SC-MAIL-DIR
;;;					  GLOBAL-MAP
;;;					  (SETF MESSAGE-SEND-MAIL-PARTIALLY-LIMIT)
;;;					  MAIL-STRIP-QUOTED-NAMES
;;;					  MOD-MESSAGE-SEND.HANDLER
;;;					  MML-ATTACH-MOD.BUFFER
;;;					  MOD-MESSAGE-SEND
;;;					  GET-MOD-MAIL-SEND-TO
;;;					  GET-MOD-MAIL-HEADER
;;;					  GET-MOD-MAIL-SUBJECT
;;;					  GNUS-SETUP-POSTING-CHARSET
;;;					  POSTING-CHARSET-ALIST
;;;					  GNUS-DEFINE-GROUP-PARAMETER
;;;					  GNUS-GROUP-PARAMETERS-MORE
;;;					  (SETQ MESSAGE-SEND-MAIL-FUNCTION)
;;;					  MESSAGE-AUTO-SAVE-DIRECTORY
;;;					  "MAIL-PRSVR"
;;;					  (SETQ SEND-MAIL-FUNCTION)
;;;					  (SETQ SMTPMAIL-LOCAL-DOMAIN)
;;;					  (SETQ SMTPMAIL-DEFAULT-SMTP-SERVER)
;;;					  SEND.MOD.MAIL.WITH.CC
;;;	05/09/14	A. Frazao	Moved definitions
;;;					  GLOBAL-MAP
;;;					  GET-MOD-MAIL-SUBJECT
;;;					  GET-MOD-MAIL-HEADER
;;;					  GET-MOD-MAIL-SEND-TO
;;;					Deleted definitions
;;;					  MOD-MESSAGE-SEND.HANDLER
;;;					Changed definitions
;;;					  SC-MAIL-SEND-BUFFER
;;;					  MML-ATTACH-BUFFER
;;;	05/09/16	A. Frazao	Changed definitions
;;;					  SC-MAIL-SEND-BUFFER
;;;					Deleted definitions
;;;					  SEND.MOD.MAIL.WITH.CC
;;;	05/10/17	Fernando	Changed definitions
;;;					  MML-ATTACH-BUFFER
;;;	06/02/16	Fernando	Changed definitions
;;;					  (SETQ SMTPMAIL-DEFAULT-SMTP-SERVER)
;;;	06/04/11	J. P. Varandas	Deleted definitions
;;;					  SC-MAIL-DIR
;;;	06/05/29	A. Frazao	Changed definitions
;;;					  SC-MAIL-SEND-BUFFER
;;;	08/10/09	Tom Weissmann	Changed definitions
;;;					  SC-ATTACH-BUFFER
;;;					  SC-MAIL-SEND-BUFFER
;;;					Added autoloads
;;;					  MML
;;;					  MESSAGE
;;;					Deleted definitions
;;;					  (SETQ SEND-MAIL-FUNCTION)
;;;					  "MAIL-PRSVR.EL"
;;;					  MESSAGE-AUTO-SAVE-DIRECTORY
;;;					  GNUS-GROUP-PARAMETERS-MORE
;;;					  GNUS-DEFINE-GROUP-PARAMETER
;;;					  POSTING-CHARSET-ALIST
;;;					  GNUS-SETUP-POSTING-CHARSET
;;;					  MAIL-STRIP-QUOTED-NAMES
;;;	08/11/13	Tom Weissmann	Changed definitions
;;;					  SC-MAIL-SEND-BUFFER
;;;	08/12/26	MCoutinho	Changed definitions
;;;					  (SETQ SMTPMAIL-DEFAULT-SMTP-SERVER)
;;;-----------------------------------------------------------------------------


;;; Parameters
;;; =============================================================================

;;;-----------------------------------------------------------------------------
;;;Autoload MML
;;;Description
;;;	Only load the mml library when necessary	
;;;		
;;;History
;;;	Date		Author		Description
;;;	08/10/09	Tom Weissmann	Created
;;;-----------------------------------------------------------------------------
(autoload 'mml-insert-empty-tag "mml")

;;;-----------------------------------------------------------------------------
;;;'MESSAGE-SEND
;;;Description
;;;	Only load the message library when necessary
;;;
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	08/10/09	Tom Weissmann	Created
;;;-----------------------------------------------------------------------------
(autoload 'message-send "message")

;;;-----------------------------------------------------------------------------
;;;(SETQ SMTPMAIL-DEFAULT-SMTP-SERVER)
;;;Description
;;;	Default mail server assignment. See the header of this file or go to
;;;	http://www.gnu.org/software/emacs/windows/faq8.html
;;;	
;;;History
;;;	Date		Author		Description
;;;	05/08/05	Fernando	Created
;;;	06/02/16	Fernando	mail.siscog.com -> alfama.siscog.com
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;-----------------------------------------------------------------------------
(setq smtpmail-default-smtp-server "mouraria.siscog.com")

;;;-----------------------------------------------------------------------------
;;;(SETQ SMTPMAIL-LOCAL-DOMAIN)
;;;Description
;;;	See the header of this file or go to
;;;	http://www.gnu.org/software/emacs/windows/faq8.html
;;;	
;;;History
;;;	Date		Author		Description
;;;	05/08/05	Fernando	Created
;;;-----------------------------------------------------------------------------
(setq smtpmail-local-domain nil)


;;;-----------------------------------------------------------------------------
;;;(SETQ MESSAGE-SEND-MAIL-FUNCTION)
;;;Description
;;;	Default function to send mail. See the header of this file or go to
;;;	http://www.gnu.org/software/emacs/windows/faq8.html
;;;	
;;;History
;;;	Date		Author		Description
;;;	05/08/05	Fernando	Created
;;;-----------------------------------------------------------------------------
(setq message-send-mail-function 'smtpmail-send-it)

;;;-----------------------------------------------------------------------------
;;;(SETF MESSAGE-SEND-MAIL-PARTIALLY-LIMIT)
;;;Description
;;;	This parameter controls the breaking of "big" messages. We do not want 
;;;	the modification mails broken in several pieces, so we assign nil to 
;;;	this parameter.
;;;	
;;;History
;;;	Date		Author		Description
;;;	05/08/05	Fernando	Created
;;;-----------------------------------------------------------------------------
(setf message-send-mail-partially-limit nil)


;;; Functions
;;; =============================================================================


;;;-----------------------------------------------------------------------------
;;;SC-ATTACH-BUFFER
;;;Description
;;;	Attaches the modification mail file to the message currently being 
;;;	composed.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{buffer} is a buffer
;;;		
;;;	\return-types
;;;		void.
;;;
;;;History
;;;	Date		Author		Description
;;;	05/08/05	Fernando	Created
;;;	05/09/14	A. Frazao	Changed name. Added argument buffer
;;;	05/10/17	Fernando	Added field 'filename to work with Netscape
;;;					  like mail clients
;;;	08/10/09	Tom Weissmann	Use a `let', changed name from `mml-attach-buffer'
;;;					No longer interactive.
;;;-----------------------------------------------------------------------------
(defun sc-attach-buffer (buffer &optional filename type description)
  "Attach a buffer to the outgoing MIME message, with a filename."
  (let ((buffername (buffer-name buffer)))
    (mml-insert-empty-tag 'part 
			  'type (or type "raw")
			  'buffer buffername
			  'filename (or filename buffername)
			  'disposition "attachment" 
			  'description (or description buffername))))

;;;-----------------------------------------------------------------------------
;;;SC-MAIL-SEND-BUFFER
;;;Description
;;;	Composes the modification message, attaches it the modification buffer and
;;;	sends the e-mail according to the specifications in the modification mail 
;;;	file and the global mail parameters.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{tos} is a list of \emph{string}, the destination mail addresses,
;;;		e.g., ("john@siscog.pt").
;;;		It must have at least one address.
;;;
;;;		\arg{ccs} is a list of \emph{string}, the cc mail addresses
;;;
;;;		\arg{subject} is a \emph{string}, the subject of the message.
;;;
;;;		\arg{text} is a \emph{string}, the text of the message.
;;;
;;;		\arg{buffer} is a \emph{buffer}.
;;;		
;;;	\return-types
;;;		A \emph{boolean}, \emph{true} if the message was sent or \emph{nil}
;;;		otherwise.
;;;
;;;	\remarks
;;;		Encoding:
;;;
;;;		The mail file is sent as an attachment of another email,
;;;		instead of sending an email with the modification files as attachments.
;;;		It's also sent using the "raw" MIME type.
;;;		Emacs versions with Unicode support (>= v22) will convert a raw attachment
;;;		to use unibyte.
;;;
;;;		When multibyte support is enabled, Emacs will internally represent accents etc.
;;;		as multibyte. This even happens when loading a file in which such
;;;		characters are represented with just one byte (eg, a file in latin-1 encoding).
;;;		
;;;		When GNUs converts the attachment to unibyte, our accents end up occupying
;;;		two bytes instead of one, and CRI breaks when it receives the resulting email.
;;;
;;;		Message mode:
;;;
;;;		`message-send' expects to be called in the context of message mode, but
;;;		we don't use message mode because it's overkill: it creates a Mail
;;;		directory and saves draft messages which we have no use for, it asks
;;;		for the user for confirmation when the temporary mail buffer is about
;;;		to be killed, etc.
;;;
;;;		One effect of not using message mode is that `message-send' sets the global
;;;		value of variables that would be buffer-local if we were using message mode.
;;;
;;;		We wouldn't need to know the internal details of message mode if we used it,
;;;		but since we aren't, we do need to know some details. At present, the only
;;;		variable we know has to be set buffer-local is `message-sent-message-via' -
;;;		otherwise the user is prompted for confirmation when trying to send more
;;;		than one mod mail from the same Emacs session
;;;		(and in some cases the third fails even when the user confirms).
;;;
;;;History
;;;	Date		Author		Description
;;;	05/08/05	Fernando	Created
;;;	05/09/14	A. Frazao	Changed name
;;;	05/09/16	A. Frazao	Receives list of CCs
;;;	06/05/29	A. Frazao	Returns a boolean
;;;	08/10/09	Tom Weissmann  	Use `string-join', `with-temp-buffer'.
;;;					Documented uni/multibyte issue.
;;;	08/11/13	Tom Weissmann	Set `mail-sent-message-via' nil in the mail
;;;					buffer.
;;;-----------------------------------------------------------------------------
(defun sc-mail-send-buffer (tos ccs subject text buffer)
  "Compose a modification email to TOS, cc-ing CCS, subject line SUBJECT,
body TEXT, with attached file BUFFER. And send it."
  (when (with-temp-buffer
          (insert
           "To: "         (string-join "," tos) "\n"
           "Cc: "         (string-join "," ccs) "\n"
           "Subject: "    subject               "\n"
           mail-header-separator                "\n"
           text                                 "\n")
          (let ((mail-buffer (current-buffer)))
            ;; Ensure `mail-sent-message-via' is buffer-local and nil, to avoid
            ;; a confirmation message if more than one mod mail is sent from the same
            ;; Emacs session.
            (set (make-local-variable 'message-sent-message-via) nil)
            ;; in another temporary buffer, disable multibyte and insert the contents of
            ;; buffer: this ensures a unibyte buffer (reverting the original buffer
            ;; would just switch it back to multibyte...)
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-buffer buffer)
              (let ((attachment-buffer (current-buffer)))
                ;; back to the first temporary buffer to attach the attachment and send
                ;; the mail
                (with-current-buffer mail-buffer
                  (sc-attach-buffer attachment-buffer (buffer-name buffer))
                  (ignore-errors (message-send)))))))
    t))

















