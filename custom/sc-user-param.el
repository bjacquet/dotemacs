;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2003, SISCOG - Sistemas Cognitivos Lda.
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
;;;	Example of typical user parameters and definitions.
;;;	
;;; History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	05/09/16	A. Frazao	Added
;;;					  user-full-name
;;;					  user-mail-address
;;;	05/10/19	J. P. Varandas	Deleted definitions
;;;					  (SETQ *CREWS-MAIL-VERSION-NAME*)
;;;					  (SETQ *CREWS-MAIL-VERSIONS*)
;;;					  (SETQ *CREWS-REVERTED-MAIL*)
;;;					Added definitions
;;;					  (SETQ *DIGEST-MAIL-NAME*)
;;;					Changed definitions
;;;					  (SETQ *SC-SISCOG-TOP-DIR*)
;;;					  (SETQ *CREWS-MAIL-FROM*)
;;;					  (SETQ USER-FULL-NAME)
;;;					  (SETQ USER-MAIL-ADDRESS)
;;;					  SET-LISP-DOC-HOME
;;;	06/04/06	J. P. Varandas	Changed definitions
;;;					  SC-ACL-VERSION-VALUES
;;;	06/04/12	J. P. Varandas	Set variable *NEW-ODBC-NAMES* to NIL
;;;	07/04/13	MCoutinho	Changed definitions
;;;					  SC-ACL-VERSION-VALUES
;;;	07/08/13	RAurelio	Changed definitions
;;;					  (SETQ *SC-CREWS-LOCAL-TOP-DIR*)
;;;	07/12/31	J. P. Varandas	Deleted definitions
;;;					  SC-ACL-VERSION-VALUES
;;;					  (SETQ ALLEGRO-COMMON-LISP-IMAGE-NAME)
;;;					  (SETQ ALLEGRO-COMMON-LISP-IMAGE-FILE)
;;;					  (SETENV "ALLEGRO_CL_HOME")
;;;	09/01/13	J. P. Varandas	Deleted definitions
;;;					  (SETQ *NEW-ODBC-NAMES*)
;;;	09/02/04	J. P. Varandas	Deleted definitions
;;;					  SC-GET-SYSTEM-VERSION-DIR
;;;	09/02/06	Rui Patrocinio	Changed definitions
;;;					  (set-lisp-doc-home "file://mouraria/lisp")
;;;	09/02/10	J. P. Varandas	Changed variable names
;;;					  *CREWS-MOD-FILE* -> *MODIF-REQUEST-FILE*
;;;					  *CREWS-BIG-BROTHER-FILE* -> *ACTIONS-LOG-FILE*
;;;					  *CREWS-MAIL-DIR* -> *MODIFS-MAIL-DIR*
;;;					  *CREWS-MAIL-FROM* -> *MODIF-MAIL-FROM*
;;;					  *CREWS-MAIL-NAME* -> *MODIF-MAIL-NAME*
;;;					  *SC-SISCOG-TOP-DIR* -> *SC-CENTRAL-REPOSITORY-DIR*
;;;					  *SC-CREWS-LOCAL-TOP-DIR* -> *SC-BINARY-REPOSITORY-DIR*
;;;					  *SC-CREWS-TOP-DIR* -> *SC-LOCAL-REPOSITORY-DIR*
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;; Local CREWS top directory (see sc-models.el)
;;;-----------------------------------------------------------------------------
(setq *sc-local-repository-dir*  "z:/siscog")

;;;-----------------------------------------------------------------------------
;;; Local CREWS top directory for binaries and patches
;;; (see 'sc-models.el' and 'sc-run-crews-images.el' (definition 'sc-select-appl-version-dir'))
;;;-----------------------------------------------------------------------------
(setq *sc-binary-repository-dir* "y:/siscog")

;;;-----------------------------------------------------------------------------
;;; SISCOG area CREWS top directory (see sc-models.el)
;;;-----------------------------------------------------------------------------
(setq *sc-central-repository-dir* "x:/siscog")

;;;-----------------------------------------------------------------------------
;;; Modifications management (see sc-mod.el)
;;;-----------------------------------------------------------------------------
(setq *default-mod-date*    "09/10/06")
(setq *default-author*      "B Jacquet")
(setq *default-org-dir*     *sc-central-repository-dir*)
(setq *default-src-dir*     (getenv "SISCOG_DIR"))
(setq *default-mod-dir*     "y:/siscog/modifications")
(setq *modif-request-file*  (format "%s/modif-requests" *default-mod-dir*))
(setq *actions-log-file*    (format "%s/.big-brother-file" *default-mod-dir*))
(setq *modifs-mail-dir*     "y:/siscog/modifications/emails")
(setq *modif-mail-from*     "bjacquet@siscog.pt")
(setq *modif-mail-name*     "modif-request-mail")
(setq *mod-mail-method*     :mine)
(setf *digest-mail-name*    "y:/mail/modifs")
(setq *emacs-init-file*     (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))
(setq *emacs-program*       "emacs")
(setq *default-acl-version* :v9-0-64)

;;;-----------------------------------------------------------------------------
;;; Modifications mail (see sc-mail.el)
;;;-----------------------------------------------------------------------------
(setq user-full-name "Bruno Jacquet")
(setq user-mail-address "bjacquet@siscog.pt")
