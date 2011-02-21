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
;;; System versions (ver sc-models.el and sc-crews-global-param.el)
;;;-----------------------------------------------------------------------------
;;; These are only examples. Don't need to paste it on the custom file.
(sc-add-system-version "CREWS-SISCOG" "v6-8-0-dispatcher")
(sc-add-system-application "CREWS-STOG" "dispatcher")
(sc-add-system-db-user "CREWS-NSB" '("nsb008database" "testensb"))

;;;-----------------------------------------------------------------------------
;;; Local CREWS top directory (ver sc-models.el)
;;;-----------------------------------------------------------------------------
(setq *sc-local-repository-dir*  "z:/siscog")

;;;-----------------------------------------------------------------------------
;;; Local CREWS top directory for binaries and patches
;;; (see 'sc-models.el' and 'sc-run-crews-images.el' (definition 'sc-select-appl-version-dir'))
;;;-----------------------------------------------------------------------------
(setq *sc-binary-repository-dir* "y:/siscog")

;;;-----------------------------------------------------------------------------
;;; SISCOG area CREWS top directory (ver sc-models.el)
;;;-----------------------------------------------------------------------------
(setq *sc-central-repository-dir* "x:/siscog")

;;;-----------------------------------------------------------------------------
;;; CREWS Applications extra menu items (ver sc-run-crews-images.el)
;;;-----------------------------------------------------------------------------
;;; This is only an example. Don't need to paste it on the custom file.
(defun sc-crews-custom-options-menu-items ()
  (list (sc-make-menu-item "Set Default Patches" '(sc-set-crews-patches-dir nil))
	(sc-make-menu-item "Set NS Patches"  '(sc-set-crews-patches-dir "v:/v3-1-0/patches/crews-ns"))
	(sc-make-menu-item "Set NSB Patches" '(sc-set-crews-patches-dir "u:/v1-1-0/patches/crews-nsb"))
	(sc-make-menu-item "Set CP Patches" '(sc-set-crews-patches-dir "w:/v2-0-0/patches/crews-cp"))
	""
	(sc-make-menu-item "Set Default Data (Local)" '(sc-set-crews-data-dir "z:/siscog/data"))
	(sc-make-menu-item "Set FAUSTO Data (Pavia)" '(sc-set-crews-data-dir "p:/siscog-data"))
	(sc-make-menu-item "Set JPV Data (Ourique)" '(sc-set-crews-data-dir "q:/siscog-data"))
	(sc-make-menu-item "Set VR Data (Tejo)"  '(sc-set-crews-data-dir "r:/datasets"))))


;;;-----------------------------------------------------------------------------
;;; Modifications management (ver sc-mod.el)
;;;-----------------------------------------------------------------------------
(setq *default-mod-date*   "01/12/31")
(setq *default-author*     "My Name")
(setq *default-org-dir*    *sc-central-repository-dir*)
(setq *default-src-dir*    (getenv "SISCOG_DIR"))
(setq *default-mod-dir*    "z:/siscog/alteracoes")
(setq *modif-request-file* (format "%s/.modif-requests" *default-mod-dir*))
(setq *actions-log-file*   (format "%s/.big-brother-file" *default-mod-dir*)) ;; NIL para nao informar o BIG-BROTHER...
(setq *modifs-mail-dir*    "y:/mail")
(setq *modif-mail-from*    "my-siscog-address@siscog.pt")
(setq *modif-mail-name*    "modif-request-mail")
(setf *digest-mail-name*   "y:/mail/modifs")

(setq *emacs-init-file*    (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))
(setq *emacs-program*      "emacs")

;;;-----------------------------------------------------------------------------
;;; Modifications mail (ver sc-mail.el)
;;;-----------------------------------------------------------------------------
(setq user-full-name "My Complete Name")
(setq user-mail-address "my-siscog-address@siscog.pt")

;;;-----------------------------------------------------------------------------
;;; Lisp Documentation (ver cl-hyperspec/online-doc.el)
;;;-----------------------------------------------------------------------------
(set-lisp-doc-home "file://mouraria/lisp")

;;;-----------------------------------------------------------------------------
;;; Grepfc (ver sc-grepfc.el)
;;;-----------------------------------------------------------------------------
(setq *grepfc-top-dir* "z:/siscog")
(setq *grepfc-names* '("crews" "cp" "ns" "dsb" "nsb" "wagn" "vr" "ml" "stog"))
(global-set-key [f2] 'grepfc-goto-line)
(global-set-key [C-f2] 'grepfc-goto-patch-log)
(global-set-key [M-f2] 'grepfc-goto-patch-data)
