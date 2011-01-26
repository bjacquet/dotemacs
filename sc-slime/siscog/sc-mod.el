;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 1995, SISCOG - Sistemas Cognitivos Lda.
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
;;;                      Campo Grande 30, 6 B
;;;                           1700 LISBOA
;;;                             PORTUGAL
;;;
;;;-----------------------------------------------------------------------------
;;; This file contains an Emacs tool to handle modification requests, helping the
;;; user to organize them.
;;; The Emacs cl, cl-macs and cl-extra modules must be loaded prior to this file
;;; Example:
;;;	Insert the following commands in the emacs init file
;;;	(load "cl")
;;;	(load "cl-macs")
;;;	(load "cl-extra")
;;;	(load (expand-file-name "~/emacs-changes"))
;;;	where this file has the name 'emacs-changes.el' and is located at the
;;;	home directory.
;;;
;;; A modification is consists on the following:
;;;	Name			Is the label of the modification. It must be
;;;				unique in all modifications.
;;;	Author			Names the author of the modification. It is used
;;;				to fill the headers.
;;;				(variable *DEFAULT-AUTHOR*)
;;;	Date			It is a fixed date and is used to fill the
;;;				headers. The user can identify the changes made
;;;				in a file by searching for this date. When
;;;				creating the mail message, the system will
;;;				replace this date by the current date in all the
;;;				modification files.
;;;				(variable *DEFAULT-MOD-DATE*)
;;;	Original Directory	Is the products directory where the application
;;;				files are installed. By default is /home/siscog.
;;;				(variable *DEFAULT-ORG-DIR*)
;;;	Source Directory	Is the products directory where the source files
;;;				are modified. This directory belongs to the
;;;				user.
;;;				(variable *DEFAULT-SRC-DIR*)
;;;	Changes Directory	Is the directory where the modified files are
;;;				stored (when backed up). When creating the mail
;;;				message, the files attached are those in this
;;;				directory.
;;;				(variable *DEFAULT-MOD-DIR*)
;;;	Files			Containts the structures that identify the files
;;;				that belong to the modification.
;;;	Patch			Optional patch file to be attached to the
;;;				modification
;;; 
;;; 
;;; Menus (Shift-Mouse-Left)
;;; 
;;;-----------------------------------------------------------------------------
;;; GENERIC
;;; This menu handles generic operations over the modifications
;;;
;;; CHANGE MOD: <Modification name>
;;; This menu appears only if the Current Modification is set.
;;; Allows to modify some of the Current Modification parameters.
;;;
;;; OPERATE MOD: <Modification name>
;;; This menu appears only if the Current Modification is set.
;;;
;;; FILE: <Buffer file name>
;;; This menu appears only if the Current Modification is set and the current
;;; buffer file is a Lisp file (ends with '.lisp' '.cl' '.bil' or '.dic').
;;; It allows the operations in the current buffer file.
;;; 
;;; <Definition expression>
;;; This menu appears only if the Current Modification is set, the current
;;; buffer file is a Lisp file (ends with '.lisp' '.cl' '.bil' or '.dic') and the 
;;; system identifies a lisp definition. The definition expression (the one that 
;;; is used in the headers) appears as the title of the menu.
;;; Four different menus can appeared due to the context of the definition.
;;; 
;;;-----------------------------------------------------------------------------
;;; History
;;;	95/10/12	A. Frazao
;;;	95/10/25	A. Frazao
;;;	95/11/07	A. Frazao
;;;	96/01/04	A. Frazao	Added check of the modification files.
;;;					When changing a definition, marks the file
;;;					changed in the current modification (if not
;;;					already marked).
;;;	96/01/09	A. Frazao	Corrected the checking of modifications
;;;					Added the handling of DEFSTRUCT
;;;					Confirmations are done through a menu.
;;;					Changed
;;;					  get-buffer-signature
;;;					  check-src-file
;;;					  check-mod-file
;;;					  make-mail-file
;;;					  get-lisp-definition-id
;;;					  mod-operations
;;;					  check-all-mods-source-file
;;;					  backup-modified-files  -> copy-source-modification-files
;;;					  install-modified-files -> copy-modification-source-files
;;;					  install-original-files -> copy-original-source-files
;;;					  beep-confirm -> x-beep-confirm
;;;					Added
;;;					  get-defstruct-name
;;;	96/04/29	A. Frazao	Added
;;;					  DIC-MODE
;;;					  add-header-change-entry
;;;					Changed
;;;					  file-mode
;;;	96/07/01	Joao Filipe	Added definitions
;;;    					  INFORM-BIG-BROTHER
;;;    					  CHECK-SIGNATURES
;;;    					  MY-CURRENT-TIME-STRING
;;;    					Changed definitions
;;;    					  DELETE-CREWS-MODIFICATION
;;;    					  GET-FILE-SIGNATURE
;;;    					  GET-BUFFER-SIGNATURE
;;;    					  MAKE-CREWS-MODIFICATION
;;;    					  MAKE-MAIL-FILE
;;;    					  SET-DEFINITION-ADDED
;;;    					  SET-DEFINITION-CHANGED
;;;    					  SET-DEFINITION-DELETED
;;;    					  CHECK-MOD-FILE
;;;    					  CHECK-SRC-FILE
;;;	96/07/12	Joao Filipe	Changed definitions
;;;					  CHECK-SIGNATURES
;;;					Added definitions
;;;					  RAC
;;; 	96/08/12	A. Frazao	Changed definitions
;;;					  EDIT-MODIFIED-FILES
;;;					  EDIT-ORIGINAL-FILES
;;;					  *EMACS-INIT-FILE*
;;;	96/08/22	A. Frazao	Added definitions
;;;					  POSITION-DEFINITION
;;;					  BUFFER-DEFINITIONS
;;;					Changed definitions
;;;					  DEFINITION-OPERATIONS
;;;					  *DEFSTRINGS*
;;;	97/01/01	Joao Filipe	Changed definitions
;;;					  BEGINNING-OF-DEFINITION
;;;					  FILE-MODE
;;;					  GET-C-DEFINITION-ID
;;;	97/07/25	A. Frazao	Changed definitions
;;;					  CHECK-SIGNATURES
;;;					  CHECK-SOURCE-FILES
;;;					  CHECK-MODIFICATION-FILES
;;;					  SAVE-MODIFICATIONS
;;;					  BEEP-MESSAGE
;;;					  MOD-OPERATIONS
;;;					  EDIT-SOURCE-MOD-FILES
;;;					  EDIT-ORIGINAL-MOD-FILES
;;;					  UPDATE-MODIFICATION-FILES-DATE
;;;					  ADD-HEADER-CHANGE-ENTRY
;;;					  SET-DEFINITION-DELETED
;;;					  SET-DEFINITION-MODIFIED
;;;					  ADD-DEFINITION-MOD-LINE
;;;					  ADD-MOD-FILE-HEADER
;;;					  DEFINITION-OPERATIONS
;;;					  FILE-MODE
;;;					Added definitions
;;;					  CHECK-FILE-SIGNATURES-ERROR-DESCRIPTION
;;;					  EDIT-ORIGINAL-SOURCE-FILES
;;;					  MODE-FILLERS
;;;					  UNIX-MODE
;;;	97/08/20	A. Frazao	Changed definitions
;;;					  GET-BUFFER-SIGNATURE
;;;	97/08/29	A. Frazao	Added definitions
;;;					  MAKE-CREWS-MODIFICATION-ORIG
;;;					  *DEFAULT-ORG-DIRS*
;;;					  COPY-EDIT-ORIGINAL-SOURCE-FILE
;;;					  BROWSE-SELECT-FILE
;;;					  BROWSE-FILES-ITEMS
;;;					  BROWSE-DIRECTORIES-ITEMS
;;;					  BROWSE-ITEM<
;;;					  SPLIT-MOD-FILE-ITEMS
;;;					Changed definitions
;;;					  COPY-ORIGINAL-SOURCE-FILES
;;;					  COPY-MODIFICATION-SOURCE-FILES
;;;					  COPY-SOURCE-MODIFICATION-FILES
;;;					  GENERIC-OPERATIONS
;;;					  MAKE-CREWS-MODIFICATION
;;;					  MOD-OPERATIONS
;;;					  X-BEEP-CONFIRM
;;;					  SELECT-MOD-FILE
;;;	97/09/04	A. Frazao	Changed definitions
;;;					  CHECK-SIGNATURES
;;;					Deleted definitions
;;;					  CHECK-FILE-SIGNATURES-ERROR-DESCRIPTION
;;;	98/11/04	A. Frazao	Changed definitions
;;;					  INFORM-BIG-BROTHER
;;;					  COPY-EDIT-ORIGINAL-SOURCE-FILE
;;;					  COPY-ORIGINAL-SOURCE-FILES
;;;					  COPY-MODIFICATION-SOURCE-FILES
;;;					  COPY-SOURCE-MODIFICATION-FILES
;;;					  FILE-MODE
;;;					  MAKE-MAIL-FILE
;;;					  WRITE-MODITICATION-HEADER
;;;					  DISPLAY-CREWS-MOD
;;;					  MOD-OPERATIONS
;;;					  GENERIC-OPERATIONS
;;;					  SELECT-CREWS-MOD-ACTION
;;;					  FILE-OPERATIONS-ITEMS
;;;					  DEFINITION-OPERATIONS-ITEMS
;;;					  POSITION-DEFINITION
;;;					  BUFFER-DEFINITIONS
;;;					  BROWSE-SELECT-FILE
;;;					  BROWSE-FILES-ITEMS
;;;					  BROWSE-DIRECTORIES-ITEMS
;;;					  BROWSE-ITEM<
;;;					  MAKE-CREWS-MODIFICATION-ORIG
;;;					  EDIT-ORIGINAL-SOURCE-FILES
;;;					  EDIT-ORIGINAL-MOD-FILES
;;;					  EDIT-SOURCE-MOD-FILES
;;;					  SELECT-CREWS-MOD
;;;					  SELECT-MOD-FILE
;;;					  DEFSTRUCT CREWS-MOD
;;;					  BEEP-MESSAGE
;;;					  X-BEEP-CONFIRM
;;;					  SAVE-MODIFICATIONS
;;;					  GET-LISP-DEFINITION-ID
;;;					Added definitions
;;;					  MY-END-OF-BUFFER
;;;					  MY-MAKE-DIR
;;;					  MY-DELETE-FILE
;;;					  MY-COPY-FILE
;;;					  EDIT-PATCH-FILE
;;;					  REMOVE-MOD-PATCH
;;;					  MOD-OPERATIONS-ITEMS2
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  GENERIC-OPERATIONS-ITEMS2
;;;					  GENERIC-OPERATIONS-ITEMS1
;;;					  MAKE-MODIFICATION-FROM-MAIL-FILE
;;;					  SET-DEFINITION-TO-PATCH
;;;					Deleted definitions
;;;					  SPLIT-MOD-FILE-ITEMS
;;;	98/11/26	A. Frazao	Changed definitions
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  GENERIC-OPERATIONS-ITEMS1
;;;	98/12/23	A. Frazao	Changed definitions
;;;					  FILE-MODE
;;;	99/01/12	A. Frazao	Deleted definitions
;;;					  MY-MAKE-DIR
;;;					  MY-DELETE-FILE
;;;					  MY-COPY-FILE
;;;					Changed definitions
;;;					  COPY-ORIGINAL-SOURCE-FILES
;;;					  COPY-MODIFICATION-SOURCE-FILES
;;;					  COPY-SOURCE-MODIFICATION-FILES
;;;	99/01/29	A. Frazao	Added definitions
;;;					  BAT-MODE
;;;					Changed definitions
;;;					  UPDATE-MODIFICATION-FILES-DATE
;;;					  CHECK-SIGNATURES
;;;					  ADD-HEADER-CHANGE-ENTRY
;;;					  DEFINITION-OPERATIONS
;;;					  FILE-MODE
;;;					  MODE-FILLERS
;;;	99/02/03	A. Frazao	Changed definitions
;;;					  WRITE-MODITICATION-HEADER
;;;	99/02/03	A. Frazao	Changed definitions
;;;					  SET-DEFINITION-TO-PATCH
;;;	99/02/26	A. Frazao	Changed definitions
;;;					  BROWSE-FILES-ITEMS
;;;	99/03/17	Dario		Changed definitions
;;;					  MAKE-MAIL-FILE
;;;	99/03/18	Dario		Changed definitions
;;;					  MAKE-MAIL-FILE
;;;	99/04/07	A. Frazao	Changed definitions
;;;					  FILE-MODE
;;;	99/05/12	Dario		Changed definitions
;;;					  CHECK-MODIFICATION-FILES
;;;					  CHECK-SOURCE-FILES
;;;	99/06/23	Dario		Changed definitions
;;;					  CHECK-MODIFICATION-FILES
;;;					  CHECK-SOURCE-FILES
;;;	99/07/05	Dario		Changed definitions
;;;					  CHECK-MODIFICATION-FILES
;;;					  CHECK-SOURCE-FILES
;;;	99/08/24	A. Frazao	Added definitions
;;;					  GET-C-NEXT-NAME
;;;					Changed definitions
;;;					  GET-C-DEFINITION-ID
;;;	99/08/31	J. P. Varandas	Added definitions
;;;					  MOD-MENU-HELP
;;;					  MOD-MENU-CHECK-SIGNATURE
;;;					  MOD-MENU-CHANGE-FILE
;;;					  MOD-MENU-DELETE-FILE
;;;					  MOD-MENU-REMOVE-FILE
;;;					  MOD-MENU-EXECUTE
;;;					  DELETE-CURRENT-CREWS-MODIFICATION
;;;					  MOD-MENU-EDIT
;;;					  MOD-MENU-QUIT
;;;					  MOD-MENU-REDISPLAY
;;;					  MOD-MENU-SELECT-MOD-FILE
;;;					  MOD-MENU-FILE-CHAR
;;;					  MOD-MENU-MODIFIED
;;;					  MOD-MENU-MOD-FILE
;;;					  EDITOR-FILE-STATUS
;;;					  CREWS-MOD-EDITOR
;;;					  IS-THE-FILE-CORRECT
;;;					  EDIT-CURRENT-CREWS-MOD
;;;					  MOD-MENU-MODE-MAP
;;;					  PROCESS-MAIL-FILE
;;;					  GET-SYSTEM-NAME
;;;					  SET-DEFINITION-UNDO
;;;					  SET-DEFINITION-UPDATE
;;;					  UPDATE-DEFINITION
;;;					  ADD-DEFINITION-ENTRY
;;;					  MOD-DEFINITION-OPERATIONS-ITEMS
;;;					  SET-HISTORY-LABEL
;;;					  *HISTORY-LABEL*
;;;					  SET-KEYWORD-DELETED
;;;					  SET-KEYWORD-ADDED
;;;					  SET-KEYWORD-CHANGE
;;;					  SET-EXPORT-ADDED
;;;					  SET-EXPORT-DELETED
;;;					  SET-DEFINITION-HEADER
;;;					  SET-DEFINITION-COMMENT
;;;					  SET-DEFINITION-COPY
;;;					  SET-DEFINITION-NEW
;;;					  SET-DEFINITION-MOVED
;;;					  SET-DEFINITION-CLEAR
;;;					  SET-SYSTEM-MOD-LINE
;;;					  ADD-DEFINITION-HEADER
;;;					  REMOVE-KEYWORD-ID
;;;					  GET-KEYWORD-ID
;;;					  REMOVE-SYMBOL-ID
;;;					  GET-SYMBOL-ID
;;;					  GET-FOREIGN-NAME
;;;					  EDIT-BACKUP-FILE
;;;					  EDIT-SOURCE-FILE
;;;					  EDIT-ORIGINAL-FILE
;;;					  EDIT-REFERENCE-FILE
;;;					  GET-REFERENCE-FILE
;;;					  HAS-MODIFIED2
;;;					  HAS-MODIFIED
;;;					  EXPORTATION-OPERATIONS-ITEMS
;;;					  KEYWORD-OPERATIONS-ITEMS
;;;					  CURRENT-YEAR
;;;					Changed definitions
;;;					  MAKE-MOD-FILE-NAME
;;;					  BEEP-MESSAGE
;;;					  COPY-SOURCE-MODIFICATION-FILES
;;;					  UPDATE-MODIFICATION-FILES-DATE
;;;					  MAKE-MAIL-FILE
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  GENERIC-OPERATIONS
;;;					  DISPLAY-ALL-CREWS-MODS
;;;					  DELETE-CREWS-MODIFICATION
;;;					  ADD-HEADER-CHANGE-ENTRY
;;;					  SET-DEFINITION-CHANGED
;;;					  SET-DEFINITION-DELETED
;;;					  SET-DEFINITION-MODIFIED
;;;					  ADD-DEFINITION-MOD-LINE
;;;					  GET-LISP-DEFINITION-ID
;;;					  DEFINITION-OPERATIONS
;;;					  DEFINITION-OPERATIONS-ITEMS
;;;					  FILE-OPERATIONS-ITEMS
;;;					  SELECT-CURRENT-CREWS-MOD
;;;					  NEXT-SPACE
;;;					  NEXT-NON-SPACE
;;;					  FILE-MODE
;;;					  ADD-MOD-FILE-HEADER
;;;					Deleted definitions
;;;					  GENERIC-OPERATIONS-ITEMS2
;;;					Comment definitions
;;;					  *DEFSTRINGS*
;;;	99/09/01	J. P. Varandas	Changed definitions
;;;					  KEYWORD-OPERATIONS-ITEMS
;;;					  SET-DEFINITION-TO-PATCH
;;;					  EXPORTATION-OPERATIONS-ITEMS
;;;					  DEFINITION-OPERATIONS
;;;					  UPDATE-MODIFICATION-FILES-DATE
;;;					  SET-MOD-FILE-CHANGED-IF
;;;					  ADD-DEFINITION-ENTRY
;;;					Added definitions
;;;					  SET-KEYWORD-TO-PATCH
;;;					  SET-EXPORT-TO-PATCH
;;;					  GET-PATCH-FILE
;;;	99/09/01	J. P. Varandas	Changed definitions
;;;					  GET-LISP-DEFINITION-ID
;;;					  DEFINITION-OPERATIONS
;;;					  SET-SYSTEM-MOD-LINE
;;;					Added definitions
;;;					  SET-SOURCE-DELETED
;;;					  SOURCE-OPERATIONS-ITEMS
;;;					  SET-SOURCE-ADDED
;;;					  REMOVE-SOURCE-ID
;;;					  GET-SOURCE-ID
;;;	99/09/07	J. P. Varandas	Changed definitions
;;;					  GET-LISP-DEFINITION-ID
;;;	99/09/10	J. P. Varandas	Changed definitions
;;;					  MOD-MENU-EDIT
;;;	99/09/13	J. P. Varandas	Changed definitions
;;;					  MAKE-MODIFICATION-FROM-MAIL-FILE
;;;					  PROCESS-MAIL-FILE
;;;					  MAKE-MAIL-FILE
;;;	99/09/28	A. Frazao	Changed definitions
;;;					  MAKE-MAIL-FILE
;;;	99/10/19	J. P. Varandas	Changed definitions
;;;					  WRITE-MODITICATION-HEADER
;;;					  MAKE-MODIFICATION-FROM-MAIL-FILE
;;;					  MAKE-MAIL-FILE
;;;					  SET-KEYWORD-TO-PATCH
;;;					  SET-EXPORT-TO-PATCH
;;;					  SET-SYSTEM-MOD-LINE
;;;					  ADD-DEFINITION-ENTRY
;;;					Added definitions
;;;					  GET-KEYWORD-IDS
;;;					  GET-TRANSLATION-LANGUAGES
;;;					  GET-SYSTEM
;;;					  MAX-MAIL-NUMBER
;;;	00/01/31	J. P. Varandas	Changed definitions
;;;					  DEFINITION-OPERATIONS-ITEMS
;;;					  ADD-DEFINITION-HEADER
;;;					  ADD-DEFINITION-MOD-LINE
;;;					  CURRENT-YEAR
;;;					Added definitions
;;;					  SET-DEFINITION-DOC-UPDATE
;;;	00/02/02	A. Frazao	Deleted definitions
;;;					  MY-CURRENT-TIME-STRING
;;;					  CURRENT-YEAR
;;;					  CURRENT-DATE-STRING
;;;					  *MONTHS*
;;;	00/02/18	J. P. Varandas	Changed definitions
;;;					  ADD-DEFINITION-MOD-LINE
;;;	00/03/24	A. Frazao	Added definitions
;;;					  EDIFF-FILE-WITH-MODIFICATION
;;;					  EDIFF-FILE-WITH-ORIGINAL
;;;					  EDIFF-ORIGINAL-SOURCE-FILES
;;;					  EDIFF-ORIGINAL-MOD-FILES
;;;					  EDIFF-SOURCE-MOD-FILES
;;;					Changed definitions
;;;					  FILE-OPERATIONS-ITEMS
;;;					  MOD-OPERATIONS-ITEMS1
;;;	00/05/05	A. Vasconcelos	Changed definitions
;;;					  SELECT-CREWS-MOD-ACTION
;;;					  WRITE-MODITICATION-HEADER
;;;					Added definitions
;;;					  *CRM-VERSION*
;;;	00/06/06	A. Frazao	Added definitions
;;;					  MOVE-MOD-FILE
;;;					Changed definitions
;;;					  *CRM-VERSION*
;;;					  FILE-OPERATIONS-ITEMS
;;;	00/06/08	Dario		Changed definitions
;;;					  *CRM-VERSION*
;;;					  GET-LISP-DEFINITION-ID
;;;	00/06/30	Dario		Changed definitions
;;;					  *CRM-VERSION*
;;;	00/08/22	Dario		Changed definitions
;;;					  *CRM-VERSION*
;;;	00/09/06	A. Frazao	Changed definitions
;;;					  *CRM-VERSION*
;;;					  WRITE-MODITICATION-HEADER
;;;	01/01/05	Dario		Changed definitions
;;;					  *CRM-VERSION*
;;;					  GET-LISP-DEFINITION-ID
;;;	01/03/02	Toni		Added definitions
;;;					  G-BEG-PREVIOUS-BUFFER-SIZE
;;;					  G-END-PREVIOUS-BUFFER-SIZE
;;;					  LOOKING-LITERALLY-AT
;;;					  RE-VB-FN-HEADER
;;;					  RE-VB-FN-ARGS-LIST
;;;					  RE-VB-FN-ARG
;;;					  RE-VB-VAR-DECL
;;;					  RE-VB-CONST-DECL
;;;					  RE-VB-NAME
;;;					  RE-PHP-FN-HEADER
;;;					  RE-PHP-FN-ARGS-LIST
;;;					  RE-PHP-FN-ARG
;;;					  RE-PHP-VAR-DECL
;;;					  RE-PHP-CONST-DECL
;;;					  RE-PHP-NAME
;;;					  RE-JSCRIPT-FN-HEADER
;;;					  RE-JSCRIPT-FN-ARGS-LIST
;;;					  RE-JSCRIPT-FN-ARG
;;;					  RE-JSCRIPT-VAR-DECL
;;;					  RE-JSCRIPT-VAR-DECL-WITHOUT-VAR
;;;					  RE-JSCRIPT-VAR-DECL-WITH-VAR
;;;					  RE-JSCRIPT-NAME
;;;					  RE-1-OR-MORE-WHITESPACES
;;;					  G-POSITION-OF-BEGINNING-OF-DEFINITION
;;;					  G-POSITION-OF-END-OF-DEFINITION
;;;					  WEB-BEGINNING-OF-DEFINITION
;;;					  WEB-END-OF-DEFINITION
;;;					  C-END-OF-DEFINITION
;;;					  END-OF-DEFINITION
;;;					  VB-END-OF-DEFINITION
;;;					  PHP-END-OF-DEFINITION
;;;					  JSCRIPT-END-OF-DEFINITION
;;;					  HTML-END-OF-DEFINITION
;;;					  RE-0-OR-MORE-WHITESPACES
;;;					  GET-VB-DEFINITION-ID
;;;					  GET-PHP-DEFINITION-ID
;;;					  GET-JSCRIPT-DEFINITION-ID
;;;					  GET-HTML-DEFINITION-ID
;;;					  C-BEGINNING-OF-DEFINITION
;;;					  HTML-BEGINNING-OF-DEFINITION
;;;					  GET-WEB-DEFINITION-LANGUAGE
;;;					  G-DEFINITION-LANGUAGE
;;;					  GET-DEFINITION-LANGUAGE
;;;					  VB-LANGUAGE
;;;					  PHP-LANGUAGE
;;;					  JSCRIPT-LANGUAGE
;;;					  HTML-LANGUAGE
;;;					  C-LANGUAGE
;;;					  LISP-LANGUAGE
;;;					  UNKNOWN-LANGUAGE
;;;					  JSCRIPT-BEGINNING-OF-DEFINITION
;;;					  PHP-BEGINNING-OF-DEFINITION
;;;					  VB-BEGINNING-OF-DEFINITION
;;;					  HTML-MODE
;;;					  PHP-MODE
;;;					  VB-MODE
;;;					Changed definitions
;;;					  GET-BUFFER-SIGNATURE
;;;					  SELECT-CREWS-MOD-ACTION
;;;					  SET-DEFINITION-CLEAR
;;;					  SET-DEFINITION-UNDO
;;;					  *CRM-VERSION*
;;;					  GET-DEFINITION-ID
;;;					  ADD-MOD-FILE-HEADER
;;;					  UPDATE-DEFINITION
;;;					  ADD-DEFINITION-ENTRY
;;;					  ADD-DEFINITION-HEADER
;;;					  ADD-DEFINITION-MOD-LINE
;;;					  HAS-MODIFIED2
;;;					  BEGINNING-OF-DEFINITION
;;;					  MODE-FILLERS
;;;					  FILE-MODE
;;;	01/04/20	Dario		Changed definitions
;;;					  *CRM-VERSION*
;;;	01/05/02	A. Vasconcelos	Changed definitions
;;;					  *CRM-VERSION*
;;;	01/07/04	J. P. Varandas	Changed definitions
;;;					  MOD-MENU-EDIT
;;;					  MOD-MENU-EXECUTE
;;;					  PROCESS-MAIL-FILE
;;;					  FILE-OPERATIONS-ITEMS
;;;					  GET-LISP-DEFINITION-ID
;;;					  GENERIC-OPERATIONS-ITEMS1
;;;					Added definitions
;;;					  GET-FILE-FROM-MODIFICATION
;;;					  GET-FILE-FROM-ORIGINAL
;;;					  SC-MAKE-FILE-WRITABLE
;;;					  SET-MOD-FILE-TO-PATCH
;;;	01/09/27	A. Vasconcelos	Changed definitions
;;;					  *CRM-VERSION*
;;;					  GET-SYSTEM-NAME
;;;	01/10/01	Dario		Changed definitions
;;;					  ADD-MOD-FILE-HEADER
;;;	01/10/12	Dario		Changed definitions
;;;					  GET-LISP-DEFINITION-ID
;;;	01/10/16	Toni		Added definitions
;;;					  JSCRIPT-MODE
;;;					Changed definitions
;;;					  *CRM-VERSION*
;;;					  GET-DEFINITION-ID
;;;					  BEGINNING-OF-DEFINITION
;;;					  END-OF-DEFINITION
;;;					  GET-DEFINITION-LANGUAGE
;;;					  GET-WEB-DEFINITION-LANGUAGE
;;;					  MODE-FILLERS
;;;					  FILE-MODE
;;;	01/11/24	A. Frazao	Changed definitions
;;;					  GENERIC-OPERATIONS-ITEMS1
;;;	02/01/18	Dario		Changed definitions
;;;					  GET-LISP-DEFINITION-ID
;;;	02/01/31	Pedro Matos	Changed definitions
;;;					  SET-DEFINITION-DOC-UPDATE
;;;					  GET-SYSTEM-NAME
;;;	02/02/28	A. Frazao	Changed definitions
;;;					  GET-SYSTEM-NAME
;;;					  GET-LISP-DEFINITION-ID
;;;					Moved definitions
;;;					  NEXT-SPACE
;;;					  NEXT-NON-SPACE
;;;	02/05/28	A. Frazao	Changed definitions
;;;					  CHECK-SIGNATURES
;;;	02/06/07	J. P. Varandas	Added definitions
;;;					  MEASURE-MOD-FILES-CHECK-DATE
;;;					  MEASURE-MOD-FILES
;;;					  MEASURE-MOD-FILE-CHECK-DATE
;;;					  MEASURE-MOD-FILE
;;;					  MAKE-CREWS-MODIFICATION2
;;;					  EDIFF-FILE-WITH-6-4-0
;;;					  EDIFF-FILE-WITH-6-3-0
;;;					  EDIFF-FILE-WITH-6-2-0
;;;					  MAKE-CREWS-MOD-WITH-FILE
;;;					  SELECT-CREWS-MOD-FROM-FILE
;;;					  SPLIT-CREWS-MOD
;;;					Changed definitions
;;;					  GENERIC-OPERATIONS-ITEMS1
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  FILE-OPERATIONS-ITEMS
;;;					  PROCESS-MAIL-FILE
;;;					  SET-MOD-FILE-TO-PATCH
;;;					  GET-LISP-DEFINITION-ID
;;;	02/07/18	Carlos Ribeiro	Changed definitions
;;;					  GET-SYSTEM-NAME
;;;					  SPLIT-CREWS-MOD
;;;	02/08/28	Dario		Changed definitions
;;;					  SET-EXPORT-TO-PATCH
;;;	03/02/05	A. Frazao	Changed definitions
;;;					  SPLIT-CREWS-MOD
;;;	03/07/28	A. Frazao	Changed definitions
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  GENERIC-OPERATIONS-ITEMS1
;;;					Deleted definitions
;;;					  *DEFAULT-ORG-DIRS*
;;;					  MAKE-CREWS-MODIFICATION-ORIG
;;;					Added definitions
;;;					  RECREATE-MOD-PATCH
;;;					  SET-NEW-FILE-TO-PATCH
;;;					  TOUCH-ALL-MODS-SOURCE-FILES
;;;					  TOUCH-CURRENT-MOD-SOURCE-FILES
;;;					  CHECK-ALL-MODIFICATIONS-SOURCE-FILES
;;;	04/01/05	Toni		Changed definitions
;;;					  ADD-DEFINITION-ENTRY
;;;	04/02/12	Duarte Peralta	Changed definitions
;;;					  SET-EXPORT-TO-PATCH
;;;	04/03/12	A. Frazao	Changed definitions
;;;					  SOURCE-OPERATIONS-ITEMS
;;;					  EXPORTATION-OPERATIONS-ITEMS
;;;					  KEYWORD-OPERATIONS-ITEMS
;;;					  MOD-DEFINITION-OPERATIONS-ITEMS
;;;					  DEFINITION-OPERATIONS-ITEMS
;;;					  FILE-OPERATIONS-ITEMS
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  MOD-OPERATIONS-ITEMS2
;;;					  COPY-ORIGINAL-SOURCE-FILES
;;;					  COPY-MODIFICATION-SOURCE-FILES
;;;					  GENERIC-OPERATIONS-ITEMS1
;;;					Added definitions
;;;					  SOURCE-OPERATIONS-ITEMS-HELP
;;;					  EXPORTATION-OPERATIONS-ITEMS-HELP
;;;					  KEYWORD-OPERATIONS-ITEMS-HELP
;;;					  MOD-DEFINITION-OPERATIONS-ITEMS-HELP
;;;					  DEFINITION-OPERATIONS-ITEMS-HELP
;;;					  FILE-OPERATIONS-ITEMS-HELP
;;;					  MOD-OPERATIONS-ITEMS1-HELP
;;;					  SHOW-MOD-HELP
;;;					  GENERIC-OPERATIONS-ITEMS1-HELP
;;;					  EDIT-ALL-PATCH-FILE
;;;					  EDIT-ALL-MOD-FILE
;;;					  EDIT-ALL-MOD-ORIGINAL-FILE
;;;					  EDIT-ALL-MOD-SOURCE-FILE
;;;					  SELECT-ALL-MOD-FILE
;;;					  COPY-ALL-ORIGINAL-SOURCE-FILES
;;;					  COPY-ALL-MODIFICATION-SOURCE-FILES
;;;					  COPY-ALL-SOURCE-MODIFICATION-FILES
;;;					Documented definitions
;;;					  COPY-SOURCE-MODIFICATION-FILES
;;;	04/03/12	A. Frazao	Changed definitions
;;;					  COPY-ALL-ORIGINAL-SOURCE-FILES
;;;	04/03/26	A. Frazao	Changed definitions
;;;					  RECREATE-MOD-PATCH
;;;	04/04/23	A. Frazao	Changed definitions
;;;					  FILE-OPERATIONS-ITEMS-HELP
;;;					  SET-MOD-FILE-TO-PATCH
;;;					  GET-PATCH-FILE
;;;	04/06/22	A. Frazao	Deleted definitions
;;;					  BUFFER-DEFINITIONS
;;;					Changed definitions
;;;					  SOURCE-OPERATIONS-ITEMS
;;;					  EXPORTATION-OPERATIONS-ITEMS-HELP
;;;					  EXPORTATION-OPERATIONS-ITEMS
;;;					  DEFINITION-OPERATIONS-ITEMS-HELP
;;;					  MOD-DEFINITION-OPERATIONS-ITEMS-HELP
;;;					  DEFINITION-OPERATIONS-ITEMS
;;;					  POSITION-DEFINITION
;;;					Added definitions
;;;					  SEARCH-DEFINITION-IN-SOURCE-FILE
;;;	04/06/30	A. Frazao	Changed definitions
;;;					  MOD-DEFINITION-OPERATIONS-ITEMS
;;;	05/03/21	J. P. Varandas	Changed definitions
;;;					  KEYWORD-OPERATIONS-ITEMS-HELP
;;;					  EXPORTATION-OPERATIONS-ITEMS-HELP
;;;					  MOD-DEFINITION-OPERATIONS-ITEMS-HELP
;;;					  DEFINITION-OPERATIONS-ITEMS-HELP
;;;					  GET-KEYWORD-IDS
;;;					  MOD-OPERATIONS-ITEMS1-HELP
;;;					  FILE-OPERATIONS-ITEMS-HELP
;;;					  MOD-OPERATIONS-ITEMS2-HELP
;;;					  WRITE-MODITICATION-HEADER
;;;					  MOD-OPERATIONS-ITEMS2
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  FILE-OPERATIONS-ITEMS
;;;					  UPDATE-MODIFICATION-FILES-DATE
;;;					  SET-MOD-FILE-CHANGED
;;;					  SET-MOD-FILE-CHANGED-IF
;;;					  DISPLAY-CREWS-MOD
;;;					  SET-KEYWORD-TO-PATCH
;;;					  SET-EXPORT-TO-PATCH
;;;					  SET-DEFINITION-TO-PATCH
;;;					  ADD-DEFINITION-MOD-LINE
;;;					  SAVE-MODIFICATIONS
;;;					  DEFSTRUCT CREWS-MOD
;;;					Added definitions
;;;					  EDIT-MOD-ALL-SOURCE-FILE
;;;					  APPEND-DEFINITIONS-PATCH
;;;					  CHANGE-CREWS-MOD-REFERENCE
;;;					  CURRENT-MOD-REFERENCE
;;;	05/04/06	A. Frazao	Changed definitions
;;;					  SET-DEFINITION-TO-PATCH
;;;	05/04/08	Rui Mestre	Changed definitions
;;;					  MOD-OPERATIONS-ITEMS1-HELP
;;;					  SET-MOD-FILE-TO-PATCH
;;;					  MOD-OPERATIONS-ITEMS1
;;;					Added definitions
;;;					  *CREWS-X-PATCHES-SRC-DIR*
;;;					  *CREWS-PATCHES-SRC-DIR*
;;;					  *COMPANY*
;;;					  FIND-PATCH
;;;					  FIND-MOD-PATCH
;;;	05/08/05	Fernando	Changed definitions
;;;					  WRITE-MODITICATION-HEADER
;;;					  MAKE-MAIL-FILE
;;;	05/09/14	A. Frazao	Added definitions
;;;					  GLOBAL-MAP
;;;					  SEND-CREWS-MOD-MAIL
;;;					  GET-MOD-MAIL-SUBJECT
;;;					  GET-CREWS-MOD-MAIL-FILE
;;;					Changed definitions
;;;					  DEFSTRUCT CREWS-MOD
;;;					  MOD-MENU-EXECUTE
;;;					  PROCESS-MAIL-FILE
;;;					  MAKE-MAIL-FILE
;;;	05/09/16	A. Frazao	Added definitions
;;;					  SEND-MOD-OPERATIONS
;;;					  SEND-MOD-OPERATIONS-ITEMS
;;;					  *WINDOWS-CRI*
;;;					Changed definitions
;;;					  WRITE-MODITICATION-HEADER
;;;					  GET-CREWS-MOD-MAIL-FILE
;;;					  SELECT-CREWS-MOD-ACTION
;;;					  SEND-CREWS-MOD-MAIL
;;;					Deleted definitions
;;;					  GET-MOD-MAIL-SEND-TO
;;;					  GLOBAL-MAP
;;;	05/09/16	A. Frazao	Changed definitions
;;;					  SELECT-CREWS-MOD-ACTION
;;;	05/09/19	A. Frazao	Changed definitions
;;;					  SEND-MOD-OPERATIONS-ITEMS
;;;	05/10/12	A. Frazao	Changed definitions
;;;					  WRITE-MODITICATION-HEADER
;;;					  SEND-MOD-OPERATIONS-ITEMS
;;;	05/10/12	A. Frazao	Changed definitions
;;;					  SEND-CREWS-MOD-MAIL
;;;	05/10/13	J. P. Varandas	Changed definitions
;;;					  ADD-CREWS-MOD
;;;					  GET-CREWS-MOD-MAIL-FILE
;;;					  MOD-MENU-EXECUTE
;;;					  SELECT-CREWS-MOD
;;;					  *CRM-VERSION*
;;;					  RECREATE-MOD-PATCH
;;;					  MOD-MENU-CHANGE-FILE
;;;					  PROCESS-MAIL-FILE
;;;					  CHECK-MODIFICATION-FILES
;;;					  CHECK-SOURCE-FILES
;;;					  CHECK-SRC-FILE
;;;					  CHECK-MOD-FILE
;;;					  MEASURE-MOD-FILE-CHECK-DATE
;;;					  MEASURE-MOD-FILE
;;;					  WRITE-MODITICATION-HEADER
;;;					  SPLIT-CREWS-MOD
;;;					  MAKE-MODIFICATION-FROM-MAIL-FILE
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  MOD-OPERATIONS-ITEMS1-HELP
;;;					  MAKE-MAIL-FILE
;;;					  DISPLAY-CREWS-MOD
;;;					  SAVE-MODIFICATIONS
;;;					  DEFSTRUCT CREWS-MOD
;;;					  GET-SYSTEM
;;;					  SET-DEFINITION-TO-PATCH
;;;					  DELETE-CREWS-MOD
;;;					  SET-KEYWORD-TO-PATCH
;;;					Added definitions
;;;					  EDIT-MOD-MAIL-FILE
;;;					  *GUI-USERS*
;;;					  APPEND-MOD-DEFINITIONS-PATCH
;;;					  CHECK-DOC-SRC-FILE
;;;					  GET-PATCH-APPLICABILITY
;;;					  SPLIT-CREWS-MOD-BY-PATCHES
;;;					  SPLIT-BY-GUI-MODULES
;;;					  SET-MOD-FILE-TO-PATCH2
;;;					  CREATE-SPLITED-MODIFICATIONS
;;;					  SPLIT-BY-SYSTEM-PATCHES
;;;					  SEARCH-EXACTLY
;;;					  SELECT-SYSTEM-PATCH-VERSION
;;;					  SYSTEM2PATCHES-ORIGINAL-DIR
;;;					  SYSTEM-SOURCE-TOP-DIR
;;;					  SELECT-DIR-FILE
;;;					  PROCESS-SELECTED-DATA-ITEM
;;;					  CURRENT-MOD-VERSIONS
;;;					Deleted definitions
;;;					  MAX-MAIL-NUMBER
;;;					  *CREWS-REVERTED-MAIL*
;;;					  *CREWS-MAIL-VERSION-NAME*
;;;					  *CREWS-MAIL-VERSIONS*
;;;					  FIND-MOD-PATCH
;;;					  *CREWS-X-PATCHES-SRC-DIR*
;;;					  *CREWS-PATCHES-SRC-DIR*
;;;					  *COMPANY*
;;;					  FIND-PATCH
;;;	05/10/14	A. Frazao	Changed definitions
;;;					  *WINDOWS-CRI*
;;;	05/10/14	A. Frazao	Changed definitions
;;;					  MAKE-MAIL-FILE
;;;	05/10/19	J. P. Varandas	Changed definitions
;;;					  SET-DEFINITION-TO-PATCH
;;;					  MAKE-MODIFICATION-FROM-MAIL-FILE
;;;					  WRITE-MODITICATION-HEADER
;;;					  SYSTEM2PATCHES-ORIGINAL-DIR
;;;					Deleted definitions
;;;					  MOVE-AFTER-STR
;;;	05/11/11	Carlos Ribeiro	Changed definitions
;;;					  GET-SYSTEM-NAME
;;;	05/12/07	A. Frazao	Changed definitions
;;;					  DEFINITION-OPERATIONS
;;;	06/04/11	J. P. Varandas	Changed definitions
;;;					  MOD-OPERATIONS-ITEMS1-HELP
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  FILE-OPERATIONS-ITEMS
;;;					  FILE-OPERATIONS-ITEMS-HELP
;;;					  PROCESS-MAIL-FILE
;;;					  GET-FILE-FROM-MODIFICATION
;;;					  GET-FILE-FROM-ORIGINAL
;;;					  GET-LISP-DEFINITION-ID
;;;					  SELECT-SYSTEM-PATCH-VERSION
;;;					  SYSTEM-SOURCE-TOP-DIR
;;;					Added definitions
;;;					  GET-PATCH-FOR-VERSION
;;;					  SET-MOD-CREWS-VERSION
;;;					  EDIFF-FILE-WITH-OLD-VERSION
;;;					  SYSTEM2ORIGINAL-DIR
;;;					Deleted definitions
;;;					  EDIFF-FILE-WITH-6-4-0
;;;					  EDIFF-FILE-WITH-6-3-0
;;;					  EDIFF-FILE-WITH-6-2-0
;;;	06/05/29	A. Frazao	Changed definitions
;;;					  SAVE-MODIFICATIONS
;;;					  SELECT-CREWS-MOD
;;;					  DEFSTRUCT CREWS-MOD
;;;					  SEND-CREWS-MOD-MAIL
;;;					  GENERIC-OPERATIONS-ITEMS1-HELP
;;;					  GENERIC-OPERATIONS-ITEMS1
;;;					  MOD-OPERATIONS-ITEMS1-HELP
;;;					  MOD-OPERATIONS-ITEMS1
;;;					Added definitions
;;;					  EDIT-ALL-MOD-MAIL-FILE
;;;					  EDIT-MOD-MAIL-FILE
;;;	06/05/29	A. Frazao	Changed definitions
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  MOD-OPERATIONS-ITEMS1-HELP
;;;					  EDIT-MOD-MAIL-FILE
;;;					  GENERIC-OPERATIONS-ITEMS1
;;;	06/08/24	J. P. Varandas	Changed definitions
;;;					  CHECK-MODIFICATION-FILES
;;;					  CHECK-SOURCE-FILES
;;;	06/12/12	RAurelio	Changed definitions
;;;					  SPLIT-BY-GUI-MODULES
;;;	08/02/22	Tom Weissmann	Changed definitions
;;;					  GET-LISP-DEFINITION-ID
;;;					  ADD-DEFINITION-MOD-LINE
;;;					  DELETE-CREWS-MODIFICATION
;;;					  SELECT-CREWS-MOD
;;;					  GET-NEXT-NAME
;;;					Added definitions
;;;					  SC-MOD-DEFINITION-CONTAINERS
;;;					  SC-MOD-DEFINITION-HANDLERS
;;;					  SC-MOD-GET-DEFINITION-HANDLER
;;;					  GET-CURRENT-NAME
;;;	08/02/25	T. Maduro Dias	Changed definitions
;;;					  SC-MOD-DEFINITION-HANDLERS
;;;	08/07/08	A. Frazao	Updated commented values
;;;					Changed definitions
;;;					  GET-SYSTEM-NAME
;;;					  *CRM-VERSION*
;;;	08/07/29	A. Frazao	Changed definitions
;;;					  GET-DEFINITION-ID
;;;					  ADD-DEFINITION-ENTRY
;;;					  GET-BUFFER-SIGNATURE
;;;					  ADD-MOD-FILE-HEADER
;;;					  MODE-FILLERS
;;;					  FILE-MODE
;;;					Added definitions
;;;					  BEGINNING-OF-FILE-HEADER
;;;					  XML-MODE
;;;	08/08/01	Fausto		Changed definitions
;;;					  SPLIT-BY-GUI-MODULES
;;;	08/09/03	A. Frazao	Changed definitions
;;;					  SC-MOD-DEFINITION-HANDLERS
;;;	08/10/09	Tom Weissmann	Changed definitions
;;;					  SAVE-MODIFICATIONS
;;;	08/12/31	P. Filipe	Changed definitions
;;;					  SYSTEM2PATCHES-ORIGINAL-DIR
;;;					  SYSTEM2ORIGINAL-DIR
;;;	09/01/19	Rui Patrocínio	Changed definitions
;;;					  SC-MOD-DEFINITION-HANDLERS
;;;	09/02/02	J. P. Varandas	Changed definitions
;;;					  SC-MOD-DEFINITION-HANDLERS
;;;					  GET-NEXT-METHOD-CLASS
;;;	09/02/04	J. P. Varandas	Changed definitions
;;;					  SYSTEM2PATCHES-ORIGINAL-DIR
;;;					  SYSTEM2ORIGINAL-DIR
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  GET-SYSTEM-NAME
;;;					  SYSTEM2PATCHES-ORIGINAL-DIR
;;;					  SYSTEM2ORIGINAL-DIR
;;;					  BROWSE-FILES-ITEMS
;;;					  *MODIF-MAIL-FROM*
;;;					  *MODIF-MAIL-NAME*
;;;					  GENERIC-OPERATIONS-ITEMS1-HELP
;;;					  *MODIFS-MAIL-DIR*
;;;					  *ACTIONS-LOG-FILE*
;;;					  SET-MODIF-REQUEST-VERSION
;;;					  DEFSTRUCT MODIF-REQUEST
;;;					  SELECT-MODIF-REQUEST
;;;					  SELECT-MODIF-REQUEST-ACTION
;;;					  SHOW-MOD-HELP
;;;					  SEND-MOD-OPERATIONS-ITEMS
;;;					  SEND-MODIF-REQUEST-MAIL
;;;					  MOD-MENU-REDISPLAY
;;;					  MODIF-REQUEST-EDITOR
;;;					  SELECT-MODIF-REQUEST-FROM-FILE
;;;					  DISPLAY-MODIF-REQUEST
;;;					  RUN-PROGRAM
;;;					  SPLIT-MODIF-REQUEST
;;;					  SPLIT-CREWS-MOD
;;;					  SPLIT-MODIF-REQUEST-BY-PATCHES
;;;					  FILE-OPERATIONS-ITEMS
;;;					  MAKE-MODIF-REQUEST-WITH-FILE
;;;					  DELETE-MODIF-REQUEST
;;;					  ADD-MODIF-REQUEST
;;;					  ADD-CREWS-MOD
;;;					  ALL-MODIF-REQUESTS
;;;					  *ALL-MODIF-REQUESTS*
;;;					  DISPLAY-ALL-MODIF-REQUESTS
;;;					  SET-ALL-MODIF-REQUESTS
;;;					  EDIT-ALL-MOD-MAIL-FILE
;;;					  EDIT-ALL-MOD-FILE
;;;					  SET-MOD-FILE-TO-PATCH2
;;;					  EDIT-ALL-MOD-SOURCE-FILE
;;;					  EDIT-ALL-MOD-ORIGINAL-FILE
;;;					  CHECK-MOD-FILE
;;;					  CHECK-SRC-FILE
;;;					  GET-PATCH-APPLICABILITY
;;;					  EDIT-ALL-PATCH-FILE
;;;					  GET-PATCH-FILE
;;;					  WRITE-MODITICATION-HEADER
;;;					  GET-SYSTEM
;;;					  SELECT-MOD-FILE
;;;					  MAKE-MOD-FILE-NAME
;;;					  CHECK-MOD-VERSION
;;;					  LOAD-MODIFICATIONS
;;;					  *MODIF-REQUEST-FILE*
;;;					  READ-MOD-NAME
;;;					  SELECT-CREWS-MOD
;;;					  SELECT-ALL-MOD-FILE
;;;					  DISPLAY-CREWS-MOD
;;;					  INFORM-BIG-BROTHER
;;;					  SAVE-MODIFICATIONS
;;;					  MOD-OPERATIONS
;;;					  FILE-OPERATIONS
;;;					  MEASURE-MOD-FILES-CHECK-DATE
;;;					  MEASURE-MOD-FILES
;;;					  MEASURE-SRC-FILES
;;;					  COPY-EDIT-ORIGINAL-SOURCE-FILE
;;;					  SEND-CREWS-MOD-MAIL
;;;					  MOD-MENU-CHANGE-FILE
;;;					  MOD-MENU-DELETE-FILE
;;;					  MOD-MENU-REMOVE-FILE
;;;					  MOD-MENU-EXECUTE
;;;					  MOD-MENU-EDIT
;;;					  MOD-MENU-SELECT-MOD-FILE
;;;					  CREWS-MOD-EDITOR
;;;					  EDIT-MOD-MAIL-FILE
;;;					  RECREATE-MOD-PATCH
;;;					  SET-NEW-FILE-TO-PATCH
;;;					  UPDATE-MODIFICATION-FILES-DATE
;;;					  GET-FILE-FROM-MODIFICATION
;;;					  GET-FILE-FROM-ORIGINAL
;;;					  EDIFF-FILE-WITH-MODIFICATION
;;;					  EDIFF-FILE-WITH-OLD-VERSION
;;;					  EDIFF-FILE-WITH-ORIGINAL
;;;					  EDIFF-ORIGINAL-SOURCE-FILES
;;;					  EDIFF-ORIGINAL-MOD-FILES
;;;					  EDIFF-SOURCE-MOD-FILES
;;;					  EDIT-PATCH-FILE
;;;					  EDIT-MOD-FILE
;;;					  EDIT-MOD-ORIGINAL-FILE
;;;					  EDIT-MOD-SOURCE-FILE
;;;					  EDIT-ORIGINAL-SOURCE-FILES
;;;					  EDIT-ORIGINAL-MOD-FILES
;;;					  EDIT-SOURCE-MOD-FILES
;;;					  COPY-ORIGINAL-SOURCE-FILES
;;;					  COPY-MODIFICATION-SOURCE-FILES
;;;					  COPY-SOURCE-MODIFICATION-FILES
;;;					  REMOVE-MOD-PATCH
;;;					  EDIT-MOD-ALL-SOURCE-FILE
;;;					  APPEND-DEFINITIONS-PATCH
;;;					  MOVE-MOD-FILE
;;;					  REM-MOD-FILE
;;;					  SET-MOD-FILE-TO-PATCH
;;;					  SET-MOD-FILE-DEL
;;;					  SET-MOD-FILE-NEW
;;;					  SET-MOD-FILE-CHANGED
;;;					  SET-MOD-FILE-CHANGED-IF
;;;					  CHECK-ALL-MODS-SOURCE-FILE
;;;					  SET-SOURCE-DELETED
;;;					  SET-SOURCE-ADDED
;;;					  SET-EXPORT-DELETED
;;;					  SET-EXPORT-ADDED
;;;					  SET-KEYWORD-TO-PATCH
;;;					  SET-EXPORT-TO-PATCH
;;;					  SET-DEFINITION-TO-PATCH
;;;					  SET-DEFINITION-COMMENT
;;;					  SET-DEFINITION-UPDATE
;;;					  SET-DEFINITION-MOVED
;;;					  SET-DEFINITION-DELETED
;;;					  SET-DEFINITION-COPY
;;;					  SET-DEFINITION-NEW
;;;					  SET-DEFINITION-ADDED
;;;					  SET-DEFINITION-DOC-UPDATE
;;;					  SET-DEFINITION-CHANGED
;;;					  EDIT-BACKUP-FILE
;;;					  EDIT-SOURCE-FILE
;;;					  EDIT-ORIGINAL-FILE
;;;					  GET-REFERENCE-FILE
;;;					  TOUCH-CURRENT-MOD-SOURCE-FILES
;;;					  GET-PATCH-FOR-VERSION
;;;					  CREATE-SPLITED-MODIFICATIONS
;;;					  CURRENT-MOD-VERSIONS
;;;					  CURRENT-MOD-REFERENCE
;;;					  CURRENT-MOD-DATE-STRING
;;;					  CURRENT-MOD-AUTHOR
;;;					  DELETE-CREWS-MOD
;;;					  CURRENT-MODIF-REQUEST
;;;					  *CURRENT-MODIF-REQUEST*
;;;					  MAKE-MAIL-FILE
;;;					  GET-MODIF-REQUEST-MAIL-FILE
;;;					  CHANGE-MODIF-REQUEST-REFERENCE
;;;					  CHANGE-MODIF-REQUEST-DATE
;;;					  CHANGE-MODIF-REQUEST-DIR
;;;					  CHANGE-MODIF-REQUEST-SRC-DIR
;;;					  CHANGE-MODIF-REQUEST-ORG-DIR
;;;					  CHANGE-MODIF-REQUEST-AUTHOR
;;;					  MOD-OPERATIONS-ITEMS2
;;;					  CHANGE-MODIF-REQUEST-NAME
;;;					  SELECT-CURRENT-MODIF-REQUEST
;;;					  MOD-OPERATIONS-ITEMS1
;;;					  DISPLAY-CURRENT-MODIF-REQUEST
;;;					  CHECK-MODIFICATION-FILES
;;;					  CHECK-SOURCE-FILES
;;;					  EDIT-CURRENT-MODIF-REQUEST
;;;					  PROCESS-MAIL-FILE
;;;					  SELECT-CURRENT-CREWS-MOD
;;;					  COPY-ALL-ORIGINAL-SOURCE-FILES
;;;					  COPY-ALL-MODIFICATION-SOURCE-FILES
;;;					  COPY-ALL-SOURCE-MODIFICATION-FILES
;;;					  SELECT-CREWS-MOD-FROM-FILE
;;;					  TOUCH-ALL-MODS-SOURCE-FILES
;;;					  CHECK-ALL-MODIFICATIONS-SOURCE-FILES
;;;					  SET-CURRENT-MODIF-REQUEST
;;;					  SET-MOD-CREWS-VERSION
;;;					  DELETE-MODIFICATION-REQUEST
;;;					  GENERIC-OPERATIONS-ITEMS1
;;;					  MAKE-MODIFICATION-REQUEST2
;;;					  MAKE-MODIFICATION-REQUEST
;;;					  MAKE-MODIFICATION-FROM-MAIL-FILE
;;;					  MAKE-CREWS-MOD-WITH-FILE
;;;					  EDIT-CURRENT-CREWS-MOD
;;;					  SPLIT-CREWS-MOD-BY-PATCHES
;;;					  SEND-MOD-OPERATIONS
;;;					  GET-CREWS-MOD-MAIL-FILE
;;;					  *CRM-VERSION*
;;;					Deleted definitions
;;;					  DELETE-CURRENT-MODIFICATION-REQUEST
;;;					  *GUI-USERS*
;;;					  SPLIT-BY-GUI-MODULES
;;;					  *WINDOWS-CRI*
;;;	09/02/17	J. P. Varandas	Changed definitions
;;;					  SYSTEM2PATCHES-ORIGINAL-DIR
;;;					  SET-MODIF-REQUEST-VERSION
;;;	09/02/25	Paulo Tomé	Changed definitions
;;;					  FILE-MODE
;;;	09/05/06	J. P. Varandas	Changed definitions
;;;					  SC-MOD-DEFINITION-HANDLERS
;;;	09/06/30	Ana Afonso	Added definitions
;;;					  CERT-MODE
;;;					Changed definitions
;;;					  FILE-MODE
;;;	09/09/23	P. Madeira	Added require
;;;					  'cl
;;;					Changed definitions
;;;					  SYSTEM2ORIGINAL-DIR
;;;					  LINE-FEED
;;;					  GET-WEB-DEFINITION-LANGUAGE
;;;					  WEB-END-OF-DEFINITION
;;;					  WEB-BEGINNING-OF-DEFINITION
;;;					  HAS-MODIFIED
;;;					  HAS-MODIFIED2
;;;					  REMOVE-SYMBOL-ID
;;;					  REMOVE-SOURCE-ID
;;;					  GET-KEYWORD-ID
;;;					  REMOVE-KEYWORD-ID
;;;					  ADD-DEFINITION-MOD-LINE
;;;					  ADD-DEFINITION-HEADER
;;;					  ADD-DEFINITION-ENTRY
;;;					  SET-SYSTEM-MOD-LINE
;;;					  SET-DEFINITION-CLEAR
;;;					  UPDATE-DEFINITION
;;;					  SPECIAL-BEGINNING-OF-BUFFER
;;;					  SPECIAL-SEARCH-FORWARD
;;;					  POSITION-DEFINITION-ON-PATCH
;;;					  SET-DEFINITION-TO-PATCH
;;;					  SET-KEYWORD-TO-PATCH
;;;					  SET-DEFINITION-UNDO
;;;					  ADD-MOD-FILE-HEADER
;;;					  SELECT-MODIF-REQUEST-FROM-FILE
;;;					  MAKE-MODIF-REQUEST-WITH-FILE
;;;					  GET-PATCH-APPLICABILITY
;;;					  MAKE-MODIFICATION-FROM-MAIL-FILE
;;;					  SEARCH-DEFINITION-IN-SOURCE-FILE
;;;					  DEFINITION-OPERATIONS
;;;					  FILE-OPERATIONS
;;;					  MOD-OPERATIONS
;;;					  SEND-MOD-OPERATIONS
;;;					  GENERIC-OPERATIONS
;;;	09/10/29	P. Madeira	Changed definitions
;;;					  SYSTEM2ORIGINAL-DIR
;;;					  SYSTEM2PATCHES-ORIGINAL-DIR
;;;	09/12/28	A. Frazao	Changed definitions
;;;					  LOAD-MODIFICATIONS
;;;	10/01/05	A. Frazao	Changed definitions
;;;					  ADD-MOD-FILE-HEADER
;;;	10/01/26	P. Madeira	Changed definitions
;;;					  LOAD-MODIFICATIONS
;;;					  SELECT-MODIF-REQUEST-ACTION
;;;	10/08/06	P. Madeira	Changed definitions
;;;					  MAKE-MODIFICATION-FROM-MAIL-FILE
;;;	10/09/06	Rui Patrocinio	Changed definitions
;;;					  PROCESS-MAIL-FILE
;;;-----------------------------------------------------------------------------

(require 'cl)

;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; 
;;; THE IMPLEMENTATION IS DIVIDED IN THE FOLLOWING SECTIONS (IN THE APPEARING
;;; ORDER):
;;;	- GLOBAL VARIABLES
;;;	- FILE TYPE
;;;	- MODIFICATION TYPE
;;;	- GENERIC STUFF
;;;	- UTILITIES
;;;	- PROCESSING OF SIGNATURES
;;;	- EDITION OF FILES
;;;	- SAVING MODIFICATIONS
;;;	- LOADING MODIFICATIONS
;;;	- OPERATIONS OVER MODIFICATIONS
;;;	- DISPLAYING A MODIFICATION
;;;	- SELECTION BY MEANS OF A MENU
;;;	- CHANGING A MODIFICATION'S BASIC DATA
;;;	- OPERATIONS OVER THE CURRENT FILE
;;;	- OPERATIONS OVER THE CURRENT MODIFICATION
;;;	- EDIFF STUFF
;;;	- GENERATION OF THE MODIFICATION HEADER
;;;	- CHANGING THE OCCURRENCES OF THE DEVELOPER DATE TO THE CURRENT DATE
;;;	- CREATING A MODIFICATION FROM A MAIL FILE
;;;	- GENERATING THE MAIL FILE
;;;	- PROCESSING FOR THE GENERATION OF THE MAIL FILE
;;;	- DISPLAYING THE EDITOR OF THE CURRENT MODIFICATION
;;;	- DISPLAYING THE CURRENT MODIFICATION
;;;	- DISPLAYING ALL MODIFICATIONS
;;;	- SELECTING A MODIFICATION
;;;	- CREATING AND SELECTING A MODIFICATION
;;;	- DELETING A MODIFICATION
;;;	- MENUS
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; GLOBAL VARIABLES
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;*CRM-VERSION*
;;;Description
;;;	The current version of the Change Request Manager.
;;;	The value of this variable shall be updated whenever there are any changes
;;;	done to the Change Request Manager.
;;;	The value shall be updated according to document "Version Numbering".
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	00/05/05	A. Vasconcelos	Created
;;;	00/06/06	A. Frazao	Changed to 1.0.1. A new file menu option
;;;					("Change modification") was added.
;;;	00/06/08	Dario		Changed to 1.0.2
;;;	00/06/30	Dario		Changed to 1.0.3
;;;	00/08/22	Dario		Changed to 1.0.4
;;;	00/09/06	A. Frazao	Changed to 1.1.0
;;;	01/01/05	Dario		Changed to 1.1.1
;;;	01/03/02	Toni		Changed to 1.2.0
;;;	01/04/20	Dario		Changed to 1.2.1
;;;	01/05/02	A. Vasconcelos	Changed to 1.2.2
;;;					Improvements in the calculations of system
;;;					versions to which a Change Request applies to.
;;;	01/09/27	A. Vasconcelos	Changed to 1.2.3.
;;;					Updated for new systems CREWS_ML and CREWS_DSB.
;;;	01/10/16	Toni		Changed to 1.2.4.
;;;					Added support for .js files.
;;;	05/10/13	J. P. Varandas	Changed to 1.3.0
;;;	08/07/08	A. Frazao	Changed to 1.4.0
;;;	09/02/10	J. P. Varandas	Changed to 1.5.0 - Generalised to handle different products than CREWS
;;;-----------------------------------------------------------------------------
(defvar *crm-version* "1.5.0")

;;;-----------------------------------------------------------------------------
;;;MODIF REQUEST VARIABLES
;;;Description
;;;	Sets the values of the modif request variables
;;;	
;;;	\visibility
;;;		SI
;;;History
;;;	Date		Author		Description
;;;	96/08/12	A. Frazao	Added *EMACS-INIT-FILE*
;;;	97/08/29	A. Frazao	Added *DEFAULT-ORG-DIRS*
;;;	99/10/19	J. P. Varandas	Added *CREWS-MAIL-NAME*, *CREWS-MAIL-VERSIONS*
;;;					and *CREWS-MAIL-VERSION-NAME*
;;;	05/10/13	J. P. Varandas	Deleted definitions
;;;					  *CREWS-REVERTED-MAIL*
;;;					  *CREWS-MAIL-VERSION-NAME*
;;;					  *CREWS-MAIL-VERSIONS*
;;;	08/07/08	A. Frazao	Changed commented values
;;;	09/02/10	J. P. Varandas	Changed variable names:
;;;					  *crews-mod-file* -> *modif-request-file*
;;;					  *all-crews-mods* -> *all-modif-requests*
;;;					  *current-crews-mod* -> *current-modif-request*
;;;					  *crews-big-brother-file* -> *actions-log-file*
;;;					  *crews-mail-dir* -> *modifs-mail-dir*
;;;					  *crews-mail-from* -> *modif-mail-from*
;;;					  *crews-mail-name* -> *modif-mail-name*
;;;-----------------------------------------------------------------------------
(defvar *default-org-dir*        nil)	;;	"x:/siscog"
(defvar *default-src-dir*        nil)	;;	(getenv "SISCOG_DIR")
(defvar *default-mod-dir*        nil)	;;	(format "%s/alteracoes" (getenv "SISCOG_DIR"))
(defvar *default-mod-date*       nil)	;;	"99/12/31"
(defvar *default-author*         nil)	;;	"João Silva"
(defvar *modif-request-file*     nil)	;;	(format "%s/alteracoes/.modif-requests" (getenv "SISCOG_DIR"))
;; NIL para nao informar o BIG-BROTHER...
(defvar *actions-log-file*       nil)	;;	(format "%s/alteracoes/.big-brother-file" (getenv "SISCOG_DIR"))
(defvar *modifs-mail-dir*        nil)	;;	"z:/siscog/changes-requests"
(defvar *modif-mail-name*        nil)   ;;      "modif-request-mail"
(defvar *modif-mail-from*        nil)	;;	"joaos"

(defvar *emacs-program*          nil)	;;	"emacs"
(defvar *emacs-init-file*        nil)	;;	(format "%s/init.el" (getenv "SISCOG_EMACS_DIR"))

(defvar *all-modif-requests*         nil)
(defvar *current-modif-request*      nil)

(defvar *current-x-arg* nil)

;; Status variables
(defvar ERR_UNKNOWN 0)
(defvar ERR_CORRECT 1)
(defvar ERR_DEL_FILE 2)
(defvar ERR_NEW_FILE 3)
(defvar ERR_NO_SRC_FILE 4)
(defvar ERR_NO_MOD_FILE 5)
(defvar ERR_NO_ORG_FILE 6)
(defvar ERR_SIGNATURE 7)

;;;-----------------------------------------------------------------------------
;;;Mode variables
;;;Description
;;;	Mode variables.
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	Added UNIX-MODE
;;;	99/01/29	A. Frazao	Added BAT-MODE
;;;	01/03/02	Toni		Added HTML_MODE, PHP-MODE and VB-MODE.
;;;	01/10/16	Toni		Added JSCRIPT-MODE.
;;;	08/07/29	A. Frazao	Added XML-MODE
;;;	09/06/30	Ana Afonso	Added CERT-MODE (POA 16095.0)
;;;-----------------------------------------------------------------------------
(defvar C-MODE       0)
(defvar LISP-MODE    1)
(defvar EMACS-MODE   2)
(defvar DIC-MODE     3)
(defvar UNIX-MODE    4)
(defvar BAT-MODE     5)
(defvar HTML-MODE    6)
(defvar PHP-MODE     7)
(defvar VB-MODE      8)
(defvar JSCRIPT-MODE 9)
(defvar XML-MODE    10)
(defvar CERT-MODE   11)


;;;-----------------------------------------------------------------------------
;;;Language variables
;;;Description
;;;	Indicate the programming language of a definition.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar UNKNOWN-LANGUAGE  0)
(defvar LISP-LANGUAGE     1)
(defvar C-LANGUAGE        2)
(defvar HTML-LANGUAGE     3)
(defvar JSCRIPT-LANGUAGE  4)
(defvar PHP-LANGUAGE      5)
(defvar VB-LANGUAGE       6)

;;;-----------------------------------------------------------------------------
;;;G-DEFINITION-LANGUAGE
;;;Description
;;;	Indicates the programming language of the definition where \emph{point}
;;;	lies on. 
;;;	
;;;	It is an helper of \elem{get-definition-language}. It avoids the most
;;;	time consuming processing of \elem{get-definition-language} when the
;;;	correct result was already obtained in a previous call.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar g-definition-language nil)

;;;-----------------------------------------------------------------------------
;;;G-POSITION-OF-END-OF-DEFINITION
;;;Description
;;;	Indicates the end position of the definition where \emph{point} lies on.
;;;	
;;;	It is an helper of \elem{end-of-definition}. It avoids the most time
;;;	consuming processing of \elem{end-of-definition} when the correct
;;;	result was already obtained in a previous call.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar g-position-of-end-of-definition nil)

;;;-----------------------------------------------------------------------------
;;;G-END-PREVIOUS-BUFFER-SIZE
;;;Description
;;;	Indicates the size that the current buffer had at the time of the last
;;;	call to \elem{end-of-definition}.
;;;	
;;;	It is an helper of \elem{end-of-definition}. It avoids the most time
;;;	consuming processing of \elem{end-of-definition} when the correct
;;;	result was already obtained in a previous call.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar g-end-previous-buffer-size nil)

;;;-----------------------------------------------------------------------------
;;;G-POSITION-OF-BEGINNING-OF-DEFINITION
;;;Description
;;;	Indicates the beginning position of the definition where \emph{point}
;;;	lies on.
;;;	
;;;	It is an helper of \elem{beginning-of-definition}. It avoids the most
;;;	time consuming processing of \elem{beginning-of-definition} when the
;;;	correct result was already obtained in a previous call.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar g-position-of-beginning-of-definition nil)

;;;-----------------------------------------------------------------------------
;;;G-BEG-PREVIOUS-BUFFER-SIZE
;;;Description
;;;	Indicates the size that the current buffer had at the time of the last
;;;	call to \elem{beginning-of-definition}.
;;;	
;;;	It is an helper of \elem{beginning-of-definition}. It avoids the most
;;;	time consuming processing of \elem{beginning-of-definition} when the
;;;	correct result was already obtained in a previous call.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar g-beg-previous-buffer-size nil)

;;;-----------------------------------------------------------------------------
;;;General regular expressions
;;;Description
;;;	Regular expressions typically independent of the programming language.
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar re-0-or-more-whitespaces "\\([ \t\n]\\)*")
(defvar re-1-or-more-whitespaces "\\([ \t\n]\\)+")

;;;-----------------------------------------------------------------------------
;;;Regular expressions for JavaScript
;;;Description
;;;	Regular expressions for parsing JavaScript.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar re-jscript-name                 "[a-zA-Z_][a-zA-Z_0-9]*")
(defvar re-jscript-var-decl-with-var    (format "^var%s%s"
						re-1-or-more-whitespaces
						re-jscript-name))
(defvar re-jscript-var-decl-without-var (format "^%s%s="
						re-jscript-name
						re-0-or-more-whitespaces))
(defvar re-jscript-var-decl             (format "\\(%s\\)\\|\\(%s\\)"
						re-jscript-var-decl-with-var 
						re-jscript-var-decl-without-var))
(defvar re-jscript-fn-arg               (format "%s%s%s[,)]"
						re-0-or-more-whitespaces
						re-jscript-name
						re-0-or-more-whitespaces))
(defvar re-jscript-fn-args-list         (format "(\\(%s\\)*"
						re-jscript-fn-arg))
(defvar re-jscript-fn-header            (format "^function%s%s%s%s"
						re-1-or-more-whitespaces
						re-jscript-name
						re-0-or-more-whitespaces
						re-jscript-fn-args-list))

;;;-----------------------------------------------------------------------------
;;;Regular expressions for PHP
;;;Description
;;;	Regular expressions for parsing PHP.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar re-php-name         "[a-zA-Z_][a-zA-Z_0-9]*")
(defvar re-php-const-decl   (format "^define%s(%s\"%s\"%s,"
				    re-0-or-more-whitespaces
				    re-0-or-more-whitespaces
				    re-php-name
				    re-0-or-more-whitespaces))
(defvar re-php-var-decl     (format "^\\$%s%s="
				    re-php-name
				    re-0-or-more-whitespaces))
(defvar re-php-fn-arg       (format "%s\\$%s%s[,)]"
				    re-0-or-more-whitespaces
				    re-php-name
				    re-0-or-more-whitespaces))
(defvar re-php-fn-args-list (format "(\\(%s\\)*"
				    re-php-fn-arg))
(defvar re-php-fn-header    (format "^function%s%s%s%s"
				    re-1-or-more-whitespaces
				    re-php-name
				    re-0-or-more-whitespaces
				    re-php-fn-args-list))

;;;-----------------------------------------------------------------------------
;;;Regular expressions for VBScript
;;;Description
;;;	Regular expressions for parsing VBScript.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar re-vb-name         "[a-zA-Z][a-zA-Z_0-9]*")
(defvar re-vb-const-decl   (format "^Const%s%s%s="
				   re-1-or-more-whitespaces
				   re-vb-name
				   re-0-or-more-whitespaces))
(defvar re-vb-var-decl     (format "^Dim%s%s"
				   re-1-or-more-whitespaces
				   re-vb-name
				   re-0-or-more-whitespaces))
(defvar re-vb-fn-arg       (format "%s%s%s[,)]"
				   re-0-or-more-whitespaces
				   re-vb-name
				   re-0-or-more-whitespaces))
(defvar re-vb-fn-args-list (format "(\\(%s\\)*"
				   re-vb-fn-arg))
(defvar re-vb-fn-header    (format "^\\(Function\\)\\|\\(Sub\\)%s%s%s\\(%s\\)?"
				   re-1-or-more-whitespaces
				   re-vb-name
				   re-0-or-more-whitespaces
				   re-vb-fn-args-list))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; FILE TYPE
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
(defun make-mod-file (src mod del new status)
  (list src mod del new status))

(defun file-src (file)
  (car file))

(defun file-mod (file)
  (cadr file))

(defun set-file-mod (file name)
  (setf (cadr file) name))

(defun file-del (file)
  (caddr file))

(defun file-new (file)
  (cadddr file))

(defun file-status (file)
  (car (cddddr file)))

(defun delete-file-p (file)
  (= (caddr file) 1))

(defun new-file-p (file)
  (= (cadddr file) 1))

(defun set-mod-file-status (file status)
  (setf (car (cddddr file)) status))

(defun print-mod-file (file)
  (cond ((delete-file-p file)
	 (format "%s :DEL" (file-src file)))
	((new-file-p file)
	 (format "%s %s :NEW" (file-src file) (file-mod file)))
	(t (format "%s %s" (file-src file) (file-mod file)))))

(defun file-src< (file1 file2)
  (string< (file-src file1) (file-src file2)))

(defun file-mod< (file1 file2)
  (if (or (delete-file-p file1)
	  (delete-file-p file2))
      nil
      (string< (file-mod file1) (file-mod file2))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; MODIFICATION TYPE
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;DEFSTRUCT MODIF-REQUEST
;;;Description
;;;	Struture to support a modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\class-attrs
;;;		\emph{name} is a \emph{string} with the name of modification.
;;;	
;;;		\emph{files} is a list of modification files.
;;;	
;;;		\emph{patch} is a \emph{string} indicating the name of the patch.
;;;	
;;;		\emph{author} is a \emph{string} indicating the name of the author
;;;	
;;;		\emph{org-dir} is a \emph{pathname} indicating the SISCOG original folder.
;;;	
;;;		\emph{src-dir} is a \emph{pathname} indicating the USER source folder.
;;;	
;;;		\emph{dir} is a \emph{pathname} indicating where the modification folder.
;;;	
;;;		\emph{date} is a \emph{date}
;;;
;;;		\emph{mail-file} is a \emph{string}, the pathname of the mail file.
;;;
;;;		\emph{mail-sent} is a \emph{boolean}, stating if the mail has been sent.
;;;	
;;;		\emph{reference} is a \emph{string}.
;;;	
;;;		\emph{versions} is a list of \emph{string}s indicating the versions for which the modification was made
;;;	
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Added PATCH
;;;	05/03/21	J. P. Varandas	Added REFERENCE
;;;	05/09/14	A. Frazao	Added mail-file
;;;	05/10/13	J. P. Varandas	Added VERSIONS
;;;	06/05/29	A. Frazao	Added mail-sent
;;;	09/02/10	J. P. Varandas	Changed struct name: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defstruct modif-request
  name
  files
  patch
  (author  *default-author*)
  (org-dir *default-org-dir*)
  (src-dir *default-src-dir*)
  (dir     *default-mod-dir*)
  (date    *default-mod-date*)
  mail-file
  mail-sent
  reference
  versions)

;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; GENERIC STUFF
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;CURRENT-MODIF-REQUEST
;;;Description
;;;	Returns the current modif request
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \elem{modif-request}
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed function name: current-crews-mod -> current-modif-request
;;;-----------------------------------------------------------------------------
(defun current-modif-request ()
  *current-modif-request*)

;;;-----------------------------------------------------------------------------
;;;SET-CURRENT-MODIF-REQUEST
;;;Description
;;;	Sets the current modif request
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;	\return-types
;;;		A \elem{modif-request}
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed function name: set-current-crews-mod -> set-current-modif-request
;;;-----------------------------------------------------------------------------
(defun set-current-modif-request (mod)
  (setf *current-modif-request* mod))

;;;-----------------------------------------------------------------------------
;;;ALL-MODIF-REQUESTS
;;;Description
;;;	Returns all modif requests
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of \elem{modif-request}s
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed function name: all-crews-mods -> all-modif-requests
;;;-----------------------------------------------------------------------------
(defun all-modif-requests ()
  *all-modif-requests*)

;;;-----------------------------------------------------------------------------
;;;SET-ALL-MODIF-REQUESTS
;;;Description
;;;	Sets all modif requests
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mods} is a list of \elem{modif-request}
;;;		
;;;	\return-types
;;;		A list of \elem{modif-request}
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed function name: set-all-crews-mods -> set-all-modif-requests
;;;-----------------------------------------------------------------------------
(defun set-all-modif-requests (mods)
  (setq *all-modif-requests* mods))

;;;-----------------------------------------------------------------------------
;;;ADD-MODIF-REQUEST
;;;Description
;;;	Appends the given modification to the list of modifications
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Appends the new modification at the end of the list
;;;	09/02/10	J. P. Varandas	Changed function name: add-crews-mod -> add-modif-request
;;;-----------------------------------------------------------------------------
(defun add-modif-request (mod)
  (setf *all-modif-requests* (nconc *all-modif-requests* (list mod))))


;;;-----------------------------------------------------------------------------
;;;DELETE-MODIF-REQUEST
;;;Description
;;;	Deletes a modification from the list of modification, deleting also 
;;;	the respective modification files.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Also deletes the respective modification files and patch.
;;;	09/02/10	J. P. Varandas	Changed function name: delete-crews-mod -> delete-modif-request
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun delete-modif-request (mod)
  (let ((mod-dir (modif-request-dir mod)))
    (dolist (file (modif-request-files mod))
      (sc-delete-file (format "%s/%s" mod-dir (file-mod file))))
    (sc-delete-file (format "%s/%s" mod-dir (modif-request-patch mod))))
  (if (eq mod *current-modif-request*)
      (setq *current-modif-request* nil))
  (setq *all-modif-requests* (delete mod *all-modif-requests*)))

;;;-----------------------------------------------------------------------------
;;;CURRENT-MOD-AUTHOR
;;;Description
;;;	Returns the modification author
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun current-mod-author ()
  (if (current-modif-request)
      (modif-request-author (current-modif-request))
      *default-author*))


;;;-----------------------------------------------------------------------------
;;;CURRENT-MOD-DATE-STRING
;;;Description
;;;	Returns the modification date string
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun current-mod-date-string ()
  (if (current-modif-request)
      (modif-request-date (current-modif-request))
      *default-mod-date*))


;;;-----------------------------------------------------------------------------
;;;CURRENT-MOD-REFERENCE
;;;Description
;;;	Returns the modification reference.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	05/03/21	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun current-mod-reference ()
  (if (current-modif-request)
      (modif-request-reference (current-modif-request))))


;;;-----------------------------------------------------------------------------
;;;CURRENT-MOD-VERSIONS
;;;Description
;;;	Returns the modification versions.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun current-mod-versions ()
  (if (current-modif-request)
      (modif-request-versions (current-modif-request))))

;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;					
;;;			   SPLIT MODIFICATION FUNCTIONALITY
;;;					
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SYSTEM-SOURCE-TOP-DIR
;;;Description
;;;	Returns the name of the top folder of a system where the VDEV code is 
;;;	stored. This name is independent from the origin of the code (source 
;;;	code or origin code).
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{cl-struct-sc-system}
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;	06/04/11	J. P. Varandas	Symplify the code
;;;-----------------------------------------------------------------------------
(defun system-source-top-dir (system)
  (format "%s-vdev" (downcase (sc-system-name system))))


;;;-----------------------------------------------------------------------------
;;;SYSTEM2ORIGINAL-DIR
;;;Description
;;;	Returns the complete path to the top folder of the given \arg{system}
;;;	and the given \arg{version}.
;;;	The root of this path corresponds to mapped drive of the stored code.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{cl-struct-sc-system}
;;;		
;;;		\arg{version} is a \emph{string}
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;		
;;;History
;;;	Date		Author		Description
;;;	06/04/11	J. P. Varandas	Created
;;;	08/12/31	P. Filipe	Added SISCOG-UTIL and CREWS-SISCOG
;;;	09/02/04	J. P. Varandas	sc-system-crews-name -> sc-system-company-name
;;;	09/02/10	J. P. Varandas	sc-siscog-top-dir -> sc-central-repository-dir
;;;					Generalised to handle any product
;;;	09/09/23	P. Madeira	Support "SISCOG-UTIL".
;;;					Support "CREWS-ML".
;;;	09/10/29	P. Madeira	Support "CREWS-SISCOG"
;;;-----------------------------------------------------------------------------
(defun system2original-dir (system version)
  (let ((name (sc-system-name system))
	(product-name (sc-product-external-id (sc-system-product system)))
	(drive (substring *sc-central-repository-dir* 0 1)))
    ;; MARTELADA: the best way is to define the root directory in the systems
    (cond ((sc-system-company-name system)
	   (cond ((string-equal name "CREWS-SISCOG")
		  (format "%s:/%s/%s-%s" drive "crews" (downcase name) version))
		 ((string-equal name "CREWS-ML")
		  (format "%s:/%s/%s/%s-%s" drive (sc-system-company-name system) (downcase name) (downcase name) version))
		 (t
		  (format "%s:/%s/%s-%s" drive (sc-system-company-name system) (downcase name) version))))
	  ((string-equal name "SISCOG-UTIL")
	   (format "%s:/%s/%s-%s" drive "crews" product-name version))
	  (t
	   (format "%s:/%s/%s-%s" drive product-name product-name version)))))


;;;-----------------------------------------------------------------------------
;;;SYSTEM2PATCHES-ORIGINAL-DIR
;;;Description
;;;	Returns the complete path to the patches folder of the given \arg{system}
;;;	and the given \arg{version}.
;;;	The root of this path corresponds to mapped drive of the stored code.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{cl-struct-sc-system}
;;;		
;;;		\arg{version} is a \emph{string}
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;	05/10/19	J. P. Varandas	The system CREWS is now stored in a different place
;;;	08/12/31	P. Filipe	Added SISCOG-UTIL and CREWS-SISCOG
;;;	09/02/04	J. P. Varandas	sc-system-crews-name -> sc-system-company-name
;;;	09/02/10	J. P. Varandas	sc-siscog-top-dir -> sc-central-repository-dir
;;;					Generalised to handle any product
;;;	09/02/17	J. P. Varandas	Deal with the special product "SISCOG-UTIL"
;;;	09/03/17	Alex E.Santo	Add system ML
;;;	09/10/29	P. Madeira	Support "CREWS-SISCOG"
;;;-----------------------------------------------------------------------------
(defun system2patches-original-dir (system version)
  (let ((name (sc-system-name system))
	(product-name (sc-product-external-id (sc-system-product system)))
	(drive (substring *sc-central-repository-dir* 0 1)))
    ;; MARTELADA: the best way is to define the root directory in the systems
    (cond ((sc-system-company-name system)
	   (cond ((string-equal name "CREWS-SISCOG")
		  (format "%s:/%s/%s-%s/patches/%s" drive "crews" (downcase name) version (downcase name)))
		 ((string-equal name "CREWS-ML")
		  (format "%s:/%s/%s/%s-%s/patches/%s" drive (sc-system-company-name system) (downcase name) (downcase name) version (downcase name)))
		 (t
		  (format "%s:/%s/%s-%s/patches/%s" drive (sc-system-company-name system) (downcase name) version (downcase name)))))
	  ((string-equal name "SISCOG-UTIL")
	   (format "%s:/%s/%s-%s/patches/%s" drive "crews" product-name version product-name))
	  (t
	   (format "%s:/%s/%s-%s/patches/%s" drive product-name product-name version product-name)))))


;;;-----------------------------------------------------------------------------
;;;SELECT-SYSTEM-PATCH-VERSION
;;;Description
;;;	Asks the user to select which version he wants to select for the given 
;;;	\arg{system}.
;;;	The user may give up.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{cl-struct-sc-system}
;;;		
;;;	\return-types
;;;		A \emph{string} or NIL
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;	06/04/11	J. P. Varandas	beep-confirm -> x-beep-confirm
;;;-----------------------------------------------------------------------------
(defun select-system-patch-version (system)
  (let ((versions (sc-system-versions system))
	(version nil)
	(found   nil)
	(items   nil))
    (dolist (version versions)
      (setf items (cons (sc-make-menu-item version version) items)))
    (while (not found)
      (setf version (sc-popup-menu *current-x-arg* (format "Select %s version" (sc-system-name system)) items))
      (if version
	  (setf found t)
	  (setf found (x-beep-confirm (format "Select %s version" (sc-system-name system))
				      "You did not selected any version." "Do you want to abort the patch splitting for this company?"))))
    version))


;;;-----------------------------------------------------------------------------
;;;SEARCH-EXACTLY
;;;Description
;;;	Searchs in the current buffer for a word (a string surrounded by spaces)
;;;	equal to the given \arg{string}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{string} is a \emph{string}
;;;		
;;;	\return-types
;;;		A \emph{boolean}
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun search-exactly (string)
  (let ((found nil))
    (while (and (not found)
		(search-forward string nil t))
      (save-excursion 
	(let ((point (point))) (next-space) (when (= point (point)) (setf found t)))))
    found))


;;;-----------------------------------------------------------------------------
;;;SPLIT-BY-SYSTEM-PATCHES
;;;Description
;;;	Splits the given list of files based on the description of the file 
;;;	PATCHES-DESCRIPTION.txt corresponding to the given \elem{system} and 
;;;	a selected version.
;;;	Creates modifications for each sublist of files and for the remaing files.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{cl-struct-sc-system}
;;;		
;;;		\arg{files} is a list of modification files.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun split-by-system-patches (system files)
  (let ((files&patches nil)
	(version (select-system-patch-version system)))
    (when version
      (let ((src-dir (system2patches-original-dir system version))
	    (top-dir (system-source-top-dir system)))
	(let ((patches-description (first (directory-files src-dir nil "\\PATCHES-DESCRIPTION.txt"))))
	  (if patches-description
	      (let* ((patches-description (concat src-dir "/" patches-description))
		     (old-buf (get-file-buffer patches-description))
		     (buf (or old-buf
			      (find-file-noselect patches-description))))
		(when buf
		  (set-buffer buf)
		  (dolist (file files)
		    (let* ((file-src (file-src file))
			   (file-dir (file-name-directory file-src))
			   (patch-name nil))
		      (setf file-src (substring file-src (+ (length top-dir) 1)))
		      (setf file-dir (substring file-dir (+ (length top-dir) 1) (- (length file-dir) 1)))
		      (beginning-of-buffer)
		      (when (or (search-exactly file-src)
				(search-exactly file-dir))
			(when (search-backward "Patch:" nil t 1)
			  (next-space)
			  (next-non-space)
			  (let* ((start (point))
				 (end start))
			    (next-space)
			    (setq end (point))
			    (when (and start end (not (= start end)))
			      (setq patch-name (buffer-substring start end)))
			    (when patch-name
			      (let ((place (member* patch-name files&patches :key 'car :test 'string-equal)))
				(if place
				    (setf (cadar place) (cons file (cadar place)))
				    (setf files&patches (cons (list patch-name (list file)) files&patches))))
			      (setf files (remove file files))))))))
		  
		  (unless old-buf
		    (kill-buffer buf))))
	      (beep-message "The file PATCHES-DESCRIPTION.txt does not exists!" "No splitting by patch will be made")))))
    
    (when files&patches
      (create-splited-modifications system version files&patches nil))
    
    (when files
      (create-splited-modifications system version (list (list nil files)) nil))
    ))
    


;;;-----------------------------------------------------------------------------
;;;CREATE-SPLITED-MODIFICATIONS
;;;Description
;;;	Creates modifications for a list of modification files. It also set the 
;;;	referenced patch (if any).
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{modif-request}
;;;		
;;;		\arg{version} is a \emph{string}
;;;		
;;;		\arg{files&patches} is a list of pairs (patch-name, list of modification files)
;;;		
;;;		\arg{module} is a \emph{string} or NIL
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun create-splited-modifications (system version files&patches module)
  (let* ((mod (current-modif-request))
	 (vdev (upcase (sc-system-name system)))
	 (vdev-name (format "%s-VDEV" vdev))
	 (version-name (and version (format "%s-%s" vdev (upcase version)))))
    (dolist (elem files&patches)
      (let ((patch-name (car elem))
	    (modif-files (cadr elem)))
	(let ((new-mod (make-modif-request :name (if patch-name
						     (format "%s - %s - (new %s)" (modif-request-name mod) vdev patch-name)
						     (if module
							 (format "%s - %s (%s)" (modif-request-name mod) vdev module)
							 (format "%s - %s" (modif-request-name mod) vdev)))
					   :files     modif-files
					   :author    (modif-request-author mod)
					   :org-dir   (modif-request-org-dir mod)
					   :src-dir   (modif-request-src-dir mod)
					   :dir       (modif-request-dir mod)
					   :reference (modif-request-reference mod)
					   :versions  (list vdev-name version-name)
					   :date      (modif-request-date mod))))
	  (add-modif-request new-mod)
	  (save-modifications)
	  (when patch-name
	    (set-mod-file-to-patch2 system version new-mod patch-name)))))))


;;;-----------------------------------------------------------------------------
;;;SET-MOD-FILE-TO-PATCH2
;;;Description
;;;	Sets the referenced patch to the given modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{cl-struct-sc-system}
;;;		
;;;		\arg{version} is a \emph{string}
;;;		
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;		\arg{patch-name} is a \emph{string}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-mod-file-to-patch2 (system version mod patch-name)
  (let ((buf (car (find-file-noselect (format "%s/%s*.lisp" (system2patches-original-dir system version) patch-name) nil nil t))))
    (when buf
      (let* ((name (file-name-sans-extension (file-name-nondirectory (buffer-file-name buf))))
	     (new-name (cond ((string-match "-[0-9][0-9][0-9][0-9]$" name)
			      (format "%s-1.lisp" name))
			     ((string-match "-[0-9][0-9][0-9][0-9]-[0-9]*$" name)
			      (string-match "-[0-9]*$" name)
			      (let ((basic-name (substring name 0 (1+ (match-beginning 0))))
				    (number (car (read-from-string (substring name (1+ (match-beginning 0)) (match-end 0))))))
				(format "%s%d.lisp" basic-name (1+ number))))))
	     (patch (get-patch-file mod new-name))
	     (mod-dir (modif-request-dir mod))
	     (filename (buffer-file-name buf)))
	(sc-copy-file filename (format "%s/%s" mod-dir patch)))
      (kill-buffer buf))))


;;;-----------------------------------------------------------------------------
;;;SPLIT-MODIF-REQUEST-BY-PATCHES
;;;Description
;;;	Splits the current modification into other modifications according the 
;;;	company, patch and GUI module.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{splitp} is a \emph{boolean}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed function name: split-crews-mod-by-patches -> split-modif-request-by-patches
;;;					Changed ADT: crews-mod -> modif-request
;;;					Removed split by gui modules
;;;					Use function 'sc-rplan-system?'
;;;-----------------------------------------------------------------------------
(defun split-modif-request-by-patches (&optional dont-splitp)
  (let ((*current-x-arg* t))
    (let* ((mod (current-modif-request))
	   (files (copy-seq (modif-request-files mod))))
      (dolist (system *sc-all-systems*)
	(when (sc-rplan-system? system)
	  (let ((dir (system-source-top-dir system))
		(system-files nil))
	    (let ((match (format "^%s/" dir)))
	      (dolist (file (copy-seq files))
		(when (string-match match (file-src file))
		  (setf system-files (cons file system-files))
		  (setf files (remove file files)))))
	    (when system-files
	      (if (not dont-splitp)
		  (split-by-system-patches system system-files)
		  (create-splited-modifications system nil (list (list nil system-files)) nil))))))
      (if files
	  (setf (modif-request-files mod) files)
	  (delete-modif-request mod))
      (save-modifications))))


;;;-----------------------------------------------------------------------------
;;;SPLIT-MODIF-REQUEST
;;;Description
;;;	Split the current modification into modifications for the companies 
;;;	envolved add the company name as sufix to the current modification name
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;	02/07/18	Carlos Ribeiro	Update for CREWS-BRISA.
;;;	03/02/05	A. Frazao	Update for CREWS-SISCOG.
;;;	05/10/13	J. P. Varandas	Use function 'split-crews-mod-by-patches'
;;;	09/02/10	J. P. Varandas	Changed names: split-crews-mod -> split-modif-request
;;;-----------------------------------------------------------------------------
(defun split-modif-request ()
  (split-modif-request-by-patches t))


;;;-----------------------------------------------------------------------------
;;;SET-MODIF-REQUEST-VERSION
;;;Description
;;;	Allow to set the system and version to the current modification based 
;;;	on the user selection.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	06/04/11	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed function name: set-mod-crews-version -> set-modif-request-version
;;;					Changed ADT: crews-mod -> modif-request
;;;					Use function 'sc-rplan-system?'
;;;	09/02/17	J. P. Varandas	Deal with the special product "SISCOG-UTIL"
;;;-----------------------------------------------------------------------------
(defun set-modif-request-version ()
  (let ((*current-x-arg* t))
    (let* ((mod (current-modif-request))
	   (files (modif-request-files mod))
	   (unique-system nil))
      (block exit
	(dolist (system *sc-all-systems*)
	  (when (or (sc-rplan-system? system)
		    (string-equal (sc-system-name system) "SISCOG-UTIL"))
	    (let* ((dir (system-source-top-dir system))
		   (match (format "^%s/" dir)))
	      (when (string-match match (file-src (first files)))
		(setf unique-system system)
		(dolist (file (rest files))
		  (unless (string-match match (file-src file))
		    (setf unique-system nil)))
		(return-from exit))))))
      (if unique-system
	  (let ((version (select-system-patch-version unique-system)))
	    (when version
	      (let* ((vdev (upcase (sc-system-name unique-system)))
		     (vdev-name (format "%s-VDEV" vdev))
		     (version-name (format "%s-%s" vdev (upcase version))))
		(setf (modif-request-versions mod) (list vdev-name version-name))
		(list unique-system version)
		)))
	  (progn
	    (x-beep-confirm "The files of the modification do not belong to the same system!")
	    nil)))))


;;;-----------------------------------------------------------------------------
;;;GET-PATCH-FOR-VERSION
;;;Description
;;;	Based on the file patches-description of the user selected system version 
;;;	sets the most apropriated patch to the current modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	06/04/11	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun get-patch-for-version ()
  (let ((mod (current-modif-request))
	(system-version (set-modif-request-version)))
    (when system-version
      (let ((system  (first system-version))
	    (version (second system-version)))
	(let ((src-dir (system2patches-original-dir system version))
	      (top-dir (system-source-top-dir system)))
	  (let ((patches-description (first (directory-files src-dir nil "\\PATCHES-DESCRIPTION.txt"))))
	    (if patches-description
		(let* ((patches-description (concat src-dir "/" patches-description))
		       (old-buf (get-file-buffer patches-description))
		       (buf (or old-buf
				(find-file-noselect patches-description)))
		       (patch nil))
		  (when buf
		    (set-buffer buf)
		    (block exit
		      (dolist (file (modif-request-files mod))
			(let* ((file-src (file-src file))
			       (file-dir (file-name-directory file-src))
			       (patch-name nil))
			  (setf file-src (substring file-src (+ (length top-dir) 1)))
			  (setf file-dir (substring file-dir (+ (length top-dir) 1) (- (length file-dir) 1)))
			  (beginning-of-buffer)
			  (when (or (search-exactly file-src)
				    (search-exactly file-dir))
			    (when (search-backward "Patch:" nil t 1)
			      (next-space)
			      (next-non-space)
			      (let* ((start (point))
				     (end start))
				(next-space)
				(setq end (point))
				(when (and start end (not (= start end)))
				  (setq patch-name (buffer-substring start end)))
				(when patch-name
				  (if patch
				      (unless (string-equal patch-name patch)
					(setf patch nil)
					(beep-message "The file of the modification belong to different patches!")
					(return-from exit))
				      (setf patch patch-name)))))))))
		    (unless old-buf
		      (kill-buffer buf)))
		  (when patch
		    (setf (modif-request-patch mod) nil)
		    (beep-message (format "Setting %s for version %s-%s" patch (sc-system-name system) version))
		    (set-mod-file-to-patch2 system version mod patch)))
		(beep-message "The file PATCHES-DESCRIPTION.txt does not exists!" "No splitting by patch will be made"))))))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; UTILITIES
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;LOOKING-LITERALLY-AT
;;;Description
;;;	Determines whether the contents of the current file starting at
;;;	\emph{point} are equal to \elem{string} or not.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{string} is a string.
;;;		
;;;		\arg{case-sensitive} is a boolean. It indicates whether case is
;;;		relevant in the comparison or not. When \emph{t} it means that
;;;		case is relevant. When \emph{nil} case is ignored.
;;;		
;;;	\return-types
;;;		A boolean. When \emph{t} it means that the contents of the
;;;		current file starting at \emph{point} are equal to
;;;		\elem{string}. When \emph{nil} it means the opposite.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun looking-literally-at (string &optional case-sensitive)
  (let* ((string-length (length string))
	 (line          (buffer-substring (point) (line-end-position)))
	 (line-length   (length line)))
    
    (when (not case-sensitive)	
      (setq string (downcase string))
      (setq line (downcase line)))
    
    (if (and (>= line-length string-length)
	     (string-equal (substring line 0 string-length) string))
	t
	nil)))

;;;-----------------------------------------------------------------------------
;;;MY-END-OF-BUFFER
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun my-end-of-buffer ()
  (if *windows-emacs*
      (goto-char (point-max))
      (end-of-buffer)))

;;;-----------------------------------------------------------------------------
;;;FILE-MODE
;;;Description
;;;	Indicates the mode of the current file. 
;;;	
;;;	The mode of a file depends on the corresponding file name extension.
;;;	
;;;	Such mode is a concept defined for the purpose of the software herein
;;;	implemented. There is not necessarily a relation with Emacs major or
;;;	minor modes.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		An integer. One of C-MODE, LISP-MODE, EMACS-MODE, DIC-MODE,
;;;		UNIX-MODE, BAT-MODE, HTML-MODE, JSCRIPT-MODE, PHP-MODE, VB-MODE,
;;;		XML-MODE
;;;		
;;;History
;;;	Date		Author		Description
;;;	97/01/01	Joao Filipe	*.h e *.x sao tambem C-MODE
;;;	97/07/25	A. Frazao	Added makefile
;;;	98/11/04	A. Frazao	Added .data, .def, .cl and .cpp
;;;	98/12/23	A. Frazao	Added .txt and .sql
;;;	99/01/29	A. Frazao	Added .bat
;;;	99/04/07	A. Frazao	Added .rul
;;;	99/08/31	J. P. Varandas	Added .bml and .lpr
;;;					Only calls 'buffer-file-name' once
;;;	01/03/02	Toni		Added .html, .htm, .php3, .php, .inc and
;;;					.asp.
;;;					Doc updated
;;;	01/10/16	Toni		Added .js.
;;;	08/07/29	A. Frazao	Added .css, .xsd and .xsl
;;;	09/02/25	Paulo Tomé	Removed '.php3'. Added .xml (POA 14402.0)
;;;	09/06/30	Ana Afonso	Added .pem and .crt (POA 16095.0)
;;;-----------------------------------------------------------------------------
(defun file-mode ()
  (let ((filename (buffer-file-name (current-buffer))))
    (when filename
      (cond ((string-match "\\.lisp$"  filename) LISP-MODE)
	    ((string-match "\\.def$"   filename) LISP-MODE)
	    ((string-match "\\.cl$"    filename) LISP-MODE)
	    ((string-match "\\.bil$"   filename) LISP-MODE)
	    ((string-match "\\.bml$"   filename) LISP-MODE)
	    ((string-match "\\.lpr$"   filename) LISP-MODE)
	    ((string-match "\\.c$"     filename) C-MODE)
	    ((string-match "\\.cpp$"   filename) C-MODE)
	    ((string-match "\\.h$"     filename) C-MODE)
	    ((string-match "\\.x$"     filename) C-MODE)
	    ((string-match "\\.txt$"   filename) C-MODE)
	    ((string-match "\\.sql$"   filename) C-MODE)
	    ((string-match "\\.rul$"   filename) C-MODE)
	    ((string-match "\\.dic$"   filename) DIC-MODE)
	    ((string-match "\\.data$"  filename) DIC-MODE)
	    ((string-match "makefile$" filename) UNIX-MODE)
	    ((string-match "\\.el$"    filename) EMACS-MODE)
	    ((string-match "\\.bat$"   filename) BAT-MODE)
	    ((string-match "\\.html$"  filename) HTML-MODE)
	    ((string-match "\\.htm$"   filename) HTML-MODE)
	    ((string-match "\\.js$"    filename) JSCRIPT-MODE)
	    ((string-match "\\.css$"   filename) JSCRIPT-MODE)
	    ((string-match "\\.php$"   filename) PHP-MODE)
	    ((string-match "\\.inc$"   filename) PHP-MODE)
	    ((string-match "\\.asp$"   filename) VB-MODE)
	    ((string-match "\\.xsd$"   filename) XML-MODE)
	    ((string-match "\\.xsl$"   filename) XML-MODE)
	    ((string-match "\\.xml$"   filename) XML-MODE)
	    ((string-match "\\.pem$"   filename) CERT-MODE)
	    ((string-match "\\.crt$"   filename) CERT-MODE)
	    (t nil)))))


;;;-----------------------------------------------------------------------------
;;;BEGINNING-OF-FILE-HEADER
;;;Description
;;;	Moves pointer to the point where the file header exists or should be inserted.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;
;;;	\remarks
;;;		Normally, it moves to the beginning of the buffer. However, some
;;;		files require a specific first line.
;;;
;;;History
;;;	Date		Author		Description
;;;	08/07/29	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun beginning-of-file-header ()
  (beginning-of-buffer)
  (when (eq (file-mode) XML-MODE)
    (let ((xml-first-line-p nil))
      (save-excursion
	(beginning-of-line)
	(let ((start (point)))
	  (end-of-line)
	  (let ((end (point)))
	    (let ((str (buffer-substring start end)))
	      (when (and (string-match "<\?xml" str)
			 (string-match "\?>$" str))
		(setf xml-first-line-p t))))))
      (when xml-first-line-p
	(forward-line)))))


;;;-----------------------------------------------------------------------------
;;;MODE-FILLERS
;;;Description
;;;	Defines fillers used in the construction of definition headers and file
;;;;	headers. The appropriate fillers depend on the file mode. For some
;;;	file modes the appropriate fillers depend also on the programming
;;;	language of the code region on which \emph{point} lies.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list with 8 elements with the following meaning:
;;;		\begin{itemize,numbered}
;;;			\item a string with the line start filler for definition
;;;				headers
;;;			\item a string with the line filler for definition
;;;				headers
;;;			\item a string with the start filler for definition
;;;				headers or \emph{nil}. When is a string it is a
;;;				multi-line comment open delimiter
;;;			\item a string with the end filler for definition
;;;				headers or \emph{nil}. When is a string it is a
;;;				multi-line comment close delimiter
;;;			\item a string with the line start filler for file
;;;				headers
;;;			\item a string with the line filler for file
;;;				headers
;;;			\item a string with the start filler for file
;;;				headers or \emph{nil}. When is a string it is a
;;;				multi-line comment open delimiter
;;;			\item a string with the end filler for file
;;;				headers or \emph{nil}. When is a string it is a
;;;				multi-line comment close delimiter
;;;		\end{itemize}
;;;		
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	Created
;;;	99/01/29	A. Frazao	Added BAT-MODE
;;;	01/03/02	Toni		Added support for HTML-MODE, PHP-MODE
;;;					and VB-MODE.
;;;					Extended the content of the list
;;;					containing mode fillers with 6 new
;;;					elements.
;;;					Doc updated
;;;	01/10/16	Toni		Added support for JSCRIPT-MODE.
;;;	08/07/29	A. Frazao	Added support for XML-MODE
;;;-----------------------------------------------------------------------------
(defun mode-fillers ()
  (let ((mode     (file-mode))
	(language (save-excursion (get-definition-language))))
    
    (cond ((eq mode UNIX-MODE)
	   '("###"   "###-----------------------------------------------------------------------------" nil nil
	     "###"   "###-----------------------------------------------------------------------------" nil nil))
	  
	  ((eq mode BAT-MODE)
	   '("@rem " "@rem ---------------------------------------------------------------------------" nil nil
	     "@rem " "@rem ---------------------------------------------------------------------------" nil nil))
	  
	  ((or (eq mode C-MODE) (eq mode JSCRIPT-MODE))
	   '(";;;" ";;;-----------------------------------------------------------------------------" "/*" "*/"
	     ";;;" ";;;-----------------------------------------------------------------------------" "/*" "*/"))

	  ((eq mode XML-MODE)
	   '(";;;" ";;;_____________________________________________________________________________" "<!--" "-->"
	     ";;;" ";;;_____________________________________________________________________________" "<!--" "-->"))

	  ((eq mode PHP-MODE)
	   (cond ((eq language HTML-LANGUAGE)
		  '(";;;" ";;;-----------------------------------------------------------------------------" "<?php /*" "*/ ?>"
		    ";;;" ";;;-----------------------------------------------------------------------------" "<?php /*" "*/ ?>"))
		 ((or (eq language JSCRIPT-LANGUAGE) (eq language PHP-LANGUAGE))
		  '(";;;" ";;;-----------------------------------------------------------------------------" "/*" "*/"
		    ";;;" ";;;-----------------------------------------------------------------------------" "<?php /*" "*/ ?>"))
		 ((eq language VB-LANGUAGE)
		  '("'''" "'''-----------------------------------------------------------------------------" nil nil
		    ";;;" ";;;-----------------------------------------------------------------------------" "<?php /*" "*/ ?>"))))
	  
	  ((or (eq mode HTML-MODE) (eq mode VB-MODE))
	   (cond ((eq language HTML-LANGUAGE)
		  '(";;;" ";;;-----------------------------------------------------------------------------" "<!--" "-->"
		    ";;;" ";;;-----------------------------------------------------------------------------" "<!--" "-->"))
		 ((or (eq language JSCRIPT-LANGUAGE) (eq language PHP-LANGUAGE))
		  '(";;;" ";;;-----------------------------------------------------------------------------" "/*" "*/"
		    ";;;" ";;;-----------------------------------------------------------------------------" "<!--" "-->"))
		 ((eq language VB-LANGUAGE)
		  '("'''" "'''-----------------------------------------------------------------------------" nil nil
		    ";;;" ";;;-----------------------------------------------------------------------------" "<!--" "-->"))))
	  
	  (t '(";;;" ";;;-----------------------------------------------------------------------------" nil nil
	       ";;;" ";;;-----------------------------------------------------------------------------" nil nil)))))


;;;-----------------------------------------------------------------------------
;;;X-BEEP-CONFIRM
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	
;;;	98/11/04	A. Frazao	Uses new dialog implementation
;;;-----------------------------------------------------------------------------
(defun x-beep-confirm (title &rest strs)
  (let ((items nil))
    (dolist (str strs)
      (setf items (cons (cons str t) items)))
    (setf items (nreverse items))
    (beep)
    (sc-dialog *current-x-arg* title strs '(("Yes" . t) ("No" . nil)))))

;;;-----------------------------------------------------------------------------
;;;BEEP-MESSAGE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	Pops a menu to display the message
;;;	98/11/04	A. Frazao	Uses new dialog implementation
;;;	99/08/31	J. P. Varandas	Allows more than one text line
;;;-----------------------------------------------------------------------------
(defun beep-message (&rest strs)
  (let ((items nil))
    (dolist (str strs)
      (setf items (cons (cons str t) items)))
    (setf items (nreverse items))
    (beep)
    (sc-dialog *current-x-arg* "Warning" strs '(("OK" . t)))))

;;;-----------------------------------------------------------------------------
;;;RUN-PROGRAM
;;;Description
;;;	Runs the given program with the given arguments
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{program} is a \emph{symbol}
;;;		
;;;		\arg{args} is a list with the arguments to start the process
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	*crews-mod-processes* -> *modif-request-processes*
;;;-----------------------------------------------------------------------------
(defun run-program (program &rest args)
  (let ((buff (get-buffer-create (concat "*modif-request-processes*"))))
    (apply 'start-process program buff program args)))

(defun next-str ()
  (let (start end)
    (next-non-space)
    (setf start (point))
    (next-space)
    (setf end (point))
    (buffer-substring start end)))

;;;	09/09/23	P. Madeira	`next-line' -> `forward-line' (EMACS 23)
(defun line-feed ()
  (forward-line 1)
  (beginning-of-line))

;;;-----------------------------------------------------------------------------
;;;GET-WEB-DEFINITION-LANGUAGE
;;;Description
;;;	Finds out what is the web programming language being used at
;;;	\emph{point}, which is supposedly the place of a definition.
;;;	
;;;	It is an helper of \elem{get-definition-language}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		An integer. One of PHP-LANGUAGE, VB-LANGUAGE, JSCRIPT-LANGUAGE
;;;		and HTML-LANGUAGE.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;	01/10/16	Toni		Added support for JSCRIPT-MODE.
;;;	09/09/23	P. Madeira	`previous-line' -> `forward-line' (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun get-web-definition-language ()
  (let ((debugging-this-function nil))

    ;; Code for debugging.
    (if debugging-this-function
	(message "ENTERING ----- get-web-definition-language ----- ENTERING"))
    
    (beginning-of-line)
    
    ;; JavaScript files can only contain code in the JavaScript language.
    (let ((mode (file-mode)))
      (when (eq mode JSCRIPT-MODE)
	(setq g-definition-language JSCRIPT-LANGUAGE)))
    
    (let ((line-prefix-max-length 10)
	  (initial-point          (point))
	  ;; The 'language' variable is initialised with the value of
	  ;; 'g-definition-variable', which is a global variable.
	  (language               (if g-definition-language
				      ;; The language of the definition is
				      ;; already known. It is stored in the
				      ;; 'g-definition-language' global
				      ;; variable.
				      g-definition-language
				      ;; The language of the definition is not
				      ;; known yet so a search will have to be
				      ;; done.
				      nil)))
      
      (do* ((line-prefix (downcase (buffer-substring (point)
						     (+ (point)
							(if (< (- (line-end-position) (point))
							       line-prefix-max-length)
							    ;; There are less than 'line-prefix-max-length'
							    ;; characters from (point) to the end of the
							    ;; line so it will have to pick a shorter
							    ;; string.
							    (- (line-end-position) (point))
							    ;; Ok.
							    line-prefix-max-length))))
			 ;; Step is equal to init.
			 (downcase (buffer-substring (point)
						     (+ (point)
							(if (< (- (line-end-position) (point))
							       line-prefix-max-length)
							    (- (line-end-position) (point))
							    line-prefix-max-length)))))
	    (line-prefix-length (length line-prefix) (length line-prefix)))
	  ((not (eq language nil)) language)
	
	(cond ((or (and (>= line-prefix-length 5)
			(string-equal (substring line-prefix 0 5) "<?php"))
		   (and (>= line-prefix-length 2)
			(string-equal (substring line-prefix 0 2)  "?>")
			(= (line-beginning-position) initial-point)))
	       ;; PHP language.
	       (setq language PHP-LANGUAGE))
	      
	      ((or (and (>= line-prefix-length 2)
			(string-equal (substring line-prefix 0 2) "<%"))
		   (and (>= line-prefix-length 2)
			(string-equal (substring line-prefix 0 2)  "%>")
			(= (line-beginning-position) initial-point)))
	       ;; VBScript language.
	       (setq language VB-LANGUAGE))
	      
	      ((or (and (>= line-prefix-length 7)
			(string-equal (substring line-prefix 0 7) "<script"))
		   (and (>= line-prefix-length 8)
			(string-equal (substring line-prefix 0 8)  "</script")
			(= (line-beginning-position) initial-point)))
	       ;; JavaScript language.
	       (setq language JSCRIPT-LANGUAGE))
	      
	      ((or (and (>= line-prefix-length 2)
			(string-equal (substring line-prefix 0 2)  "?>"))
		   (and (>= line-prefix-length 2)
			(string-equal (substring line-prefix 0 2)  "%>"))
		   (and (>= line-prefix-length 8)
			(string-equal (substring line-prefix 0 8)  "</script"))
		   (and (>= line-prefix-length 5)
			(string-equal (substring line-prefix 0 5)  "<body"))
		   (and (>= line-prefix-length 6)
			(string-equal (substring line-prefix 0 6)  "</body"))
		   (and (>= line-prefix-length 9)
			(string-equal (substring line-prefix 0 9) "<frameset"))
		   (and (>= line-prefix-length 10)
			(string-equal (substring line-prefix 0 10) "</frameset"))
		   (and (>= line-prefix-length 5)
			(string-equal (substring line-prefix 0 5)  "<head"))
		   (and (>= line-prefix-length 6)
			(string-equal (substring line-prefix 0 6)  "</head"))
		   (= (point) (point-min))) ;; Reached the beginning of buffer.
	       ;; HTML language.
	       (setq language HTML-LANGUAGE))
	      (t
	       ;; Cannot tell yet what is the language of the definition so moves
	       ;; point to the previous line.
	       (forward-line -1))))
      
      (when (not (eq language g-definition-language))
	;; Updates the global variable holding the language of the definition.
	(setq g-definition-language language))
    
      ;; Code for debugging.
      (when debugging-this-function
	(message "Language: %s" (cond ((eq language HTML-LANGUAGE) "HTML-LANGUAGE")
				      ((eq language JSCRIPT-LANGUAGE) "JSCRIPT-LANGUAGE")
				      ((eq language PHP-LANGUAGE) "PHP-LANGUAGE")
				      ((eq language VB-LANGUAGE) "VB-LANGUAGE")
				      (t "Unknown")))
	(message "LEAVING ----- get-web-definition-language ----- LEAVING"))
      
      language)))

;;;-----------------------------------------------------------------------------
;;;GET-DEFINITION-LANGUAGE
;;;Description
;;;	Finds out what is the programming language being used at \emph{point},
;;;	which is supposedly the place of a definition.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		An integer. One of LISP-LANGUAGE, C-LANGUAGE, PHP-LANGUAGE,
;;;		VB-LANGUAGE, JSCRIPT-LANGUAGE, HTML-LANGUAGE and
;;;		UNKNOWN-LANGUAGE.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;	01/10/16	Toni		Added support for JSCRIPT-MODE.
;;;-----------------------------------------------------------------------------
(defun get-definition-language ()
  (let ((debugging-this-function nil))

    ;; Code for debugging.
    (if debugging-this-function
	(message "ENTERING ----- get-definition-language ----- ENTERING"))
    
    (let ((file-mode (file-mode))
	  (language  nil))
      (setq language (cond ((eq file-mode LISP-MODE)
			    LISP-LANGUAGE)
			   ((eq file-mode C-MODE)
			    C-LANGUAGE)
			   ((or (eq file-mode HTML-MODE)
				(eq file-mode JSCRIPT-MODE)
				(eq file-mode PHP-MODE)
				(eq file-mode VB-MODE))
			    (save-excursion (get-web-definition-language)))
			   (t
			    UNKNOWN-LANGUAGE)))
    
      ;; Code for debugging.
      (if debugging-this-function
	  (message "LEAVING ----- get-definition-language ----- LEAVING"))
    
      language)))

;;;-----------------------------------------------------------------------------
;;;C-END-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the end of the C definition where \emph{point}
;;;	lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the end of the buffer.
;;;	
;;;	It is an helper of \elem{end-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created from piece of body inside
;;;					'set-definition-clear'.
;;;-----------------------------------------------------------------------------
(defun c-end-of-definition ()
  (end-of-defun)
  (backward-char))

;;;-----------------------------------------------------------------------------
;;;WEB-END-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the end of the Web definition where \emph{point}
;;;	lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the end of the buffer.
;;;	
;;;	It is an helper of \elem{end-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{language} is an integer. Possible values include
;;;		HTML-LANGUAGE, JSCRIPT-LANGUAGE, PHP-LANGUAGE and VB-LANGUAGE.
;;;		It tells which is the programming language of the definition
;;;		where \emph{point} lies on.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		A Web definition is one that is defined using a programming
;;;		language proper for the development of Web applications.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;	09/09/23	P. Madeira	`previous-line' -> `forward-line' (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun web-end-of-definition (language)
  (let ((debugging-this-function nil))

    ;; Code for debugging.
    (if debugging-this-function
	(message "ENTERING ----- web-end-of-definition ----- ENTERING"))
    
    (beginning-of-line)
    
    (let* ((mode-fillers (mode-fillers))
	   (comment      (nth 0 mode-fillers))
	   (filler       (nth 1 mode-fillers))
	   (reached-beginning-of-definition nil)
	   ;; This variable is updated through the search for the end of the
	   ;; definition. At each moment of the search it holds the position of
	   ;; the end of the definition as known so far. Thus, at the end of the
	   ;; search it holds the position of the end of the definition.
	   (position-of-end-of-definition (if g-position-of-end-of-definition
					      ;; The position of the end of the
					      ;; definition is already known.
					      ;; It is stored in the
					      ;; 'g-position-of-end-of-definition'
					      ;; global variable.
					      (progn
						(when (/= (buffer-size) g-end-previous-buffer-size)
						  (setq g-position-of-end-of-definition
						    (+ g-position-of-end-of-definition
						       (- (buffer-size) g-end-previous-buffer-size)))
						  (setq g-end-previous-buffer-size (buffer-size)))
						g-position-of-end-of-definition)
					      ;; The position of the end of
					      ;; the definition is not known yet
					      ;; so a search will have to be done.
					      (progn
						(setq g-end-previous-buffer-size (buffer-size))
						nil)))
	   (reached-end-of-definition     (not (eq position-of-end-of-definition nil))))
      
      ;; Searches down in the file, one line per iteration.
      (do* ((line (downcase (buffer-substring (point) (line-end-position)))
		  (downcase (buffer-substring (point) (line-end-position))))
	    (line-length (length line) (length line)))
	  (reached-end-of-definition)

	(cond ((= (point) (point-max))
	       ;; This is the last line of the file and is an empty line.
	       ;;
	       ;; The definition had to end before this line.
	       (if debugging-this-function (message "Line ( 0 ): %s" line))
	       (if (eq position-of-end-of-definition nil)
		   (setq position-of-end-of-definition (point)))
	       (setq reached-end-of-definition t))

	      ;;------------------------------------------------------------------
	      
	      ((= line-length 0)
	       ;; This is an empty line.
	       ;;
	       ;; Do nothing.
	       (if debugging-this-function (message "Line ( 1 ): %s" line)))

	      ;;------------------------------------------------------------------
	      
	      ((and (>= line-length 1)
		    (or (= (aref line 0) 32)  ;; Space character
			(= (aref line 0) 9))) ;; Tab character
	       ;; This line starts with a white space.
	       ;;
	       ;; Check if it is a line containing only white spaces.
	       (if debugging-this-function (message "Line ( 2 ): %s" line))
	       (if (string-match "[^ \t]" line)
		   (setq position-of-end-of-definition (line-end-position))))
	      
	      ;;------------------------------------------------------------------
	      
	      ((or (and (or (eq language JSCRIPT-LANGUAGE)
			    (eq language PHP-LANGUAGE))
			(and (>= line-length 2)
			     (string-equal (substring line 0 2) "/*")))
		   (and (eq language HTML-LANGUAGE)
			(and (>= line-length 4)
			     (string-equal (substring line 0 4) "<!--"))))
	       ;; This line starts with "/*" (not HTML-LANGUAGE) or "<!--"
	       ;; (HTML-LANGUAGE), i.e, starts a multi-line comment.
	       ;;
	       ;; Checks if it is the beginning of a definition header.
	       (if debugging-this-function (message "Line ( 3 ): %s" line))
	       (forward-line 1)
	       (let ((next-line (downcase (buffer-substring (point) (line-end-position)))))
		 (if (string-equal next-line filler)
		     ;; It is the beginning of a definition header.
		     (if position-of-end-of-definition
			 ;; It is the beginning of the definition header of the
			 ;; next definition.
			 (setq reached-end-of-definition t))
		     ;; It is the beginning of a multi-line comment inside the
		     ;; definition.
		     (progn
		       (if debugging-this-function (message "Line ( 3A): %s" line))
		       (forward-line -1)
		       (setq position-of-end-of-definition (line-end-position))
		       (forward-line))))
	       (forward-line -1))
	      
	      ;;------------------------------------------------------------------
	      
	      ((or (and (or (eq language JSCRIPT-LANGUAGE)
			    (eq language PHP-LANGUAGE))
			(and (>= line-length 2)
			     (string-equal (substring line 0 2) "*/")))
		   (and (eq language HTML-LANGUAGE)
			(and (>= line-length 3)
			     (string-equal (substring line 0 3) "-->"))))
	       ;; This line starts with "*/" (not HTML-LANGUAGE) or "-->"
	       ;; (HTML-LANGUAGE), i.e, ends a multi-line comment.
	       ;;
	       ;; Checks if it is the end of a definition header.
	       (if debugging-this-function (message "Line ( 4 ): %s" line))
	       (forward-line -1)
	       (let ((previous-line (downcase (buffer-substring (point) (line-end-position)))))
		 (if (string-equal previous-line filler)
		     ;; It is the end of a definition header.
		     (if position-of-end-of-definition
			 ;; It is the end of the definition header of the
			 ;; next definition.
			 (setq reached-end-of-definition t))
		     ;; It is the end of a multi-line comment inside the definition.
		     (progn
		       (if debugging-this-function (message "Line ( 4A): %s" line))
		       (forward-line 1)
		       (setq position-of-end-of-definition (line-end-position))
		       (forward-line -1))))
	       (forward-line 1))
	      
	      ;;------------------------------------------------------------------
	      
	      ((and (eq language VB-LANGUAGE)
		    (string-equal line filler))
	       ;; This line starts with a filler (VB-LANGUAGE), i.e, starts a
	       ;; single-line comment.
	       ;;
	       ;; Checks if it is the beginning of a definition header.
	       (if debugging-this-function (message "Line ( 5 ): %s" line))
	       (forward-line 1)
	       (let ((next-line (downcase (buffer-substring (point) (line-end-position)))))
		 (if (and (>= line-length (length comment))
			  (string-equal (substring next-line 0 (length comment)) comment))
		     ;; It is the beginning of a definition header.
		     (if position-of-end-of-definition
			 ;; It is the beginning of the definition header of the
			 ;; next definition.
			 (setq reached-end-of-definition t))
		     ;; Checks if it is the end of a definition header.
		     (progn
		       (forward-line -2)
		       (let ((previous-line (downcase (buffer-substring (point) (line-end-position)))))
			 (if (and (>= line-length (length comment))
				  (string-equal (substring previous-line 0 (length comment)) comment))
			     ;; It is the end of a definition header.
			     (if position-of-end-of-definition
				 ;; It is the end of the definition header of the
				 ;; next definition.
				 (setq reached-end-of-definition t))
			     ;; It is the beginning of a single-line comment inside
			     ;; the definition.
			     (progn
			       (if debugging-this-function (message "Line ( 5A): %s" line))
			       (forward-line 1)
			       (setq position-of-end-of-definition (line-end-position))
			       (forward-line -1))))
		       (forward-line 2))))
	       (forward-line -1))
	      
	      ;;------------------------------------------------------------------
	      
	      ((and (>= line-length (length comment))
		    (string-equal (substring line 0 (length comment)) comment))
	       ;; This line belongs to the definition header.
	       ;;
	       ;; Do nothing.
	       (if debugging-this-function (message "Line ( 6 ): %s" line)))

	      ;;------------------------------------------------------------------
	      
	      ((or (and (or (eq language JSCRIPT-LANGUAGE)
			    (eq language PHP-LANGUAGE))
			(or (and (>= line-length 1)
				 (or (= (aref line 0) 123)   ;; { character
				     (= (aref line 0) 125))) ;; } character
			    (and (>= line-length 2)
				 (string-equal (substring line 0 2) "//"))))
		   (and (eq language PHP-LANGUAGE)
			(and (>= line-length 1)
			     (= (aref line 0) 35))) ;; # character
		   (and (eq language VB-LANGUAGE)
			(or (and (>= line-length 12)
				 (string-equal (substring line 0 12) "end function"))
			    (and (>= line-length 7)
				 (string-equal (substring line 0 7) "end sub"))
			    (and (>= line-length 1)
				 (= (aref line 0) 39)) ;; ' character
			    (and (>= line-length 3)
				 (string-equal (substring line 0 3) "rem")))))
	       ;; This line starts either with the {, }, "end function" or "end sub"
	       ;; delimiter or with the "//", #, ', "rem" single-line comment.
	       ;;
	       (if debugging-this-function (message "Line ( 7 ): %s" line))
	       (setq position-of-end-of-definition (line-end-position)))
	      
	      ;;------------------------------------------------------------------
	      
	      ((and (not (eq language HTML-LANGUAGE))
		    (or (and (>= line-length 7)
			     (string-equal (substring line 0 7) "<script"))
			(and (>= line-length 8)
			     (string-equal (substring line 0 8) "</script"))
			(and (>= line-length 5)
			     (string-equal (substring line 0 5) "<?php"))
			(and (>= line-length 2)
			     (string-equal (substring line 0 2) "?>"))
			(and (>= line-length 2)
			     (string-equal (substring line 0 2) "<%"))
			(and (>= line-length 2)
			     (string-equal (substring line 0 2) "%>"))))
	       ;; This line starts or ends a code region.
	       ;;
	       ;; The definition had to end before this line.
	       (if debugging-this-function (message "Line ( 8 ): %s" line))
	       (if (eq position-of-end-of-definition nil)
		   (setq position-of-end-of-definition (point)))
	       (setq reached-end-of-definition t))
	      
	      ;;------------------------------------------------------------------
	      
	      ((and (eq language HTML-LANGUAGE)
		    (or (and (>= line-length 7)
			     (string-equal (substring line 0 7) "</head>"))
			(and (>= line-length 7)
			     (string-equal (substring line 0 7) "</body>"))
			(and (>= line-length 11)
			     (string-equal (substring line 0 11) "</frameset>"))))
	       ;; This line ends the definition.
	       ;;
	       (if debugging-this-function (message "Line ( 9 ): %s" line))
	       (setq position-of-end-of-definition (line-end-position))
	       (setq reached-end-of-definition t))
	      
	      ;;------------------------------------------------------------------
	      
	      ((eq language HTML-LANGUAGE)
	       ;; This line belongs to the definition.
	       ;;
	       (if debugging-this-function (message "Line (10 ): %s" line))
	       (setq position-of-end-of-definition (line-end-position)))
	      
	      ;;------------------------------------------------------------------
	      
	      (t ;; (not (eq language HTML-LANGUAGE))
	       ;; This line is either the beginning of the definition or the
	       ;; beginning of the next definition.
	       (if debugging-this-function (message "Line (11 ): %s" line))
	       (if (and (not reached-beginning-of-definition)
			(eq position-of-end-of-definition nil))
		   ;; It is the beginning of the definition.
		   (progn
		     (setq reached-beginning-of-definition t)
		     (setq position-of-end-of-definition (line-end-position)))
		   ;; It is the beginning of the next definition.
		   ;; The definition had to end before this line.
		   (progn
		     (if (eq position-of-end-of-definition nil)
			 (setq position-of-end-of-definition (point)))
		     (setq reached-end-of-definition t)))))
	
	(if (not reached-end-of-definition)
	    (forward-line 1)))
      
      ;; Moves to the end of the definition.
      (goto-char position-of-end-of-definition)
      
      (when (not (eq position-of-end-of-definition g-position-of-end-of-definition))
	;; Updates the global variable holding the position of the end of the
	;; definition.
	(setq g-position-of-end-of-definition position-of-end-of-definition)))
    
    ;; Code for debugging.
    (when debugging-this-function
      (message "Def. End: %s" (buffer-substring (line-beginning-position) (point)))
      (message "LEAVING ----- web-end-of-definition ----- LEAVING"))
    
    ))  

;;;-----------------------------------------------------------------------------
;;;HTML-END-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the end of the HTML definition where \emph{point}
;;;	lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the end of the buffer.
;;;	
;;;	It is an helper of \elem{end-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		If called when point lies on a region of non-HTML code the
;;;		behaviour is undefined.
;;;		
;;;		In order to prevent such situation \elem{get-definition-language}
;;;		should be used before.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun html-end-of-definition ()
  (web-end-of-definition HTML-LANGUAGE))

;;;-----------------------------------------------------------------------------
;;;JSCRIPT-END-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the end of the JavaScript definition where
;;;	\emph{point} lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the end of the buffer.
;;;	
;;;	It is an helper of \elem{end-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		If called when point lies on a region of non-JavaScript code the
;;;		behaviour is undefined.
;;;		
;;;		In order to prevent such situation \elem{get-definition-language}
;;;		should be used before.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun jscript-end-of-definition ()
  (web-end-of-definition JSCRIPT-LANGUAGE))

;;;-----------------------------------------------------------------------------
;;;PHP-END-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the end of the PHP definition where \emph{point}
;;;	lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the end of the buffer.
;;;	
;;;	It is an helper of \elem{end-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		If called when point lies on a region of non-PHP code the
;;;		behaviour is undefined.
;;;		
;;;		In order to prevent such situation \elem{get-definition-language}
;;;		or should be used before.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun php-end-of-definition ()
  (web-end-of-definition PHP-LANGUAGE))

;;;-----------------------------------------------------------------------------
;;;VB-END-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the end of the VBScript definition where
;;;	\emph{point} lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the end of the buffer.
;;;	
;;;	It is an helper of \elem{end-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		If called when point lies on a region of non-VBScript code the
;;;		behaviour is undefined.
;;;		
;;;		In order to prevent such situation \elem{get-definition-language}
;;;		should be used before.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun vb-end-of-definition ()
  (web-end-of-definition VB-LANGUAGE))

;;;-----------------------------------------------------------------------------
;;;END-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the end of the definition where \emph{point}
;;;	lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the end of the buffer.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		An integer. It is the position of the end of the definition.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;	01/10/16	Toni		Added support for JSCRIPT-MODE.
;;;-----------------------------------------------------------------------------
(defun end-of-definition ()
  (let ((debugging-this-function nil))

    ;; Code for debugging.
    (if debugging-this-function
	(message "ENTERING ----- end-of-definition ----- ENTERING"))
    
    (let ((file-mode (file-mode))
	  (language (save-excursion (get-definition-language))))
      (cond ((eq file-mode C-MODE)
	     ;; C language.
	     (c-end-of-definition))
	    
	    ((or (eq file-mode HTML-MODE)
		 (eq file-mode JSCRIPT-MODE)
		 (eq file-mode PHP-MODE)
		 (eq file-mode VB-MODE))
	     ;; HTML, JavaScript, PHP and VBScript languages.
	     (cond ((eq language HTML-LANGUAGE)
		    (html-end-of-definition))
		   ((eq language JSCRIPT-LANGUAGE)
		    (jscript-end-of-definition))
		   ((eq language PHP-LANGUAGE)
		    (php-end-of-definition))
		   ((eq language VB-LANGUAGE)
		    (vb-end-of-definition))))
	    
	    (t
	     ;; Lisp language and by default also other languages.
	     (end-of-defun))))
    
    ;; Code for debugging.
    (when debugging-this-function
      (message "Def. End: %s" (buffer-substring (line-beginning-position) (point)))
      (message "LEAVING ----- end-of-definition ----- LEAVING"))
    
    ;; Returns the position of the end of the definition.
    (point)))

;;;-----------------------------------------------------------------------------
;;;C-BEGINNING-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the beginning of the C definition where
;;;	\emph{point} lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the beginning of the
;;;	buffer.
;;;	
;;;	It is an helper of \elem{beginning-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created from piece of code inside
;;;					'beginning-of-definition'.
;;;-----------------------------------------------------------------------------
(defun c-beginning-of-definition ()
  (beginning-of-line)

  ;; The line with the beginning of the definition cannot start with any of the
  ;; following characters: space, tab, {, }.
  (while (or (= (char-after (point)) 32)    ;; Space character
	     (= (char-after (point)) 9)     ;; Tab character
	     ;; LINUX
	     ;; (= (char-after (point)) 10)
	     (= (char-after (point)) 123)   ;; { character
	     (= (char-after (point)) 125))  ;; } character
    (c-beginning-of-statement 1)
    ;; LINUX
    ;; (beginning-of-line)
    ;; (backward-char)
    (beginning-of-line)))

;;;-----------------------------------------------------------------------------
;;;WEB-BEGINNING-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the beginning of the Web definition where
;;;	\emph{point} lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the beginning of the
;;;	buffer.
;;;	
;;;	It is an helper of \elem{beginning-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{language} is an integer. Possible values include
;;;		HTML-LANGUAGE, JSCRIPT-LANGUAGE, PHP-LANGUAGE and VB-LANGUAGE.
;;;		It tells which is the programming language of the definition
;;;		where \emph{point} lies on.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		A Web definition is one that is defined using a programming
;;;		language proper for the development of Web applications.
;;;		
;;;	\implem-notes
;;;		The implementation might have been quite simplified. That was
;;;		not done in order to keep it as close as possible similar to the
;;;		implementation of \elem{web-end-of-definition}. I hope that this
;;;		decision will ease the maintenance of both definitions.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;	09/09/23	P. Madeira	`previous-line' -> `forward-line' (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun web-beginning-of-definition (language)
  (let ((debugging-this-function nil))

    ;; Code for debugging.
    (if debugging-this-function
	(message "ENTERING ----- web-beginning-of-definition ----- ENTERING"))
    
    (beginning-of-line)
    
    (let* ((mode-fillers (mode-fillers))
	   (comment      (nth 0 mode-fillers))
	   (filler       (nth 1 mode-fillers))
	   (line-prefix-max-length 12)
	   (reached-beginning-of-definition (if g-position-of-beginning-of-definition
						;; The position of the beginning of
						;; the definition is already known.
						;; It is stored in the
						;; 'g-position-of-beginning-of-definition'
						;; global variable.
						(progn
						  (when (/= (buffer-size) g-beg-previous-buffer-size)
						    (setq g-position-of-beginning-of-definition
						      (+ g-position-of-beginning-of-definition
							 (- (buffer-size) g-beg-previous-buffer-size)))
						    (setq g-beg-previous-buffer-size (buffer-size)))
						  (goto-char g-position-of-beginning-of-definition)
						  t)
						;; The position of the beginning of
						;; the definition is not known yet
						;; so a search will have to be done.
						(progn
						  (setq g-beg-previous-buffer-size (buffer-size))
						  nil))))
      
      ;; Searches up in the file, one line per iteration.
      (do* ((line-prefix (downcase (buffer-substring (point)
						     (+ (point)
							(if (< (- (line-end-position) (point))
							       line-prefix-max-length)
							    ;; There are less than 'line-prefix-max-length'
							    ;; characters from (point) to the end of the
							    ;; line so it will have to pick a shorter
							    ;; string.
							    (- (line-end-position) (point))
							    ;; Ok.
							    line-prefix-max-length))))
			 ;; Step is equal to init.
			 (downcase (buffer-substring (point)
						     (+ (point)
							(if (< (- (line-end-position) (point))
							       line-prefix-max-length)
							    (- (line-end-position) (point))
							    line-prefix-max-length)))))
	    (line-prefix-length (length line-prefix) (length line-prefix)))
	  (reached-beginning-of-definition)
	
	(cond ((= (point) (point-min))
	       ;; This is the last line of the file and is an empty line.
	       ;;
	       ;; The definition has to begin in this line or after this line.
	       (if debugging-this-function (message "Line Prefix ( 0 ): %s" line-prefix))
	       (setq reached-beginning-of-definition t))

	      ;;------------------------------------------------------------------
	      
	      ((= line-prefix-length 0)
	       ;; This is an empty line.
	       ;;
	       ;; Do nothing.
	       (if debugging-this-function (message "Line Prefix ( 1 ): %s" line-prefix)))

	      ;;------------------------------------------------------------------
	      
	      ((and (>= line-prefix-length 1)
		    (or (= (aref line-prefix 0) 32)  ;; Space character
			(= (aref line-prefix 0) 9))) ;; Tab character
	       ;; This line starts with a white space.
	       ;;
	       (if debugging-this-function (message "Line Prefix ( 2 ): %s" line-prefix)))
	      
	      ;;------------------------------------------------------------------
	      
	      ((or (and (or (eq language JSCRIPT-LANGUAGE)
			    (eq language PHP-LANGUAGE))
			(and (>= line-prefix-length 2)
			     (string-equal (substring line-prefix 0 2) "/*")))
		   (and (eq language HTML-LANGUAGE)
			(and (>= line-prefix-length 4)
			     (string-equal (substring line-prefix 0 4) "<!--"))))
	       ;; This line starts with "/*" (not HTML-LANGUAGE) or "<!--"
	       ;; (HTML-LANGUAGE), i.e, starts a multi-line comment.
	       ;;
	       (if debugging-this-function (message "Line Prefix ( 3 ): %s" line-prefix)))
	      
	      ;;------------------------------------------------------------------
	      
	      ((or (and (or (eq language JSCRIPT-LANGUAGE)
			    (eq language PHP-LANGUAGE))
			(and (>= line-prefix-length 2)
			     (string-equal (substring line-prefix 0 2) "*/")))
		   (and (eq language HTML-LANGUAGE)
			(and (>= line-prefix-length 3)
			     (string-equal (substring line-prefix 0 3) "-->"))))
	       ;; This line starts with "*/" (not HTML-LANGUAGE) or "-->"
	       ;; (HTML-LANGUAGE), i.e, ends a multi-line comment.
	       ;;
	       (if debugging-this-function (message "Line Prefix ( 4 ): %s" line-prefix)))
	      
	      ;;------------------------------------------------------------------
	      
	      ((and (eq language VB-LANGUAGE)
		    (string-equal line-prefix filler))
	       ;; This line starts with a filler (VB-LANGUAGE), i.e, starts a
	       ;; single-line comment.
	       ;;
	       (if debugging-this-function (message "Line Prefix ( 5 ): %s" line-prefix)))
	      
	      ;;------------------------------------------------------------------
	      
	      ((and (>= line-prefix-length (length comment))
		    (string-equal (substring line-prefix 0 (length comment)) comment))
	       ;; This line belongs to the definition header.
	       ;;
	       ;; Do nothing.
	       (if debugging-this-function (message "Line Prefix ( 6 ): %s" line-prefix)))
	      
	      ;;------------------------------------------------------------------
	      
	      ((or (and (or (eq language JSCRIPT-LANGUAGE)
			    (eq language PHP-LANGUAGE))
			(or (and (>= line-prefix-length 1)
				 (or (= (aref line-prefix 0) 123)   ;; { character
				     (= (aref line-prefix 0) 125))) ;; } character
			    (and (>= line-prefix-length 2)
				 (string-equal (substring line-prefix 0 2) "//"))))
		   (and (eq language PHP-LANGUAGE)
			(and (>= line-prefix-length 1)
			     (= (aref line-prefix 0) 35))) ;; # character
		   (and (eq language VB-LANGUAGE)
			(or (and (>= line-prefix-length 12)
				 (string-equal (substring line-prefix 0 12) "end function"))
			    (and (>= line-prefix-length 7)
				 (string-equal (substring line-prefix 0 7) "end sub"))
			    (and (>= line-prefix-length 1)
				 (= (aref line-prefix 0) 39)) ;; ' character
			    (and (>= line-prefix-length 3)
				 (string-equal (substring line-prefix 0 3) "rem")))))
	       ;; This line starts either with the {, }, "end function" or "end sub"
	       ;; delimiter or with the "//", #, ', "rem" single-line comment.
	       ;;
	       (if debugging-this-function (message "Line Prefix ( 7 ): %s" line-prefix)))
	      
	      ;;------------------------------------------------------------------
	      
	      ((and (not (eq language HTML-LANGUAGE))
		    (or (and (>= line-prefix-length 7)
			     (string-equal (substring line-prefix 0 7) "<script"))
			(and (>= line-prefix-length 8)
			     (string-equal (substring line-prefix 0 8) "</script"))
			(and (>= line-prefix-length 5)
			     (string-equal (substring line-prefix 0 5) "<?php"))
			(and (>= line-prefix-length 2)
			     (string-equal (substring line-prefix 0 2) "?>"))
			(and (>= line-prefix-length 2)
			     (string-equal (substring line-prefix 0 2) "<%"))
			(and (>= line-prefix-length 2)
			     (string-equal (substring line-prefix 0 2) "%>"))))
	       ;; This line starts or ends a code region.
	       ;;
	       ;; The definition had to begin after this line.
	       (if debugging-this-function (message "Line Prefix ( 8 ): %s" line-prefix))
	       (setq reached-beginning-of-definition t))
	      
	      ;;------------------------------------------------------------------
	      
	      ((and (eq language HTML-LANGUAGE)
		    (or (and (>= line-prefix-length 5)
			     (string-equal (substring line-prefix 0 5) "<head"))
			(and (>= line-prefix-length 5)
			     (string-equal (substring line-prefix 0 5) "<body"))
			(and (>= line-prefix-length 9)
			     (string-equal (substring line-prefix 0 9) "<frameset"))))
	       ;; This line is the beginning of the definition.
	       ;;
	       (if debugging-this-function (message "Line Prefix ( 9 ): %s" line-prefix))
	       (setq reached-beginning-of-definition t))
	      
	      ;;------------------------------------------------------------------
	      
	      ((eq language HTML-LANGUAGE)
	       ;; This line belongs to the definition but does not begin it.
	       ;;
	       (if debugging-this-function (message "Line Prefix (10 ): %s" line-prefix)))
	      
	      ;;------------------------------------------------------------------
	      
	      (t ;; (not (eq language HTML-LANGUAGE))
	       ;; This line is the beginning of the definition.
	       ;;
	       (if debugging-this-function (message "Line Prefix (11 ): %s" line-prefix))
	       (setq reached-beginning-of-definition t)))
	
	(if (not reached-beginning-of-definition)
	    (forward-line -1))))
    
    (when (not (eq (point) g-position-of-beginning-of-definition))
      ;; Updates the global variable holding the position of the beginning of the
      ;; definition.
      (setq g-position-of-beginning-of-definition (point)))
    
    ;; Code for debugging.
    (when debugging-this-function
	(message "Def. Beginning: %s" (buffer-substring (point) (line-end-position)))
	(message "LEAVING ----- web-beginning-of-definition ----- LEAVING"))
    
    ))

;;;-----------------------------------------------------------------------------
;;;HTML-BEGINNING-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the beginning of the HTML definition where
;;;	\emph{point} lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the beginning of the
;;;	buffer.
;;;	
;;;	It is an helper of \elem{beginning-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		If called when point lies on a region of non-HTML code the
;;;		behaviour is undefined.
;;;		
;;;		In order to prevent such situation \elem{get-definition-language}
;;;		should be used before.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun html-beginning-of-definition ()
  (web-beginning-of-definition HTML-LANGUAGE))

;;;-----------------------------------------------------------------------------
;;;JSCRIPT-BEGINNING-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the beginning of the JavaScript definition where
;;;	\emph{point} lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the beginning of the
;;;	buffer.
;;;	
;;;	It is an helper of \elem{beginning-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		If called when point lies on a region of non-JavaScript code the
;;;		behaviour is undefined.
;;;		
;;;		In order to prevent such situation \elem{get-definition-language}
;;;		should be used before.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun jscript-beginning-of-definition ()
  (web-beginning-of-definition JSCRIPT-LANGUAGE))

;;;-----------------------------------------------------------------------------
;;;PHP-BEGINNING-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the beginning of the PHP definition where
;;;	\emph{point} lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the beginning of the
;;;	buffer.
;;;	
;;;	It is an helper of \elem{beginning-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		If called when point lies on a region of non-PHP code the
;;;		behaviour is undefined.
;;;		
;;;		In order to prevent such situation \elem{get-definition-language}
;;;		or should be used before.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun php-beginning-of-definition ()
  (web-beginning-of-definition PHP-LANGUAGE))

;;;-----------------------------------------------------------------------------
;;;VB-BEGINNING-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the beginning of the VBScript definition where
;;;	\emph{point} lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the beginning of the
;;;	buffer.
;;;	
;;;	It is an helper of \elem{beginning-of-definition}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		If called when point lies on a region of non-VBScript code the
;;;		behaviour is undefined.
;;;		
;;;		In order to prevent such situation \elem{get-definition-language}
;;;		should be used before.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun vb-beginning-of-definition ()
  (web-beginning-of-definition VB-LANGUAGE))

;;;-----------------------------------------------------------------------------
;;;BEGINNING-OF-DEFINITION
;;;Description
;;;	Places \emph{point} at the beginning of the definition where \emph{point}
;;;	lies on.
;;;	
;;;	If no definition is found \emph{point} is left at the beginning of the
;;;	buffer.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		An integer. It is the position of the beginning of the
;;;		definition.
;;;		
;;;History
;;;	Date		Author		Description
;;;	97/01/01	Joao Filipe	Modificado para C-MODE
;;;	01/03/02	Toni		Support for HTML-MODE, PHP-MODE and
;;;					VB-MODE.
;;;					Doc updated.
;;;	01/10/16	Toni		Added support for JSCRIPT-MODE.
;;;-----------------------------------------------------------------------------
(defun beginning-of-definition (&optional at-end-of-definition)
  (let ((debugging-this-function nil))

    ;; Code for debugging.
    (if debugging-this-function
	(message "ENTERING ----- beginning-of-definition ----- ENTERING"))
    
    (let ((file-mode (file-mode))
	  (language (save-excursion (get-definition-language))))
      
      (cond ((eq file-mode C-MODE)
	     ;; C language.
	     (c-beginning-of-definition))
	    
	    ((or (eq file-mode HTML-MODE)
		 (eq file-mode JSCRIPT-MODE)
		 (eq file-mode PHP-MODE)
		 (eq file-mode VB-MODE))
	     ;; HTML, JavaScript, PHP and VBScript languages.
	     (when (not at-end-of-definition)
	       (end-of-definition))
	     (cond ((eq language HTML-LANGUAGE)
		    (html-beginning-of-definition))
		   ((eq language JSCRIPT-LANGUAGE)
		    (jscript-beginning-of-definition))
		   ((eq language PHP-LANGUAGE)
		    (php-beginning-of-definition))
		   ((eq language VB-LANGUAGE)
		    (vb-beginning-of-definition))))
	    
	    (t
	     ;; Lisp language and by default also other languages.
	     (when (not at-end-of-definition)
	       (end-of-defun))
	     (beginning-of-defun))))
    
    ;; Code for debugging.
    (when debugging-this-function
      (message "Def. Beginning: %s" (buffer-substring (point) (line-end-position)))
      (message "LEAVING ----- beginning-of-definition ----- LEAVING"))
    
    ;; Returns the position of the beginning of the definition.
    (point)))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; PROCESSING OF SIGNATURES
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;GET-BUFFER-SIGNATURE
;;;Description
;;;	Builds a list with every signature of the current file.
;;;	
;;;	Signatures are extracted from the file header by searching from the
;;;	beginning of the file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of strings. Each string corresponds to one signature.
;;;		The first signature in the list is the last one in the file
;;;		header.
;;;		
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	
;;;	97/08/20	A. Frazao	Handles different file types
;;;	01/03/02	Toni		Added support for modes HTML-MODE,
;;;					PHP-MODE and VB-MODE by generalising.
;;;					Doc updated
;;;	08/07/29	A. Frazao	Calls beginning-of-file-header
;;;-----------------------------------------------------------------------------
(defun get-buffer-signature ()
  (let* ((mode-fillers            (mode-fillers))
	 (comment                 (nth 4 mode-fillers))
	 (multi-line-comment-open (nth 6 mode-fillers))
	 (signature nil)
	 (str nil))
    (beginning-of-file-header)
    (when multi-line-comment-open
      (forward-line))
    (let ((start (point))
	  (end nil))
      (forward-line)
      (setf end (point))
      (setq str (buffer-substring start end))
      (while (string-match (format "^%s" comment) str)
	(when (string-match (format "^%s[ \t]+[0-9][0-9]/[0-9][0-9]/[0-9][0-9][ \t]+[^ \t]+" comment) str)
	  (push str signature))
	(setq start end)
	(forward-line)
	(setf end (point))
	(setq str (buffer-substring start end))))
    signature))

;;;-----------------------------------------------------------------------------
;;;GET-FILE-SIGNATURE
;;;Description
;;;	Devolve uma lista com TODAS as assinaturas de FILE.
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	
;;;-----------------------------------------------------------------------------
(defun get-file-signature (file)
  (let* ((old-buf (get-file-buffer file))
	 (buf (or old-buf
		  (find-file-noselect file))))
    (set-buffer buf)
    (let ((signature (get-buffer-signature)))
      (unless old-buf
	(kill-buffer buf))
      signature)))

;;;-----------------------------------------------------------------------------
;;;RAC
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/07/12	Joao Filipe	Created
;;;-----------------------------------------------------------------------------
(defun rac (l)
  (car (last l)))

;;;-----------------------------------------------------------------------------
;;;CHECK-SIGNATURES
;;;Description
;;;	Verifica se as assinaturas novas (SIGS-NEW) estao de acordo com as
;;;     assinaturas originais (SIGS-ORG). 
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{sigs-new} is a <>.
;;;		
;;;		\arg{sigs-org} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Created
;;;	96/07/12	Joao Filipe	Deve testar a ultima de forma diferente.
;;;					Quando a lista de assinaturas e muito grande,
;;;					pode-se apagar algumas intermedias, mantendo
;;;					no entanto a primeira, para guardar a data
;;;					de criacao.
;;;	97/07/25	A. Frazao	Retorna explicacao de erros
;;;	97/09/04	A. Frazao	Replaced STRING-MATCH by STRING-EQUAL
;;;	99/01/29	A. Frazao	Corrected message when sigs-new is NIL
;;;	02/05/28	A. Frazao	Corrected test to exit do loop.
;;;-----------------------------------------------------------------------------
(defun check-signatures (sigs-new sigs-org)
  (if sigs-new
      (progn
	(setq sigs-new (cdr sigs-new))
	(cond ((null sigs-new)
	       (if (null sigs-org)
		   ERR_CORRECT
		   (list "Modified file has only one signature and the original should not have any")))
	      ((null sigs-org)
	       (list "Original does not have any signature"))
	      ((> (length sigs-new) (length sigs-org))
	       (list (format "Number of signatures does not match (Modified: %d Original: %d)" (length sigs-new) (length sigs-org))))
	      ((not (string-equal (rac sigs-new) (rac sigs-org)))
	       (list "Last signatures do not match"
		       (format "Modified: %s" (rac sigs-new))
		       (format "Original: %s" (rac sigs-org))))
	      (t (do ((s-new sigs-new (cdr s-new))
		      (s-org sigs-org (cdr s-org)))
		     ((null s-new) ERR_CORRECT)
		   (unless (string-equal (car s-new) (car s-org))
		     (return (list "Intermediate signatures do not match"
				   (format "Modified: %s" (car s-new))
				   (format "Original: %s" (car s-org)))))))))
      (list "Source file does not have signatures")))


;;;-----------------------------------------------------------------------------
;;;CHECK-DOC-SRC-FILE
;;;Description
;;;	Checks if the given \arg{filename} has elements that are not correctly 
;;;	documented.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{filename} is a \emph{pathname}
;;;		
;;;	\return-types
;;;		A \emph{fixnum} or a list of strings
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun check-doc-src-file (filename)
  (let ((old-control-by-file %control-by-file%)
	(old-control-by-element %control-by-element%)
	(old-sample-base-date %sample-base-date%)
	(old-stat-pretty-print %stat-pretty-print%)
	(stat (make-doc-stat)))
    (setq %control-by-file% nil)
    (setq %control-by-element% t)
    (setq %sample-base-date% (get.mod.date))
    (setq %stat-pretty-print% t)
    (measure-file filename stat)
    (setq %control-by-file% old-control-by-file)
    (setq %control-by-element% old-control-by-element)
    (setq %sample-base-date% old-sample-base-date)
    (setq %stat-pretty-print% old-stat-pretty-print)
    (cond ((not (= (doc-stat-total-elems stat) (doc-stat-elems-with-desc stat)))
	   (list "There are changed elements without documentation"))
	  ((not (= (doc-stat-total-elems stat) (doc-stat-elems-fully-desc stat)))
	   (list "There are changed elements not fully documented"))
	  (t
	   ERR_CORRECT))))


;;;-----------------------------------------------------------------------------
;;;CHECK-SRC-FILE
;;;Description
;;;	Verifica se o ficheiro source FILE tem as assinaturas correctas.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;		\arg{file} is a modification file
;;;		
;;;		\arg{check-doc} is a \emph{boolean}
;;;		
;;;	\return-types
;;;		A fixnum or a list of strings
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Passou a considerar TODAS e nao so a ultima
;;;					assinatura...
;;;	05/10/13	J. P. Varandas	Also check the correctness of the file documentation
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun check-src-file (mod file &optional check-doc)
  (let* ((src-dir (modif-request-src-dir mod))
	 (org-dir (modif-request-org-dir mod))
	 (src-file (format "%s/%s" src-dir (file-src file)))
	 (org-file (format "%s/%s" org-dir (file-src file))))
    (cond ((delete-file-p file)
	   (if (file-exists-p org-file)
	       (set-mod-file-status file ERR_CORRECT)
	       (set-mod-file-status file ERR_DEL_FILE)))
	  ((new-file-p file)
	   (if (file-exists-p org-file)
	       (set-mod-file-status file ERR_NEW_FILE)
	       (set-mod-file-status file ERR_CORRECT)))
	  ((not (file-exists-p src-file))
	   (set-mod-file-status file ERR_NO_SRC_FILE))
	  ((not (file-exists-p org-file))
	   (set-mod-file-status file ERR_NO_ORG_FILE))
	  (t
	   (let ((status (check-signatures (get-file-signature src-file)
					   (get-file-signature org-file))))
	     (if (and (not (listp status))
		      (= status ERR_CORRECT)
		      check-doc)
		 (set-mod-file-status file (check-doc-src-file src-file))
		 (set-mod-file-status file status))))))
  (file-status file))




;;;-----------------------------------------------------------------------------
;;;CHECK-MOD-FILE
;;;Description
;;;	Verifica se o ficheiro modif FILE tem as assinaturas correctas.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;		\arg{file} is a 5-tuple corresponding to a modification file
;;;		
;;;		\arg{check-doc} is a \emph{boolean}
;;;		
;;;	\return-types
;;;		A fixnum or a list of strings
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Passou a considerar TODAS e nao so a ultima
;;;					assinatura...
;;;	05/10/13	J. P. Varandas	Also check the correctness of the file documentation
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun check-mod-file (mod file &optional check-doc)
  (let* ((mod-dir (modif-request-dir mod))
	 (org-dir (modif-request-org-dir mod))
	 (mod-file (format "%s/%s" mod-dir (file-mod file)))
	 (org-file (format "%s/%s" org-dir (file-src file))))
    (cond ((delete-file-p file)
	   (if (file-exists-p org-file)
	       (set-mod-file-status file ERR_CORRECT)
	       (set-mod-file-status file ERR_DEL_FILE)))
	  ((new-file-p file)
	   (if (file-exists-p org-file)
	       (set-mod-file-status file ERR_NEW_FILE)
	       (set-mod-file-status file ERR_CORRECT)))
	  ((not (file-exists-p mod-file))
	   (set-mod-file-status file ERR_NO_MOD_FILE))
	  ((not (file-exists-p org-file))
	   (set-mod-file-status file ERR_NO_ORG_FILE))
	  (t
	   (let ((status (check-signatures (get-file-signature mod-file)
					   (get-file-signature org-file))))
	     (if (and (not (listp status))
		      (= status ERR_CORRECT)
		      check-doc)
		 (set-mod-file-status file (check-doc-src-file mod-file))
		 (set-mod-file-status file status))))))
  (file-status file))
  
;;;-----------------------------------------------------------------------------
;;;CHECK-SOURCE-FILES
;;;Description
;;;	Check the correctness of the signatures and documentation of the source 
;;;	files in the current modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{no-display} is a \emph{boolean}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	
;;;	99/05/12	Dario		If active turn off font lock while
;;;					checking files. (For common lisp mode
;;;					only).
;;;	99/06/23	Dario		Ignore font lock in SOLARIS.
;;;	99/07/05	Dario		Call REMOVE-HOOK.
;;;	05/10/13	J. P. Varandas	'check-src-file' will also check by documentation
;;;	06/08/24	J. P. Varandas	Removed obsolete code for Solaris
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun check-source-files (&optional no-display)
  (let ((error nil))
    (flet ((check-files ()
	     (save-excursion
	       (let ((mod (current-modif-request)))
		 (dolist (file (modif-request-files mod))
		   (let ((res (check-src-file mod file t)))
		     (unless (and (numberp res)
				  (= res ERR_CORRECT))
		       (setf error t))))))
	     (when (or error (null no-display))
	       (display-current-modif-request))))
      (let ((font-lock-on-p (member 'turn-on-font-lock fi:common-lisp-mode-hook)))
	(unwind-protect
	     (progn (when font-lock-on-p
		      (remove-hook 'fi:common-lisp-mode-hook 'turn-on-font-lock))
		    (check-files))
	  (progn (when font-lock-on-p
		   (add-hook 'fi:common-lisp-mode-hook 'turn-on-font-lock))
		 error))))))

;;;-----------------------------------------------------------------------------
;;;CHECK-MODIFICATION-FILES
;;;Description
;;;	Check the correctness of the signatures and documentation of the modification 
;;;	files in the current modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{no-display} is a \emph{boolean}
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	
;;;	99/05/12	Dario		If active turn off font lock while
;;;					checking files. (For common lisp mode
;;;					only).
;;;	99/06/23	Dario		Ignore font lock in SOLARIS.
;;;	99/07/05	Dario		Call REMOVE-HOOK.
;;;	05/10/13	J. P. Varandas	'check-src-file' will also check by documentation
;;;	06/08/24	J. P. Varandas	Removed obsolete code for Solaris
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun check-modification-files (&optional no-display)
  (let ((error nil))
    (flet ((check-files ()
	     (save-excursion
	       (let ((mod (current-modif-request)))
		 (dolist (file (modif-request-files mod))
		   (let ((res (check-mod-file mod file t)))
		     (unless (and (numberp res)
				  (= res ERR_CORRECT))
		       (setf error t))))))
	     (when (or error (null no-display))
	       (display-current-modif-request))))
      (let ((font-lock-on-p (member 'turn-on-font-lock fi:common-lisp-mode-hook)))
	(unwind-protect
	     (progn (when font-lock-on-p
		      (remove-hook 'fi:common-lisp-mode-hook 'turn-on-font-lock))
		    (check-files))
	  (progn (when font-lock-on-p
		   (add-hook 'fi:common-lisp-mode-hook 'turn-on-font-lock))
		 error))))))


;;;-----------------------------------------------------------------------------
;;;CHECK-ALL-MODIFICATIONS-SOURCE-FILES
;;;Description
;;;	Checks all the modifications source files
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{no-display} is a \emph{boolean}. If it is \emph{nil} the
;;;		modification is displayed after checking. Otherwise not.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun check-all-modifications-source-files (&optional no-display)
  (unless (all-modif-requests)
    (load-modifications))
  (let ((current-mod (current-modif-request)))
    (unwind-protect
	(dolist (mod (all-modif-requests))
	  (set-current-modif-request mod)
	  (check-source-files t))
      (set-current-modif-request current-mod))
    (unless no-display
      (display-all-modif-requests))))

;;;-----------------------------------------------------------------------------
;;;TOUCH-CURRENT-MOD-SOURCE-FILES
;;;Description
;;;	Changes all the source files of the current modification so that they
;;;	become with a different changed date in order to force compilation.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun touch-current-mod-source-files ()
  (save-excursion
    (let ((old-font-lock-maximum-size font-lock-maximum-size)
	  (mod (current-modif-request)))
      (setf font-lock-maximum-size 0)
      (unwind-protect
	  (dolist (file (modif-request-files mod))
	    (let* ((src-dir (modif-request-src-dir mod))
		   (src-file (format "%s/%s" src-dir (file-src file))))
	      (if (file-exists-p src-file)
		  (let* ((old-buf (get-file-buffer src-file))
			 (buf (or old-buf
				  (find-file-noselect src-file))))
		    (set-buffer buf)
		    (insert 10)
		    (let ((end (point)))
		      (forward-char -1)
		      (delete-region (point) end))
		    (save-buffer)
		    (unless old-buf
		      (kill-buffer buf))))))
	(setf font-lock-maximum-size old-font-lock-maximum-size)))))


;;;-----------------------------------------------------------------------------
;;;TOUCH-ALL-MODS-SOURCE-FILES
;;;Description
;;;	Changes all the source files of all modifications so that they
;;;	become with a different changed date in order to force compilation.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun touch-all-mods-source-files ()
  (unless (all-modif-requests)
    (load-modifications))
  (let ((current-mod (current-modif-request)))
    (unwind-protect
	(dolist (mod (all-modif-requests))
	  (set-current-modif-request mod)
	  (touch-current-mod-source-files))
      (set-current-modif-request current-mod))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; EDITION OF FILES
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;GET-REFERENCE-FILE
;;;Description
;;;	Returns the original or source file corresponding to the current buffer
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{where} is a \emph{string}
;;;		
;;;	\return-types
;;;		- A \elem{pathname}
;;;		- A \elem{pathname}
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun get-reference-file (where)
  (let ((filename (buffer-file-name (current-buffer)))
	(mod (current-modif-request)))
    (cond ((string-match (format "^%s/" (modif-request-src-dir mod)) filename)
	   (values (format "%s/%s" where (substring filename (match-end 0))) filename))
	  ((string-match (format "^%s/" (modif-request-org-dir mod)) filename)
	   (values (format "%s/%s" where (substring filename (match-end 0))) filename)))))

;;;-----------------------------------------------------------------------------
;;;EDIT-REFERENCE-FILE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun edit-reference-file (where)
  (multiple-value-bind (source-file filename)
      (get-reference-file where)
    (if source-file
	(if (file-exists-p source-file)
	    (switch-to-buffer-other-window (find-file-noselect source-file))
	    (beep-message (format "Could not open file %s" source-file)))
	(beep-message (format "The file %s does not match the main directories" filename)))))

;;;-----------------------------------------------------------------------------
;;;EDIT-ORIGINAL-FILE
;;;Description
;;;	Ask to edit the original file corresponding to the current buffer
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-original-file ()
  (edit-reference-file (modif-request-org-dir (current-modif-request))))
	
;;;-----------------------------------------------------------------------------
;;;EDIT-SOURCE-FILE
;;;Description
;;;	Ask to edit the source file corresponding to the current buffer
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-source-file ()
  (edit-reference-file (modif-request-src-dir (current-modif-request))))

;;;-----------------------------------------------------------------------------
;;;EDIT-BACKUP-FILE
;;;Description
;;;	Ask to edit the backup file corresponding to the current buffer
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-backup-file ()
  (multiple-value-bind (source-file filename)
      (get-reference-file (modif-request-src-dir (current-modif-request)))
    (setf source-file (format "%s~" source-file))
    (if source-file
	(if (file-exists-p source-file)
	    (switch-to-buffer-other-window (find-file-noselect source-file))
	    (beep-message (format "Could not open file %s" source-file)))
	(beep-message (format "The file %s does not match the main directories" filename)))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; SAVING MODIFICATIONS
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SAVE-MODIFICATIONS
;;;Description
;;;	Saves all modification structures into a file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	
;;;	98/11/04	A. Frazao	Added PATCH. Use MY-END-OF-BUFFER
;;;	05/03/21	J. P. Varandas	Added REFERENCE
;;;	05/10/13	J. P. Varandas	Added VERSIONS and MAIL-FILE
;;;	06/05/29	A. Frazao	Added MAIL-SENT
;;;	08/10/09	Tom Weissmann	Restructured. Corrected problem when indenting
;;;					in Emacs 22. (POA 12960.0)
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun save-modifications ()
  (macrolet ((insert-surrounding (first last &rest body)
               `(progn
                  (insert ,first)
                  ,@body
                  (insert ,last))))
    (with-temp-buffer
      (dolist (mod (all-modif-requests))
        (insert-surrounding "(" ")\n"
           (dolist (prop '((:version   . 2)
                           (:name      . modif-request-name)
                           (:author    . modif-request-author)
                           (:org-dir   . modif-request-org-dir)
                           (:src-dir   . modif-request-src-dir)
                           (:dir       . modif-request-dir)
                           (:reference . modif-request-reference)
                           (:versions  . modif-request-versions)
                           (:date      . modif-request-date)
                           (:mail-file . modif-request-mail-file)
                           (:mail-sent . modif-request-mail-sent)
                           (:patch     . modif-request-patch)))
             (let ((value (if (functionp (cdr prop))
                              (funcall (cdr prop) mod)
                              (cdr prop))))
               (when value (insert (format "%s %S\n" (car prop) value)))))
           ;; now the file list
           (insert-surrounding ":files\n(\n" ")\n"
	     (dolist (file (modif-request-files mod))
               (insert (format "%S\n" (list (file-src file)
                                            (file-mod file)
                                            (file-del file)
                                            (file-new file)
                                            (let ((status (file-status file)))
                                              (if (listp status)
                                                  0
                                                  status)))))))))
      ;; indentation
      (let ((lisp-mode-hook       nil)
            (emacs-lisp-mode-hook nil)
            (font-lock-mode       nil))
        (emacs-lisp-mode))
      (lisp-indent-region (point-min) (point-max))
      ;; write it
      (write-file *modif-request-file*)
      t)))

;;;-----------------------------------------------------------------------------
;;;INFORM-BIG-BROTHER
;;;Description
;;;	Informa o big-brother de tudo o que o utilizador fez (BUFO!!!).
;;;     "Every step you take, every move you make, I'll be watching you!"
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{type} is a <>.
;;;		
;;;		\arg{modif} is a <>.
;;;		
;;;		\arg{elem} is a <>.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Created
;;;	98/11/04	A. Frazao	Use MY-END-OF-BUFFER
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;					*crews-big-brother-file* -> *actions-log-file*
;;;-----------------------------------------------------------------------------
(defun inform-big-brother (type modif &optional elem)
  (when *actions-log-file*
    (let* ((old-buf (current-buffer))
	   (name (file-name-nondirectory *actions-log-file*))
	   (buf (get-buffer name)))
      (when buf
	(kill-buffer buf))
      (setq buf (get-buffer-create name))
      (set-buffer buf)
      (erase-buffer)
      
      (insert (my-current-time-string) 9 type 9 (modif-request-name modif))
      (when elem
	(let ((filename (buffer-file-name old-buf)))
	  (when (string-match *default-src-dir* filename)
	    (setq filename (substring filename (+ 1 (match-end 0)))))
	  (insert 9 40 elem 41 9 filename)))
      (insert 10)
      
      (beginning-of-buffer)
      (let ((start (point)))
	(my-end-of-buffer)
	(append-to-file start (point) *actions-log-file*))
      (set-buffer old-buf)
      (kill-buffer buf)
      t)))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; LOADING MODIFICATIONS
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;LOAD-MODIFICATIONS
;;;Description
;;;	Loads and instantiate all modification requests stored in the *modif-request-file*
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;	09/12/28	A. Frazao	Set current modif as nil
;;;	10/01/26	P. Madeira	Reset current modification after loading
;;;-----------------------------------------------------------------------------
(defun load-modifications ()
  (when (and (file-exists-p *modif-request-file*)
	     (file-readable-p *modif-request-file*))
    (let ((old-buf (current-buffer))
	  (buf (find-file-noselect *modif-request-file*))
	  (save nil)
	  (start nil)
	  (end nil))
      (set-all-modif-requests nil)
      (set-buffer buf)
      (beginning-of-buffer)
      (let ((form t))
	(while form
	  (setq start (point))
	  (forward-sexp)
	  (setq end (point))
	  (setq form (ignore-errors (read-from-string (buffer-substring start end))))
	  (when form
	    (let ((mod (apply 'make-modif-request (cddar form))))
	      (setf save (check-mod-version mod (cadar form)))
	      (add-modif-request mod)))))
      (set-buffer old-buf)
      (kill-buffer buf)
      (when save
	(save-modifications))
      (when (current-modif-request)
	(set-current-modif-request (find (modif-request-name (current-modif-request))
					 (all-modif-requests)
					 :key 'modif-request-name
					 :test 'string=)))
      t)))

;;;-----------------------------------------------------------------------------
;;;CHECK-MOD-VERSION
;;;Description
;;;	Check if the modif request was stored in the correct version. 
;;;	If not correct it.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;		\arg{version} is a \emph{integer}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun check-mod-version (mod version)
  (when (= version 1)
    (let ((files (modif-request-files mod))
	  (new-files nil))
      (dolist (file files)
	(cond ((eq (cadr file) 1)
	       (push (make-mod-file (car file) "" 1 0 0) new-files))
	      ((eq (caddr file) 1)
	       (push (make-mod-file (car file) (cadr file) 0 1 0) new-files))
	      (t (push (make-mod-file (car file) (cadr file) 0 0 0) new-files))))
      (setf (modif-request-files mod) new-files))
    t))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; OPERATIONS OVER MODIFICATIONS
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;GET-SYSTEM-NAME
;;;Description
;;;	Create the file modif name 
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{filename} is a \emph{pathname}
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	01/09/27	A. Vasconcelos	Updated for CREWS_ML and CREWS_DSB.
;;;	02/01/31	Pedro Matos	Updated for CREWS_VR.
;;;	02/02/28	A. Frazao	Added emacs
;;;	02/07/18	Carlos Ribeiro	Updated for CREWS_BRISA.
;;;	05/11/11	Carlos Ribeiro	Updated for CREWS_LUL.
;;;	08/07/08	A. Frazao	Updated to new environment varaibles
;;;	09/02/10	J. P. Varandas  Generalise for any product by using the information of 'sc-system-src-version-dirs'
;;;-----------------------------------------------------------------------------
(defun get-system-name (filename)
  (let ((system (file-name-directory filename))
	(name (file-name-nondirectory filename)))
    (block exit
      (let ((dir (and system (file-name-nondirectory (substring system 0 (1- (length system)))))))
	(dolist (system *sc-all-systems*)
	  (dolist (src-version-dir (sc-system-src-version-dirs system))
	    (when (string-match (format "^%s" (eval src-version-dir)) filename)
	      (setf name (format "%s-%s-%s" (or (sc-system-company-name system)
						(sc-product-external-id (sc-system-product system)))
				 dir name))
	      (return-from exit))))))
    name))


;;;-----------------------------------------------------------------------------
;;;MAKE-MOD-FILE-NAME
;;;Description
;;;	Creates the unique name of the given file that will the reference in 
;;;	all modifications
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{filename} is a \emph{pathname}
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Usa a função 'get-system-name' para préfixar 
;;;					o nome com o nome da companhia
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun make-mod-file-name (filename)
  (let* ((name (get-system-name filename))
	 (mod-name name)
	 (n 0))
    (while name
      (if (dolist (mod (all-modif-requests))
	    (when (or (dolist (file (modif-request-files mod))
			(when (and (not (delete-file-p file))
				   (string= mod-name (file-mod file)))
			  (return t)))
		      (and (modif-request-patch mod)
			   (string= mod-name (modif-request-patch mod))))
	      (return t)))
	  (setq mod-name (format "%s%s" (incf n) name))
	  (setq name nil)))
    mod-name))


;;;-----------------------------------------------------------------------------
;;;HAS-MODIFIED
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/09/23	P. Madeira	`next-line' -> `forward-line' (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun has-modified ()
  (let* ((mode-fillers (mode-fillers))
	 (comment (car mode-fillers))
	 (filler (cadr mode-fillers)))
    (save-excursion
      (search-backward (format "%s	Date" comment) nil t)
      (let ((found nil)
	    (str (format "%s	%s" comment (current-mod-date-string))))
	(while (not found)
	  (cond ((looking-at str)
		 (setq found 0))
		((looking-at filler)
		 (setq found 1))
		(t
		 (forward-line 1))))
	(= found 0)))))

;;;-----------------------------------------------------------------------------
;;;HAS-MODIFIED2
;;;Description
;;;	Checks whether the definition where \emph{point} lies on has or not
;;;	already a signature for the user to describe the change that is
;;;	currently making on it.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A generalised boolean. When non-\emph{nil} it means that the
;;;		definition has already a signature for the user to describe the
;;;		change. When \emph{nil} it means the opposite.
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	01/03/02	Toni		Added support for modes HTML-MODE,
;;;					PHP-MODE and VB-MODE by generalising.
;;;					Doc updated
;;;	09/09/23	P. Madeira	`previous-line' -> `forward-line' (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun has-modified2 ()
  (save-excursion
    (let* ((mode-fillers (mode-fillers))
	   (comment      (nth 0 mode-fillers))
	   (filler       (nth 1 mode-fillers))
	   (multi-line-comment-close (nth 3 mode-fillers)))
      (beginning-of-definition)
      (forward-line -1)
      (when multi-line-comment-close
	(forward-line -1))
      (if (looking-at filler)
	  (progn
	    (when (search-backward (format "%s	Date" comment) nil t)
	      (let ((found nil)
		    (str (format "%s	%s" comment (current-mod-date-string))))
		(while (not found)
		  (cond ((looking-at str)
			 (setq found 0))
			((looking-at filler)
			 (setq found 1))
			(t
			 (forward-line 1))))
		(= found 0))))))))


;;;-----------------------------------------------------------------------------
;;;GET-NEXT-NAME
;;;Description
;;;	The next sexp at point as an upper-case string.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		\emph{string}
;;;		
;;;	\implem-notes
;;;		This function is not interested in the formatting properties of the sexp,
;;;		only its string.
;;;	
;;;History
;;;	Date		Author		Description
;;;	08/02/22	Tom Weissmann	Doc updated, use `buffer-substring-no-properties'
;;;					instead of `buffer-substring' (POA 12701.0)
;;;-----------------------------------------------------------------------------
(defun get-next-name ()
  (next-non-space)
  (let ((start (point)))
    (forward-sexp)
    (upcase (buffer-substring-no-properties start (point)))))


;;;-----------------------------------------------------------------------------
;;;GET-CURRENT-NAME
;;;Description
;;;	A version of `get-next-name' that does not move point.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		An upper-case \emph{string}.
;;;
;;;History
;;;	Date		Author		Description
;;;	08/02/22	Tom Weissmann	Created (POA 12701.0).
;;;-----------------------------------------------------------------------------
(defun get-current-name ()
  (save-excursion
    (get-next-name)))


;;;-----------------------------------------------------------------------------
;;;GET-NEXT-METHOD-CLASS
;;;Description
;;;	Gets the next method specializer
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	09/02/02	J. P. Varandas	Replace cycle 'while' by the command 'forward-sexp'
;;;-----------------------------------------------------------------------------
(defun get-next-method-class ()
  (next-non-space)
  (cond ((or (= (char-after (point)) 38)
	     (= (char-after (point)) 41))
	 nil)
	((= (char-after (point)) 40)
	 (let (start end)
	   (forward-char 1)
	   (forward-sexp)
	   (next-non-space)
	   (setq start (point))
	   (forward-sexp)
;;;	   (while (not (= (char-after (point)) 41))
;;;	     (forward-char 1))
	   (setq end (point))
	   (forward-char 1)
	   (upcase (buffer-substring start end))))
	(t (forward-sexp 1)
	   "T")))

(defun get-method-classes ()
  (let ((str (get-next-method-class))
	class)
    (while (setq class (get-next-method-class))
      (setq str (format "%s %s" str class)))
    str))

(defun get-defstruct-name ()
  (next-non-space)
  (when (= (char-after (point)) 40)
    (forward-char 1)
    (next-non-space))
  (let ((start (point)))
    (forward-sexp)
    (upcase (buffer-substring start (point)))))

;;;-----------------------------------------------------------------------------
;;;GET-FOREIGN-NAME
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun get-foreign-name ()
  (next-non-space)
  (forward-char 1)
  (let ((start (point)))
    (forward-sexp)
    (upcase (buffer-substring start (point)))))

(defun get-method-id ()
  (let ((str (get-next-name)))
    (next-non-space)
    (if (= (char-after (point)) 40)
	(progn
	  (forward-char 1)
	  (format "%s (%s)" str (get-method-classes)))
	(progn
	  (setq str (format "%s %s" str (get-next-name)))
	  (next-non-space)
	  (forward-char 1)
	  (format "%s (%s)" str (get-method-classes))))))

;;;-----------------------------------------------------------------------------
;;;GET-SYMBOL-ID
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun get-symbol-id ()
  (beginning-of-line)
  (next-non-space)
  (let ((start (point)))
    (forward-sexp)
    (upcase (buffer-substring start (point)))))

;;;-----------------------------------------------------------------------------
;;;REMOVE-SYMBOL-ID
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/09/23	P. Madeira	`kill-line' -> `sc-kill-line'
;;;-----------------------------------------------------------------------------
(defun remove-symbol-id ()
  (beginning-of-line)
  (next-non-space)
  (let ((start (point))
	(id nil))
    (forward-sexp)
    (setq id (upcase (buffer-substring start (point))))
    (beginning-of-line)
    (sc-kill-line)
    (sc-kill-line)
    id))

;;;-----------------------------------------------------------------------------
;;;GET-SOURCE-ID
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/09/01	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun get-source-id ()
  (beginning-of-line)
  (next-non-space)
  (forward-char 1)
  (forward-sexp)
  (next-non-space)
  (let ((start (point)))
    (forward-sexp)
    (upcase (buffer-substring start (point)))))

;;;-----------------------------------------------------------------------------
;;;REMOVE-SOURCE-ID
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/09/01	J. P. Varandas	Created
;;;	09/09/23	P. Madeira	`kill-line' -> `sc-kill-line'
;;;-----------------------------------------------------------------------------
(defun remove-source-id ()
  (beginning-of-line)
  (next-non-space)
  (forward-char 1)
  (forward-sexp)
  (next-non-space)
  (let ((start (point))
	(id nil))
    (forward-sexp)
    (setq id (upcase (buffer-substring start (point))))
    (beginning-of-line)
    (sc-kill-line)
    (sc-kill-line)
    id))

;;;-----------------------------------------------------------------------------
;;;GET-KEYWORD-ID
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/09/23	P. Madeira	Fix for `common-lisp-mode's `forward-sexp'.
;;;					`previous-line' -> `forward-line' (EMACS 23).
;;;-----------------------------------------------------------------------------
(defun get-keyword-id ()
  (beginning-of-line)
  (while (not (and (= (char-after (point)) 126)
		   (= (char-after (+ (point) 1)) 34)))
    (forward-line -1)
    (beginning-of-line))
  (next-non-space)
  (forward-char)
  (let ((start (point)))
    (forward-sexp)
    (buffer-substring start (point))))

;;;-----------------------------------------------------------------------------
;;;GET-KEYWORD-IDS
;;;Description
;;;	Collects the list of keywords for a first catch translation keyword
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of \emph{strings}
;;;History
;;;	Date		Author		Description
;;;	99/10/19	J. P. Varandas	Created
;;;	05/03/21	J. P. Varandas	Protects the system against infinite cycle 
;;;					when the last translation keyword of a dic file
;;;					does not have a CR.
;;;-----------------------------------------------------------------------------
(defun get-keyword-ids ()
  (let ((keys (list (get-keyword-id)))
	(not-end t))
    (while not-end
      (if (zerop (forward-line 1))
	  (progn
	    (beginning-of-line)
	    (next-non-space)
	    (if (or (null (char-after (point)))
		    (and (= (char-after (point)) 126)
			 (= (char-after (+ (point) 1)) 34)))
		(setf not-end nil)
		(let ((start (point)))
		  (forward-sexp)
		  (setf keys (cons (buffer-substring start (point)) keys))))
	    (end-of-line))
	  (setf not-end nil)))
    (nreverse keys)))

;;;-----------------------------------------------------------------------------
;;;GET-TRANSLATION-LANGUAGES
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/10/19	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun get-translation-languages ()
  (save-excursion
    (beginning-of-buffer)
    (when (search-forward "(in-package" nil t)
      (forward-line 1)
      (when (search-forward ":" nil t)
	(beginning-of-line)
	(let ((start (point)))
	  (end-of-line)
	  (let ((string (buffer-substring start (point))))
	    (car (read-from-string string))))))))

;;;-----------------------------------------------------------------------------
;;;REMOVE-KEYWORD-ID
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/09/23	P. Madeira	Fix for `common-lisp-mode's `forward-sexp'.
;;;					`kill-line' -> `sc-kill-line'.
;;;					`previous-line' -> `forward-line' (EMACS 23).
;;;-----------------------------------------------------------------------------
(defun remove-keyword-id ()
  (beginning-of-line)
  (while (not (and (= (char-after (point)) 126)
		   (= (char-after (+ (point) 1)) 34)))
    (forward-line -1)
    (beginning-of-line))
  (next-non-space)
  (forward-char)
  (let ((start (point))
	(id nil))
    (forward-sexp)
    (setq id (buffer-substring start (point)))
    (beginning-of-line)
    (sc-kill-line)
    (sc-kill-line)
    (beginning-of-line)
    (while (not (and (= (char-after (point)) 126)
		     (= (char-after (+ (point) 1)) 34)))
      (beginning-of-line)
      (sc-kill-line)
      (beginning-of-line))
    id))

;;;-----------------------------------------------------------------------------
;;;GET-C-NEXT-NAME
;;;Description
;;;	Returns the next word in the current buffer. Similar to get-next-name
;;;	but with no upcase
;;;History
;;;	Date		Author		Description
;;;	99/08/24	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun get-c-next-name ()
  (next-non-space)
  (let ((start (point)))
    (forward-sexp)
    (buffer-substring start (point))))

;;;-----------------------------------------------------------------------------
;;;GET-C-DEFINITION-ID
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/01/01	Joao Filipe	
;;;	99/08/24	A. Frazao	Calls get-c-next-name
;;;-----------------------------------------------------------------------------
(defun get-c-definition-id ()
  (ignore-errors
    (beginning-of-definition)
    (while (not (or (= (char-after (point)) 40) (= (char-after (point)) 123)))
      (forward-sexp)
      (next-non-space))
    (backward-sexp)
    (get-c-next-name)))


;;;-----------------------------------------------------------------------------
;;;SC-MOD-DEFINITION-CONTAINERS
;;;Description
;;;	Specifies top-level forms to skip when looking for the current
;;;     definition.
;;;	::= ((<symbol def> . <number of argument forms to skip>)*)
;;;	<symbol def> is an upper-case symbol name.
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	08/02/22	Tom Weissmann	Created
;;;-----------------------------------------------------------------------------
(defvar sc-mod-definition-containers
  '(("LET" 	. 1)
    ("LET*" 	. 1)
    ("UNLESS"   . 1)
    ("EVAL-WHEN" . 1)
    ("WITH-STABLE-STRING-OUTPUT-STREAM" . 1)))


;;;-----------------------------------------------------------------------------
;;;SC-MOD-DEFINITION-HANDLERS
;;;Description
;;;	Specifies how to create the documentation name of defintions
;;;	that require more than just the name passed to the defining
;;;	form (eg, `defun').
;;;
;;;	Each item in the list has the form ((<definer> . <name spec>)*)
;;;	<definer> is an upper-case symbol name, or a list of them.
;;;	<name spec> is a string, which is applied to the name
;;;	of the symbol after the definer to produce its name, or a lisp
;;;	form which is evaluated to get the name.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	08/02/22	Tom Weissmann	Created (POA 12701.0).
;;;	08/02/25	T. Maduro Dias	Removed erroneous setq (POA 12701.0)
;;;	08/09/03	A. Frazao	Corrected defstruct handler (PMS 14051)
;;;	09/01/19	Rui Patrocínio	Corrected export handler (POA 14970.0)
;;;	09/02/02	J. P. Varandas	Corrected the DEFINE-PROJECT handler
;;;	09/05/06	J. P. Varandas	Special cases for
;;;					  MAP.OBJECTS.TO.COMMON.TOPICS
;;;					  MAP.OBJECTS.TO.APP.TOPICS
;;;					  INSTALL.MENU.ITEM.SHORTCUTS
;;;-----------------------------------------------------------------------------
(defvar sc-mod-definition-handlers
   '(("SET-SYTEM-SOURCE-FILE" .	"System Source %s")
    ("DEFPACKAGE"  	.  	"Package %s")
    ("USE-PACKAGE"  	.  	"Use package %s")
    ("DEFSTRUCT"  	.  	(format "DEFSTRUCT %s" (get-defstruct-name)))
    ("DEFSYSTEM"  	.  	"SYSTEM %s")
    ("EXPORT"  		.  	"EXPORTATIONS")
    ("SETF"  		.  	"(SETF %s)")
    ("SETQ"  		.  	"(SETQ %s)")
    ("DEF.COLOR"  	.  	 "COLOR %s")
    ("DEF.GCONTEXT"  	.  	"GCONTEXT %s")
    ("DEF.GET.INST"  	.  	"GET.%s")
    ("DEF.FIND.INST"  	.  	"FIND.%s")
    ("DEF.CONSTRUCTOR"  .  	"MAKE.%s")
    ("SET-MACRO-CHARACTER"  	.	"(SET-MACRO-CHARACTER %s)")
    ("DEFMETHOD.IS?"  		.  	"DEFMETHOD.IS? %s")
    (("DEFINE-PROJECT")
     .	(let ((name    (get-next-name))
	      (project (get-next-name))) (format "(DEFINE-PROJECT %s %s)" name project)))
    ("DEF.CONSTRUCTOR&NAME"  		.  	"MAKE.%s!")
    ("DEF.EXTERNAL.CONSTRUCTOR"  	.  	"DEF.EXTERNAL.CONSTRUCTOR")
    ("DEF.ROSTER.DELEGATED.METHOD"  	.  	"(DEF.ROSTER.DELEGATED.METHOD %s)")
    ("DEF.BASIC.DUTY.SETF.AROUND"  	.  	"(DEF.BASIC.DUTY.SETF.AROUND %s)")
    ("REMOVE.FROM.PROBLEM.LOCAL.FILES"  .  	"REMOVE.FROM.PROBLEM.LOCAL.FILES %s")
    ;; more complex ones
    ("DEFINE.FUNCTIONALITY"
     .	(format "%s"
	 (replace-regexp-in-string "^:" "" (get-next-name))))
    (("DEF-FOREIGN-CALLABLE" "DEF-FOREIGN-FUNCTION")
     . (get-foreign-name))
    (("DEFMETHOD" "DEF.TR.CLASS" "DEF.TR" "DEF.AUX" "DEF.LR")
     . (get-method-id))
    (("DEF.DELEGATED.METHOD" "DEF.DELEGATED.METHOD.IF")
     . (let ((class    (get-next-name))
	     (accessor (get-next-name))
	     (name     (get-next-name))) (format "%s (%s)" name class)))
    (("ADD.BATCH.VALID.COMMAND")
     . (let ((command (replace-regexp-in-string "^'" "" (get-next-name)))
	     (application (replace-regexp-in-string "^:" "" (get-next-name))))
	 (format "ADD.BATCH.VALID.COMMAND %s (%s)" command application)))
     ("MAP.OBJECTS.TO.COMMON.TOPICS"  	.  	"MAP.OBJECTS.TO.COMMON.TOPICS")
     ("MAP.OBJECTS.TO.APP.TOPICS"  	.  	"MAP.OBJECTS.TO.APP.TOPICS")
     ("INSTALL.MENU.ITEM.SHORTCUTS"  	.  	"INSTALL.MENU.ITEM.SHORTCUTS")
     ))


;;;-----------------------------------------------------------------------------
;;;SC-MOD-GET-DEFINITION-HANDLER
;;;Description
;;;	Return a handler function that will create the documentation name of a
;;;	definition - if the definer needs it.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{definer} is an upper-case \emph{string}, the symbol name of
;;;		definer (eg "DEFMETHOD").
;;;		
;;;	\return-types
;;;		\emph{NIL} if the definer has no special handler, or a function
;;;		that no arguments and when called with point at the buffer position
;;;		following the definer and before its arguments, and returns the
;;;		documentation name of the definition.
;;;	\refs
;;;		`sc-mod-definition-handlers'
;;;
;;;History
;;;	Date		Author		Description
;;;	08/02/22	Tom Weissmann	Created (POA 12701.0).
;;;-----------------------------------------------------------------------------
(defun sc-mod-get-definition-handler (definer)
  (let ((info (cdr (assoc* definer sc-mod-definition-handlers
			   :key 'ensure-list
			   :test 'member))))
    (when info
      `(lambda ()
	 ,(cond ((consp info) info)
		(t `(format ,info (get-next-name))))))))


;;;-----------------------------------------------------------------------------
;;;GET-LISP-DEFINITION-ID
;;;Description
;;;	Computes the ID of the definition where the mouse in pointing out.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;	
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Remove \ in defs. DEF.LR is the same as
;;;					defmethod.
;;;	99/08/31	J. P. Varandas	Deixa de estar dependente do *defstrings*
;;;					Salta por cima de algumas expressões tipo "let" que aparecem antes de um defun
;;;	99/09/01	J. P. Varandas	Tratamento especial do "set-system-source-file"
;;;	99/09/07	J. P. Varandas	Tratamento do "use-package"
;;;	00/06/08	Dario		Special treatment for "setf".
;;;	01/01/05	Dario		Special treatment for
;;;					"def.external.constructor"
;;;					"set-macro-character"
;;;					"def.roster.delegated.method"
;;;					"defmethod.is?"
;;;					"def.basic.duty.setf.around"
;;;					"define-project"
;;;	01/07/04	J. P. Varandas	Special treatment for
;;;					"def.color"
;;;					"def.gcontext"
;;;	01/10/12	Dario		Special treatment for
;;;					"remove.from.problem.local.files"
;;;	02/01/18	Dario		Special treatment for
;;;					"setq"
;;;	02/02/28	A. Frazao	Special treatment for
;;;					"def.delegated.method"
;;;					"def.delegated.method.if"
;;;	02/06/07	J. P. Varandas	Special treatment for
;;;					"map.objects.to.common.topics"
;;;					"map.objects.to.app.topics"
;;;					"map.objects.to.labels"
;;;	06/04/11	J. P. Varandas	Special treatment for
;;;					"install.menu.item.shortcuts"
;;;	08/02/22	Tom Weissmann	Moved treatment of forms to \ref{sc-mod-definition-containers}
;;;                                     and \ref{sc-mod-get-definition-handler} (POA 12701.0).
;;;-----------------------------------------------------------------------------
(defun get-lisp-definition-id ()
  (labels ((skip-container-forms ()
	     (let ((args-to-skip
		    (cdr (assoc (get-current-name) sc-mod-definition-containers))))
	       (when args-to-skip
		 (forward-sexp (1+ args-to-skip)) ; skip the definer as well as its arguments
		 (down-list)
		 (skip-container-forms)))))
    (save-restriction 
      (narrow-to-defun)
      (save-excursion 
	(beginning-of-defun)
	(down-list)
	(skip-container-forms)
	;; Handle different kinds of definition
	(let* ((definer (get-next-name))
	       (handler (sc-mod-get-definition-handler definer)))
	  (if handler
	      (funcall handler)
	      (get-next-name)))))))




;;;-----------------------------------------------------------------------------
;;;GET-HTML-DEFINITION-ID
;;;Description
;;;	Gets the identifier of the HTML definition where \elem{point} lies on.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A string or \emph{nil}. When is a string it is the identifier of
;;;		the definition. When is \{nil} it indicates that the underlying
;;;		constructor is unknown and so the definition identifier could
;;;		not be obtained.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun get-html-definition-id ()
  (beginning-of-definition)
   
  ;; For the time being supports only 3 HTML definition ids: "<HEAD>", 
  ;; "<BODY>" and "<FRAMESET>".
  
  (let* ((line-prefix (buffer-substring (point) (+ (point) 5)))
	 (line-prefix-downcase (downcase line-prefix))
	 (definition-id nil))
    
    (cond ((or (string-equal line-prefix-downcase "<body")
	       (string-equal line-prefix-downcase "<head"))
	   ;; The definition id is either "<HEAD>" or "<BODY>".
	   (setq definition-id (format "%s>" line-prefix)))
	  (t
	   ;; The definition id is "<FRAMESET>".
	   (setq definition-id (format "%s>" (buffer-substring (point) (+ (point) 9))))))
    
    definition-id))

;;;-----------------------------------------------------------------------------
;;;GET-JSCRIPT-DEFINITION-ID
;;;Description
;;;	Gets the identifier of the JavaScript definition where \elem{point} lies
;;;	on.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A string or \emph{nil}. When is a string it is the identifier of
;;;		the definition. When is \{nil} it indicates that the underlying
;;;		constructor is unknown and so the definition identifier could
;;;		not be obtained.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun get-jscript-definition-id ()
  (beginning-of-definition)
  (let* ((initial-point (point))
	 (line (downcase (buffer-substring (point) (line-end-position))))
	 (line-length (length line))
	 (definition-id nil))
    
    (cond ((and (>= line-length 8)
		(string-equal (substring line 0 8) "function")
		(search-forward-regexp re-jscript-fn-header (point-max) t 1)
		(= (match-beginning 0) initial-point))
	   ;; It is a function definition.
	   (goto-char initial-point)
	   (search-forward-regexp (format "function%s" re-1-or-more-whitespaces)
				  (point-max) t 1)
	   (search-forward-regexp re-jscript-name (point-max) t 1)
	   (setq definition-id (match-string 0)))

	  ((and (>= line-length 3)
		(string-equal (substring line 0 3) "var")
		(search-forward-regexp re-jscript-var-decl-with-var (point-max) t 1)
		(= (match-beginning 0) initial-point))
	   ;; It is a variable declaration using the "var" keyword.
	   (goto-char initial-point)
	   (search-forward-regexp (format "var%s" re-1-or-more-whitespaces)
				  (point-max) t 1)
	   (search-forward-regexp re-jscript-name (point-max) t 1)
	   (setq definition-id (match-string 0)))

	  ((and (search-forward-regexp re-jscript-var-decl-without-var (point-max) t 1)
		(= (match-beginning 0) initial-point))
	   ;; It is a variable declaration without using the "var" keyword.
	   (goto-char initial-point)
	   (search-forward-regexp re-jscript-name (point-max) t 1)
	   (setq definition-id (match-string 0))))

    ;; Returns 'point' to its initial position. 
    (goto-char initial-point)
    
    definition-id))

;;;-----------------------------------------------------------------------------
;;;GET-PHP-DEFINITION-ID
;;;Description
;;;	Gets the identifier of the PHP definition where \elem{point} lies
;;;	on.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A string or \emph{nil}. When is a string it is the identifier of
;;;		the definition. When is \{nil} it indicates that the underlying
;;;		constructor is unknown and so the definition identifier could
;;;		not be obtained.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun get-php-definition-id ()
  (beginning-of-definition)
  (let* ((initial-point (point))
	 (line (downcase (buffer-substring (point) (line-end-position))))
	 (line-length (length line))
	 (definition-id nil))
    
    (cond ((and (>= line-length 8)
		(string-equal (substring line 0 8) "function")
		(search-forward-regexp re-php-fn-header (point-max) t 1)
		(= (match-beginning 0) initial-point))
	   ;; It is a function definition.
	   (goto-char initial-point)
	   (search-forward-regexp (format "function%s" re-1-or-more-whitespaces)
				  (point-max) t 1)
	   (search-forward-regexp re-php-name (point-max) t 1)
	   (setq definition-id (match-string 0)))

	  ((and (search-forward-regexp re-php-var-decl (point-max) t 1)
		(= (match-beginning 0) initial-point))
	   ;; It is a variable declaration.
	   (goto-char initial-point)
	   (search-forward-regexp re-php-name (point-max) t 1)
	   (setq definition-id (match-string 0)))
	  
	  ((and (>= line-length 6)
		(string-equal (substring line 0 6) "define")
		(search-forward-regexp re-php-const-decl (point-max) t 1)
		(= (match-beginning 0) initial-point))
	   ;; It is a constant declaration.
	   (goto-char initial-point)
	   (search-forward-regexp (format "define%s(%s\""
					  re-0-or-more-whitespaces
					  re-0-or-more-whitespaces)
				  (point-max) t 1)
	   (search-forward-regexp re-php-name (point-max) t 1)
	   (setq definition-id (match-string 0))))

    ;; Returns 'point' to its initial position. 
    (goto-char initial-point)

    definition-id))

;;;-----------------------------------------------------------------------------
;;;GET-VB-DEFINITION-ID
;;;Description
;;;	Gets the identifier of the VBScript definition where \elem{point} lies
;;;	on.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A string or \emph{nil}. When is a string it is the identifier of
;;;		the definition. When is \{nil} it indicates that the underlying
;;;		constructor is unknown and so the definition identifier could
;;;		not be obtained.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Created
;;;-----------------------------------------------------------------------------
(defun get-vb-definition-id ()
  (beginning-of-definition)
  (let* ((initial-point (point))
	 (line (downcase (buffer-substring (point) (line-end-position))))
	 (line-length (length line))
	 (definition-id nil))
    
    (cond ((and (>= line-length 8)
		(string-equal (substring line 0 8) "function")
		(search-forward-regexp re-vb-fn-header (point-max) t 1)
		(= (match-beginning 0) initial-point))
	   ;; It is a function definition.
	   (goto-char initial-point)
	   (search-forward-regexp (format "Function%s" re-1-or-more-whitespaces)
				  (point-max) t 1)
	   (search-forward-regexp re-vb-name (point-max) t 1)
	   (setq definition-id (match-string 0)))

	  ((and (>= line-length 3)
		(string-equal (substring line 0 3) "sub")
		(search-forward-regexp re-vb-fn-header (point-max) t 1)
		(= (match-beginning 0) initial-point))
	   ;; It is a procedure definition.
	   (goto-char initial-point)
	   (search-forward-regexp (format "Sub%s" re-1-or-more-whitespaces)
				  (point-max) t 1)
	   (search-forward-regexp re-vb-name (point-max) t 1)
	   (setq definition-id (match-string 0)))
	  
	  ((and (>= line-length 3)
		(string-equal (substring line 0 3) "dim")
		(search-forward-regexp re-vb-var-decl (point-max) t 1)
		(= (match-beginning 0) initial-point))
	   ;; It is a variable declaration.
	   (goto-char initial-point)
	   (search-forward-regexp (format "Dim%s" re-1-or-more-whitespaces)
				  (point-max) t 1)
	   (search-forward-regexp re-vb-name (point-max) t 1)
	   (setq definition-id (match-string 0)))
	  
	  ((and (>= line-length 5)
		(string-equal (substring line 0 5) "const")
		(search-forward-regexp re-vb-const-decl (point-max) t 1)
		(= (match-beginning 0) initial-point))
	   ;; It is a constant declaration.
	   (goto-char initial-point)
	   (search-forward-regexp (format "Const%s"
					  re-1-or-more-whitespaces)
				  (point-max) t 1)
	   (search-forward-regexp re-vb-name (point-max) t 1)
	   (setq definition-id (match-string 0))))

    ;; Returns 'point' to its initial position. 
    (goto-char initial-point)
    
    definition-id))

;;;-----------------------------------------------------------------------------
;;;GET-DEFINITION-ID
;;;Description
;;;	Gets the identifier of the definition where \elem{point} lies on.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A string. It is either the identifier of the definition, some
;;;		other string or \emph{"UNKNOWN CONSTRUCTOR!!!"}. In the later
;;;		two cases it indicates that the underlying constructor is
;;;		unknown and so the definition identifier could not be obtained.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/03/02	Toni		Added support for HTML-MODE, PHP-MODE
;;;					and VB-MODE.
;;;					Doc updated.
;;;	01/10/16	Toni		Added support for JSCRIPT-MODE.
;;;	08/07/29	A. Frazao	In JSCRIPT-MODE do not add tag in definition ID
;;;-----------------------------------------------------------------------------
(defun get-definition-id ()
  (let ((debugging-this-function nil))

    ;; Code for debugging.
    (if debugging-this-function
	(message "ENTERING ----- get-definition-id ----- ENTERING"))

    (let ((file-mode (file-mode))
	  (language (save-excursion (get-definition-language)))
	  (definition-id nil))
      
      (cond ((eq file-mode C-MODE)
	     ;; C language.
	     (setq definition-id (get-c-definition-id)))
	    
	    ((or (eq file-mode HTML-MODE)
		 (eq file-mode JSCRIPT-MODE)
		 (eq file-mode PHP-MODE)
		 (eq file-mode VB-MODE))
	     ;; HTML, Javascript, PHP and VBScript languages.
	     (setq definition-id
	       (cond ((eq language HTML-LANGUAGE)
		      (get-html-definition-id))
		     ((eq language JSCRIPT-LANGUAGE)
		      (get-jscript-definition-id))
		     ((eq language PHP-LANGUAGE)
		      (get-php-definition-id))
		     ((eq language VB-LANGUAGE)
		      (get-vb-definition-id)))))
	    
	    (t
	     ;; Lisp language and by default also other languages.
	     (setq definition-id (get-lisp-definition-id))))
      
      (if (eq definition-id nil)
	  ;; The definition id could not be obtained, maybe because the
	  ;; underlying constructor is unknown.
	  ;;
	  ;; Set the definition id to the first non-blank sequence of
	  ;; characters in the beginning of the definition.
	  (setq definition-id (if (search-forward-regexp "[^ \t]+" (line-end-position) t 1)
				  (match-string 0)
				  "UNKNOWN CONSTRUCTOR !!!")))
      
      ;; Since may be used several Web languages in a single file, append an
      ;; identification of the underlying programming language at the beginning
      ;; of the definition id.
      (setq definition-id (cond ((or (eq file-mode PHP-MODE)
				     (eq file-mode HTML-MODE)
				     (eq file-mode VB-MODE))
				 (cond ((eq language HTML-LANGUAGE)
					(format "%s %s" "[HTM]" definition-id))
				       ((eq language JSCRIPT-LANGUAGE)
					(format "%s %s" "[JVS]" definition-id))
				       ((eq language PHP-LANGUAGE)
					(format "%s %s" "[PHP]" definition-id))
				       ((eq language VB-LANGUAGE)
					(format "%s %s" "[VBS]" definition-id))
				       (t
					;; For other languages leave the definition id
					;; as is.
					definition-id)))
				(t definition-id)))

      ;; Code for debugging.
      (if debugging-this-function
	  (progn
	    (message "Definition ID: %s" definition-id)
	    (message "LEAVING ----- get-definition-id ----- LEAVING")))
      
      definition-id)))

;;;-----------------------------------------------------------------------------
;;;ADD-DEFINITION-MOD-LINE
;;;Description
;;;	Adds a change signature to the header of the definition where
;;;	\elem{point} lies on so that the user may describe the change.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{definition} is a string. It is the definition identifier.
;;;		
;;;		\arg{description} is a string. It is a default description for
;;;		the current change that the user is making on the definition.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	
;;;	99/08/31	J. P. Varandas	Se a definição já foi modificada não a marca novamente
;;;	00/01/31	J. P. Varandas	Call the function 'insert.mandatory.attributes'
;;;	00/02/18	J. P. Varandas	Ignore errors when INSERT.MANDATORY.ATTRIBUTES
;;;	01/03/02	Toni		Added support for HTML-MODE, PHP-MODE
;;;					and VB-MODE by generalising.
;;;					Doc updated
;;;	05/03/21	J. P. Varandas	Inserts the modification reference in the description
;;;	08/02/22	Tom Weissmann	Restructured the code to ensure the mod reference is always added
;;;					if it exists (POA 12706.0).
;;;	09/09/23	P. Madeira	`kill-line' -> `sc-kill-line'
;;;-----------------------------------------------------------------------------
(defun add-definition-mod-line (definition description)
  (destructuring-bind (comment filler multi-line-comment-open multi-line-comment-close &rest ignore)
      (mode-fillers)
    (beginning-of-definition)
    (let ((start (point)))
      ;; To the filler after the last entry in the header
      (forward-line (if multi-line-comment-close -2 -1))
      ;; Ensure the definition has a header
      (unless (looking-at filler)
	;; return to the beginning of the definition
	(goto-char start)
	(when multi-line-comment-open (insert multi-line-comment-open "\n"))
	(mapc #'insert
	      (list
	       filler 								"\n"
	       comment (or definition "") 					"\n"
	       comment "Description"						"\n"
	       comment "\t"							"\n"
	       comment "History"						"\n"
	       comment "\tDate\t\tAuthor\t\tDescription"		 	"\n"
	       filler 								"\n"))
	(when multi-line-comment-close (insert multi-line-comment-close 	"\n"))
	(forward-line (if multi-line-comment-close -2 -1)))

      (when (looking-at filler) ; should always be t after the previous code.
	(if (has-modified)
	    ;; The header already has an entry for the current change
	    (beep)
	    ;; Insert a signature for the current change
	    (let ((reference-string (if (current-mod-reference)
					(format " (%s)" (current-mod-reference))
					"")))
	      (insert (concat comment "\t"
			      (current-mod-date-string) "\t"
			      (current-mod-author) "\t"
			      (concat description reference-string) "\n"))
	      (save-excursion 
		(ignore-errors
		  (insert.mandatory.attributes)))
	      ;; Update the definition title in case it's changed.
	      (when definition
		(forward-line -1)
		(when (search-backward filler nil t)
		  (forward-line 1)
		  (sc-kill-line)
		  (insert comment definition)))))))))

;;;-----------------------------------------------------------------------------
;;;ADD-DEFINITION-HEADER
;;;Description
;;;	Adds a header to the definition where \elem{point} lies on.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{definition} is a string. It is the definition identifier.
;;;		
;;;		\arg{description} is a string. It is a default description for
;;;		the current change that the user is making on the definition.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	00/01/31	J. P. Varandas	Call the function 'insert.mandatory.attributes'
;;;	01/03/02	Toni		Added support for HTML-MODE, PHP-MODE
;;;					and VB-MODE by generalising.
;;;					Doc updated
;;;	09/09/23	P. Madeira	`kill-line' -> `sc-kill-line'.
;;;					`next-line' -> `forward-line' (EMACS 23).
;;;					`previous-line' -> `forward-line' (EMACS 23).
;;;-----------------------------------------------------------------------------
(defun add-definition-header (definition description)
  (let* ((mode-fillers             (mode-fillers))
	 (comment                  (nth 0 mode-fillers))
	 (filler                   (nth 1 mode-fillers))
	 (multi-line-comment-open  (nth 2 mode-fillers))
	 (multi-line-comment-close (nth 3 mode-fillers)))
    (beginning-of-definition)
    (forward-line -1)
    (when multi-line-comment-close
      (forward-line -1))
    
    (if (looking-at filler)
	
	;; The definition already has an header.
	;;
	;; Inserts a signature for the user in the header.
	(progn
	  (forward-line -1)
	  (save-excursion (insert.mandatory.attributes))
	  (when (search-backward filler nil t)
	    (forward-line 1)
	    (beginning-of-line)
	    (sc-kill-line)
	    (insert comment (or definition ""))))
	
	
	;; The definition has not an header yet.
	;;
	;; Inserts an header for the definition as well as a signature for the
	;; user to describe the current change.
	(progn
	  (forward-line 1)
	  (when multi-line-comment-open
	    (forward-line 1)
	    (insert multi-line-comment-open 10))
	  (insert filler 10)
	  (insert comment (or definition "") 10)
	  (insert comment "Description" 10)
	  (insert comment 9 10)
	  (insert comment "History" 10)
	  (insert comment 9 "Date" 9 9 "Author" 9 9 "Description" 10)
	  (insert comment 9 9 9 (current-mod-author) 9 description 10)
	  (insert filler 10)
	  (when multi-line-comment-close
	    (insert multi-line-comment-close 10))
	  (save-excursion
	    (forward-line -2)
	    (insert.mandatory.attributes))
	  ))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-MODIFIED
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	
;;;	99/08/31	J. P. Varandas	Chama a função 'add-definition-entry'
;;;-----------------------------------------------------------------------------
(defun set-definition-modified (definition pattern)
  (add-definition-entry definition pattern))

;;;-----------------------------------------------------------------------------
;;;ADD-DEFINITION-ENTRY
;;;Description
;;;	Adds the identifier of a definition to the file header in the signature
;;;	of the user for the current modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{definition} is a string. It is the definition identifier.
;;;		
;;;		\arg{pattern} is a string. It indicates the kind of change over
;;;		the definition, like for example, whether it was added, modified
;;;		or deleted.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	99/09/01	J. P. Varandas	Protects when pattern is NIL
;;;	99/10/19	J. P. Varandas	Protects when the definition is a substring 
;;;					of another that was inserted previously
;;;	01/03/02	Toni		Added support for HTML-MODE, PHP-MODE
;;;					and VB-MODE by generalising.
;;;					Doc updated
;;;	04/01/05	Toni		Replaced (= (char-after (point)) comment-char)
;;;					for (looking-at comment) and (forward-char (length comment)).
;;;					This is needed because otherwise it fails
;;;					when working with '.bat' files, where
;;;					the comment characters are not all equal.
;;;					Variable 'comment-char' has been removed,
;;;					since it is no longer used.
;;;	08/07/29	A. Frazao	Calls beginning-of-file-header
;;;	09/09/23	P. Madeira	`next-line' -> `forward-line' (EMACS 23).
;;;					`previous-line' -> `forward-line' (EMACS 23).
;;;-----------------------------------------------------------------------------
(defun add-definition-entry (definition pattern)
  (let* ((mode-fillers            (mode-fillers))
	 (comment                 (nth 4 mode-fillers))
	 (filler                  (nth 5 mode-fillers))
	 (multi-line-comment-open (nth 6 mode-fillers))
	 (date (current-mod-date-string))
	 (user (current-mod-author))
	 (found nil)
	 (failed nil))
    (beginning-of-file-header)
    (when multi-line-comment-open
      (forward-line 1))
    
    ;; Look for the history
    (while (and (not found) (not failed))
      (if (not (looking-at filler))
	  (if (looking-at comment)
	      (progn
		(forward-char (length comment))
		(while (and (or (= (char-after (point)) 32)
				(= (char-after (point)) 9))
			    (not (= (char-after (point)) 10)))
		  (forward-char 1))
		(if (looking-at "History")
		    (setq found t)))
	      (setq failed "Header is not in correct format")))
      (line-feed))
    
    ;; Look for the date
    (setq found nil)
    (beginning-of-line)
    (while (and (not found) (not failed))
      (cond ((looking-at filler)
	     (insert comment 9 date 9 user 9 (or pattern "") 10)
	     (forward-line -1)
	     (setq found t))
	    ((looking-at comment)
	     (forward-char (length comment))
	     (while (and (or (= (char-after (point)) 32)
			     (= (char-after (point)) 9))
			 (not (= (char-after (point)) 10)))
	       (forward-char 1))
	     (if (looking-at date)
		 (progn
		   (while (not (or (= (char-after (point)) 32)
				   (= (char-after (point)) 9)))
		     (forward-char 1))
		   (next-non-space)
		   (if (looking-at user)
		       (setq found t)
		       (line-feed)))
		 (line-feed)))
	    (t (setq failed "Header is not in correct format"))))
    
    ;; Inserts the definition identifier in the list for the given pattern.
    (when pattern
      (let ((def-str (format "%s					  %s\n" comment definition))
	    (found-str nil))
	(save-excursion
	  (if (search-forward def-str nil t)
	      (setq found-str 1)
	      (setq found-str 0)))
	(when (= found-str 0)
	  (let ((len (length pattern)))
	    (setq found nil)
	    (setq failed nil)
	    (while (and (not found) (not failed))
	      (end-of-line)
	      (forward-char (- len))
	      (cond ((save-excursion (looking-at pattern))
		     (line-feed)
		     (insert def-str)
		     (setq found t))
		    (t (forward-char len)
		       (beginning-of-line)
		       (cond ((looking-at filler)
			      (insert comment 9 9 9 9 9 pattern 10)
			      (insert def-str)
			      (setq found t))
			     ((not (looking-at comment))
			      (setq failed "Header is not in correct format"))
			     (t (forward-line 1))))))))))
    
    (if failed
	(beep-message failed))
    ))

;;;-----------------------------------------------------------------------------
;;;SET-SYSTEM-MOD-LINE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	99/09/01	J. P. Varandas	Correct a bug
;;;	99/10/19	J. P. Varandas	Protects when the definition is a substring 
;;;					of another that was inserted previously
;;;	09/09/23	P. Madeira	`next-line' -> `forward-line' (EMACS 23).
;;;					`previous-line' -> `forward-line' (EMACS 23).
;;;-----------------------------------------------------------------------------
(defun set-system-mod-line (definition pattern symbol)
  (let* ((c-mode (eq (file-mode) C-MODE))
	 (mode-fillers (mode-fillers))
	 (comment (car mode-fillers))
	 (filler (cadr mode-fillers))
	 (comment-char (aref comment 0))
	 (found nil)
	 (failed nil))
    (beginning-of-definition)
    (forward-line -1)
    (when c-mode
      (forward-line -1))
    (if (looking-at filler)
	(progn
	  (forward-line -1)
	  (when (search-backward filler nil t)
	    (while (not found)
	      (if (not (looking-at filler))
		  (if (= (char-after (point)) comment-char)
		      (progn
			(while (and (or (= (char-after (point)) comment-char)
					(= (char-after (point)) 32)
					(= (char-after (point)) 9))
				    (not (= (char-after (point)) 10)))
			  (forward-char 1))
			(if (looking-at "History")
			    (setq found t)))))
	      (line-feed))
	    (setq found nil)
	    (beginning-of-line)
	    (while (not found)
	      (cond ((looking-at filler)
		     (insert comment 9 (current-mod-date-string) 9 (current-mod-author) 9 pattern 10)
		     (forward-line -1)
		     (setq found t))
		    ((= (char-after (point)) comment-char)
		     (while (and (or (= (char-after (point)) comment-char)
				     (= (char-after (point)) 32)
				     (= (char-after (point)) 9))
				 (not (= (char-after (point)) 10)))
		       (forward-char 1))
		     (if (looking-at (current-mod-date-string))
			 (progn
			   (while (not (or (= (char-after (point)) 32)
					   (= (char-after (point)) 9)))
			     (forward-char 1))
			   (next-non-space)
			   (if (looking-at (current-mod-author))
			       (setq found t)
			       (line-feed)))
			 (line-feed)))))
	    (let ((def-str (format "%s					  %s\n" comment symbol))
		  (len (length pattern))
		  (found-str nil))
	      (save-excursion
		(while (not found-str)
		  (if (looking-at filler)
		      (setq found-str 0)
		      (if (looking-at def-str)
			  (setq found-str 1)
			  (line-feed)))))
	      (when (= found-str 0)
		(setq found nil)
		(while (not found)
		  (end-of-line)
		  (forward-char (- len))
		  (cond ((save-excursion (looking-at pattern))
			 (line-feed)
			 (insert def-str)
			 (setq found t))
			(t (forward-char len)
			   (beginning-of-line)
			   (cond ((looking-at filler)
				  (insert comment 9 9 9 9 9 pattern 10)
				  (insert def-str)
				  (setq found t))
				 (t (forward-line 1))))))))))
	(progn
	  (forward-line 1)
	  (when c-mode
	    (forward-line 1)
	    (insert "/*" 10))
	  (insert filler 10)
	  (insert comment definition 10)
	  (insert comment "Description" 10)
	  (insert comment 9 10)
	  (insert comment "History" 10)
	  (insert comment 9 "Date" 9 9 "Author" 9 9 "Description" 10)
	  (insert comment 9 (current-mod-date-string) 9 (current-mod-author) 9 pattern 10)
	  (insert filler 10)
	  (when c-mode
	    (insert "*/" 10))))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-CLEAR
;;;Description
;;;	Deletes the definition where \emph{point} lies on. The corresponding
;;;	header is deleted as well.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	01/03/02	Toni		Added support for HTML-MODE, PHP-MODE
;;;					and VB-MODE by generalising.
;;;					Doc updated
;;;	09/09/23	P. Madeira	`previous-line' -> `forward-line' (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun set-definition-clear ()
  (let* ((mode-fillers             (mode-fillers))
	 (filler                   (nth 1 mode-fillers))
	 (multi-line-comment-open  (nth 2 mode-fillers))
	 (multi-line-comment-close (nth 3 mode-fillers)))
    (let (start end)
      
      ;; Deletes the definition first.
      (save-excursion
	(end-of-definition)
	(setq end (point))
	(beginning-of-definition t)
	(setq start (point))
	(delete-region start end))
      
      ;; Now deletes the corresponding header, if there is one.
      (save-excursion
	(setq end (point))
	(forward-line -1)
	(when multi-line-comment-close
	  (forward-line -1))
	(if (looking-at filler)
	    (progn
	      (forward-line -1)
	      (while (not (looking-at filler))
		(forward-line -1))
	      (when multi-line-comment-open
		(forward-line -1))
	      (delete-region (point) end))))
      
      )))

;; 2001/03/01 - Toni - For debugging purposes.
;;;(defun set-definition-clear ()
;;;  (let* ((mode-fillers             (mode-fillers))
;;;	 (filler                   (nth 1 mode-fillers))
;;;	 (multi-line-comment-open  (nth 2 mode-fillers))
;;;	 (multi-line-comment-close (nth 3 mode-fillers)))
;;;    (let (start end)
;;;      
;;;      ;; Deletes the definition first.
;;;      ;;(save-excursion
;;;
;;;	;; Code for debugging.
;;;	(setq start (point))
;;;	 
;;;	(set-mark start)
;;;	
;;;	(end-of-definition)
;;;	
;;;	
;;;	;;(setq end (point))
;;;	;;(beginning-of-definition t)
;;;	;;(setq start (point))
;;;	;;(delete-region start end)
;;;	;;)
;;;      
;;;      ;; Now deletes the corresponding header, if there is one.
;;;;;      (save-excursion
;;;;;	(setq end (point))
;;;;;	(previous-line 1)
;;;;;	(when multi-line-comment-close
;;;;;	  (previous-line 1))
;;;;;	(if (looking-at filler)
;;;;;	    (progn
;;;;;	      (previous-line 1)
;;;;;	      (while (not (looking-at filler))
;;;;;		(previous-line 1))
;;;;;	      (when multi-line-comment-open
;;;;;		(previous-line 1))
;;;;;	      (delete-region (point) end))))
;;;      
;;;      )))

;;;-----------------------------------------------------------------------------
;;;UPDATE-DEFINITION
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{update-p} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	\unable-to-document
;;;		01/02/08	Toni	
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	01/03/02	Toni		Added support for HTML-MODE, PHP-MODE
;;;					and VB-MODE by generalising.
;;;	09/09/23	P. Madeira	`kill-line' -> `sc-kill-line'.
;;;					`next-line' -> `forward-line' (EMACS 23).
;;;					`previous-line' -> `forward-line' (EMACS 23).
;;;-----------------------------------------------------------------------------
(defun update-definition (update-p)
  (let* ((mode-fillers             (mode-fillers))
	 (comment                  (nth 0 mode-fillers))
	 (filler                   (nth 1 mode-fillers))
	 (multi-line-comment-close (nth 3 mode-fillers)))
    (beginning-of-definition)
    (forward-line -1)
    (when multi-line-comment-close
      (forward-line -1))
    (if (looking-at filler)
	(progn
	  (forward-line -1)
	  (when (search-backward filler nil t)
	    (forward-line 1)
	    (beginning-of-line)
	    (sc-kill-line)
	    (insert comment)
	    (when update-p
	      (forward-line 2)
	      (beginning-of-line)
	      (while (not (or (format "%s History" comment) (format "%sHistory" comment)))
		(sc-kill-line)
		(sc-kill-line))
	      (insert comment 9 10))
	    (when (search-forward (format "%s	Date		Author		Description" comment) nil t)
	      (forward-line 1)
	      (beginning-of-line)
	      (while (not (looking-at filler))
		(sc-kill-line)
		(sc-kill-line))))))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-CHANGED
;;;Description
;;;	Set current definition as changed
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Passou a informar o big-brother...
;;;	99/08/31	J. P. Varandas	Utiliza a variavel '*history-label*'
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-definition-changed ()
  (let ((definition (save-excursion (get-definition-id))))
    (if definition
	(progn
	  (save-excursion (add-definition-mod-line definition *history-label*))
	  (save-excursion (set-definition-modified definition "Changed definitions"))
	  (set-mod-file-changed-if)
	  (inform-big-brother "M" (current-modif-request) definition)))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-DOC-UPDATE
;;;Description
;;;	Set current definition as with documentation updated
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	00/01/31	J. P. Varandas	Created
;;;	02/01/31	Pedro Matos	Added interactive.
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-definition-doc-update ()
  (interactive)
  (let ((definition (save-excursion (get-definition-id))))
    (if definition
	(progn
	  (save-excursion (add-definition-mod-line definition "Doc updated"))
	  (save-excursion (set-definition-modified definition "Documented definitions"))
	  (set-mod-file-changed-if)
	  (inform-big-brother "M" (current-modif-request) definition)))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-ADDED
;;;Description
;;;	Set current definition as added
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Passou a informar o big-brother...
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-definition-added ()
  (let ((definition (save-excursion (get-definition-id))))
    (if definition
	(progn
	  (save-excursion (add-definition-mod-line definition "Created"))
	  (save-excursion (set-definition-modified definition "Added definitions"))
	  (set-mod-file-changed-if)
	  (inform-big-brother "C" (current-modif-request) definition)))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-NEW
;;;Description
;;;	Set current definition as added but not registring it in the file header
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-definition-new ()
  (let ((definition (save-excursion (get-definition-id))))
    (when definition
      (save-excursion (add-definition-mod-line definition "Created"))
      (set-mod-file-changed-if)
      (inform-big-brother "C" (current-modif-request) definition))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-COPY
;;;Description
;;;	Set current definition as added as a copy of an old definition
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-definition-copy ()
  (let ((definition (save-excursion (get-definition-id))))
    (when definition
      (save-excursion (update-definition nil))
      (save-excursion (add-definition-mod-line definition "Created"))
      (save-excursion (set-definition-modified definition "Added definitions"))
      (set-mod-file-changed-if)
      (inform-big-brother "C" (current-modif-request) definition))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-DELETED
;;;Description
;;;	Set current definition as deleted
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Passou a informar o big-brother...
;;;	97/07/25	A. Frazao	
;;;	99/08/31	J. P. Varandas	Chama a função 'set-definition-clear'
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-definition-deleted ()
  (let ((definition (save-excursion (get-definition-id))))
    (when definition
      (save-excursion (set-definition-clear))
      (save-excursion (set-definition-modified definition "Deleted definitions"))
      (set-mod-file-changed-if)
      (inform-big-brother "D" (current-modif-request) definition))))

;; 2001/03/01 - Toni - For debugging purposes (debugging of 'set-definition-clear').
;;;(defun set-definition-deleted ()
;;;  (let ((definition (save-excursion (get-definition-id))))
;;;    (when definition
;;;      ;; Toni - Comentei para poder testar 'set-definition-clear'.
;;;      ;;(save-excursion 
;;;      (set-definition-clear)
;;;      ;;)
;;;      ;; Toni - Comentei para poder testar 'set-definition-clear'.
;;;      ;;(save-excursion (set-definition-modified definition "Deleted definitions"))
;;;      ;;(set-mod-file-changed-if)
;;;      (inform-big-brother "D" (current-crews-mod) definition)
;;;      )))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-MOVED
;;;Description
;;;	Set current definition as moved to another file 
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-definition-moved ()
  (let ((definition (save-excursion (get-definition-id))))
    (when definition
      (save-excursion (set-definition-clear))
      (save-excursion (set-definition-modified definition "Moved definitions"))
      (set-mod-file-changed-if)
      (inform-big-brother "D" (current-modif-request) definition))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-UPDATE
;;;Description
;;;	Set current definition as updated to a new definition
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-definition-update ()
  (let ((definition (save-excursion (get-definition-id))))
    (when definition
      (save-excursion (update-definition nil))
      (save-excursion (set-definition-modified (format "%s -> " definition) "Updated definitions"))
      (set-mod-file-changed-if)
      (inform-big-brother "D" (current-modif-request) definition))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-COMMENT
;;;Description
;;;	Set current definition as comment
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-definition-comment ()
  (let ((definition (save-excursion (get-definition-id))))
    (when definition
      (save-excursion (add-definition-mod-line definition "Commented"))
      (save-excursion (set-definition-modified definition "Comment definitions"))
      (sc-insert-commas2)
      (set-mod-file-changed-if)
      (inform-big-brother "D" (current-modif-request) definition))))


;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-HEADER
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun set-definition-header ()
  (let ((definition (save-excursion (get-definition-id))))
    (when definition
      (save-excursion (add-definition-header definition *history-label*))
      (set-mod-file-changed-if))))

;;;-----------------------------------------------------------------------------
;;;GET-PATCH-FILE
;;;Description
;;;	Creates a patch name and assigns to the current modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}.
;;;
;;;		\arg{name} is a \emph{string}, the name of the file to use
;;;		(including extension) or \emph{nil}.
;;;		
;;;	\return-types
;;;		A \emph{string}, the name of the patch.
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/09/01	J. P. Varandas	Created
;;;	04/04/23	A. Frazao	Added argument name
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun get-patch-file (mod &optional name)
  (let ((patch (modif-request-patch mod)))
    (unless patch
      (setf patch (make-mod-file-name (or name "patch.lisp")))
      (setf (modif-request-patch mod) patch)
      (let* ((file (format "%s/%s" (modif-request-dir mod) patch))
	     (buf (get-file-buffer file)))
	(when buf
	  (kill-buffer buf))
	(when (file-exists-p file)
	  (delete-file file)))
      (save-modifications))
    patch))


;;;	09/09/23	P. Madeira	`next-line' -> `forward-line' (EMACS 23)
(defun special-beginning-of-buffer ()
  (let* ((mode-fillers            (mode-fillers))
	 (comment                 (nth 4 mode-fillers))
	 (filler                  (nth 5 mode-fillers))
	 (multi-line-comment-open (nth 6 mode-fillers))
	 (date (current-mod-date-string))
	 (user (current-mod-author))
	 (found nil)
	 (failed nil))
    (beginning-of-buffer)
    (when multi-line-comment-open
      (forward-line 1))
    
    ;; Look for the history
    (while (and (not found) (not failed))
      (if (not (looking-at filler))
	  (if (looking-at comment)
	      (progn
		(forward-char (length comment))
		(while (and (or (= (char-after (point)) 32)
				(= (char-after (point)) 9))
			    (not (= (char-after (point)) 10)))
		  (forward-char 1))
		(if (looking-at "History")
		    (setq found t)))
	      (setq failed t)))
      (line-feed))
    
    ;; Look for the date
    (setq found nil)
    (beginning-of-line)
    (while (and (not found) (not failed))
      (cond ((looking-at filler)
	     (forward-line 1)
	     (setq found t))
	    ((looking-at comment)
	     (line-feed))
	    (t (setq failed t))))
    
    (if failed
	(beginning-of-buffer)
	(line-feed))
    ))

;;;	09/09/23	P. Madeira	Declared `not-found' variable
(defun special-search-forward (string)
  (let ((found nil)
	(not-found t)
	(end nil))
    (while (and (not found) not-found)
      (if (search-forward string nil t)
	  (let ((comments nil))
	    (setf end (point))
	    (save-excursion
	      (progn
		(beginning-of-line)
		(while (and (not comments)
			    (not (= (point) end)))
		  (if (eq (char-after (point)) 59)
		      (setf comments t)
		      (forward-char 1)))))
	    (when (not comments)
	      (setf found t)))
	  (setf not-found nil)))
    found))

(defun special-search-backward (string)
  (let ((found nil)
	(not-found t)
	(end nil))
    (while (and (not found) not-found)
      (if (search-backward string nil t)
	  (let ((comments nil))
	    (setf end (point))
	    (save-excursion
	      (progn
		(beginning-of-line)
		(while (and (not comments)
			    (not (= (point) end)))
		  (if (eq (char-after (point)) 59)
		      (setf comments t)
		      (forward-char 1)))))
	    (when (not comments)
	      (setf found t)))
	  (setf not-found nil)))
    found))



;;;	09/09/23	P. Madeira	`next-line' -> `forward-line' (EMACS 23).
;;;					`previous-line' -> `forward-line' (EMACS 23)
(defun position-definition-on-patch (file package &optional at-start)
  (let ((found nil)
	(failed nil))
    (my-end-of-buffer)
    (if (search-backward file nil t)
	(progn
	  (beginning-of-line)
	  (forward-line 4)
	  (end-of-definition)
	  (insert 10 10))
	(if (special-search-backward package)
	    (progn
	      (beginning-of-line)
	      (forward-line -2)
	      (if (looking-at (nth 1 (mode-fillers)))
		  (forward-line 3)
		  (forward-line 2))
	      (end-of-definition)
	      (insert 10 10))
	    (if at-start
		(special-beginning-of-buffer)
		(my-end-of-buffer))))))



;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-TO-PATCH
;;;Description
;;;	Copies the selected definition into the modification patch file 
;;;	positioning it in the proper place.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Created
;;;	99/02/03	A. Frazao	Check if there is not header before the
;;;					definition
;;;	99/09/01	J. P. Varandas	Use of function 'get-patch-file'
;;;	05/03/21	J. P. Varandas	Use of function 'position-definition-on-patch' to position the new definition on the patch
;;;					Registers the new entry in patch header.
;;;	05/04/06	A. Frazao	Add file header if it does not exists
;;;	05/10/13	J. P. Varandas	Position the new definition next to existing one if it exists.
;;;	05/10/19	J. P. Varandas	Do not use the 'get-definition-id' to position the defintion in the patch
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;	09/09/23	P. Madeira	`previous-line' -> `forward-line' (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun set-definition-to-patch ()
  (let* ((c-mode (eq (file-mode) C-MODE))
	 (mode-filler (mode-fillers))
	 (filler (cadr mode-filler))
	 (definition (save-excursion (get-definition-id)))
	 (mod (current-modif-request))
	 (filename (buffer-file-name (current-buffer))))
    (if (and definition (string-match (format "^%s/" (modif-request-src-dir mod)) filename))
	(let ((source-file (substring filename (match-end 0)))
	      (patch (get-patch-file mod))
	      start
	      end
	      package
	      def-str
	      patch-file
	      str1)
	  (save-excursion
	    (beginning-of-definition)
	    (let ((start (point)))
	      (while (not (= (char-after (point)) 10))
		(forward-char 1))
	      (setq str1 (buffer-substring start (- (point) 1)))))
	  (save-excursion
	    (end-of-defun)
	    (setq end (point))
	    (beginning-of-defun)
	    (forward-line -1)
	    (if (looking-at filler)
		(progn
		  (forward-line -1)
		  (while (not (looking-at filler))
		    (forward-line -1))
		  (setq start (point)))
		(progn
		  (forward-line 1)
		  (setq start (point))))
	    (setf def-str (buffer-substring start end))
	    (while (not (looking-at "(in-package"))
	      (forward-line -1))
	    (setf start (point))
	    (end-of-defun)
	    (setf package (buffer-substring start (point))))
	  (let* ((file (format "%s/%s" (modif-request-dir mod) patch))
		 (file-exists-p (file-exists-p file))
		 (old-buf (current-buffer))
		 (buf (or (get-file-buffer file)
			  (find-file-noselect file))))
	    (set-buffer buf)
	    (unless file-exists-p
	      (add-mod-file-header))
	    (beginning-of-buffer)
	    (if (search-forward str1 nil t)
		(progn
		  (end-of-defun)
		  (insert 10 10))
		(position-definition-on-patch source-file package))
	    
	    (save-excursion
	      (insert filler 10)
	      (insert (car mode-filler) " File: " source-file 10)
	      (insert filler 10 10)
	      (insert package 10)
	      (insert def-str 10))
	    (save-excursion (set-definition-modified definition (format "Re-definitions of file: %s" source-file)))
	    (save-buffer)
	    (set-buffer old-buf)))
	(beep-message "Cannot find a definition or match the source file"))))

;;;-----------------------------------------------------------------------------
;;;SET-EXPORT-TO-PATCH
;;;Description
;;;	Copies a symbol exportation from a package into the modification patch file 
;;;	positioning it in the proper place.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/09/01	J. P. Varandas	Created
;;;	99/10/19	J. P. Varandas	Junta aspas ao nome do package
;;;	02/08/28	Dario		Removed excess double quotes when
;;;					the name of the package already contains
;;;					double quotes.
;;;	04/02/12	Duarte Peralta	Wraps exportation inside an eval-when (compile load eval)
;;;	05/03/21	J. P. Varandas	Use of function 'position-definition-on-patch' to position the new definition on the patch
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-export-to-patch ()
  (let* ((mode-filler (mode-fillers))
	 (filler (cadr mode-filler))
	 (definition (save-excursion (get-definition-id)))
	 (symbol (save-excursion (get-symbol-id)))
	 (mod (current-modif-request))
	 (filename (buffer-file-name (current-buffer))))
    (if (and definition (string-match (format "^%s/" (modif-request-src-dir mod)) filename))
	(let ((source-file (substring filename (match-end 0)))
	      (patch (get-patch-file mod))
	      package
	      patch-file)
	  (save-excursion
	    (string-match "^Package " definition)
	    (setf package (substring definition (match-end 0))))
	  (let* ((file (format "%s/%s" (modif-request-dir mod) patch))
		 (old-buf (current-buffer))
		 (buf (or (get-file-buffer file)
			  (find-file-noselect file))))
	    (set-buffer buf)
	    (let ((da-package (if (char-equal (aref package 0) ?:)
				  (substring package 1)
				  (substring package 1 (1- (length package))))))
	      (position-definition-on-patch source-file (format "(in-package \"%s\")" da-package) t)
	      (insert filler 10)
	      (insert (car mode-filler) " File: " source-file 10)
	      (insert filler 10 10)
	      (insert "(in-package " 34 
		      da-package
		      34 ")" 10 10)
	      (insert "(eval-when (compile load eval)" 10 
		      "  (export (intern " symbol 32 34 
		      da-package
		      34 ") " 34 
		      da-package
		      34 "))" 10 10))
	    (save-buffer)
	    (set-buffer old-buf)))
	(beep-message "Cannot find a definition or match the source file"))))

;;;-----------------------------------------------------------------------------
;;;SET-KEYWORD-TO-PATCH
;;;Description
;;;	Copies a translation keyword into the modification patch file 
;;;	positioning it in the proper place.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/09/01	J. P. Varandas	Created
;;;	99/10/19	J. P. Varandas	Escreve as traduções para todas as linguas
;;;	05/03/21	J. P. Varandas	Adds the new translation keyword inside the patch just after all other translation keyword if any.
;;;					If there are no translation keywords adds it just after the patch header
;;;	05/10/13	J. P. Varandas	Check if there are keywords for all languages.
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;	09/09/23	P. Madeira	`previous-line' -> `forward-line' (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun set-keyword-to-patch ()
  (let* ((mode-filler (mode-fillers))
	 (filler (cadr mode-filler))
	 (keywords (save-excursion (get-keyword-ids)))
	 (languages (get-translation-languages))
	 (mod (current-modif-request))
	 (filename (buffer-file-name (current-buffer))))
    (if (and keywords (string-match (format "^%s/" (modif-request-src-dir mod)) filename))
	(let ((source-file (substring filename (match-end 0)))
	      (patch (get-patch-file mod))
	      patch-file)
	  (let* ((file (format "%s/%s" (modif-request-dir mod) patch))
		 (old-buf (current-buffer))
		 (buf (or (get-file-buffer file)
			  (find-file-noselect file))))
	    (set-buffer buf)
	    (my-end-of-buffer)
	    (if (special-search-backward "add.keyword")
		(progn
		  (beginning-of-line)
		  (while (not (= (char-after (point)) 40))
		    (forward-line -1))
		  (end-of-definition)
		  (line-feed))
		(special-beginning-of-buffer))
	    (insert filler 10)
	    (insert (car mode-filler) " File: " source-file 10)
	    (insert filler 10 10)
	    (insert "(in-package :traducao)" 10 10)
	    (insert "(eval-when (compile load eval)" 10 "  (add.keyword " (car keywords))
	    (when languages 
	      (insert 10 9 9 ":translations '(")
	      (dolist (language languages)
		(setf keywords (cdr keywords))
		(when (car keywords)
		  (insert (format "%s" language) 32 (car keywords) 32)))
	      (insert ")"))
	    (insert "))" 10 10)
	    (save-buffer)
	    (set-buffer old-buf)))
	(beep-message "Cannot find a definition or match the source file"))))

;;;-----------------------------------------------------------------------------
;;;SET-DEFINITION-UNDO
;;;Description
;;;	Removes from the definition header of the definition where \elem{point}
;;;	lies on the signature of the user for the current change. Likewise,
;;;	removes the definition identifier from the corresponding signature but
;;;	in the file header.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	01/03/02	Toni		Added support for HTML-MODE, PHP-MODE
;;;					and VB-MODE by generalising.
;;;					Doc updated.
;;;					Replaced one occurrence of 'looking-at'
;;;					by 'looking-literally-at' in order to
;;;					have REGEXPs special characters
;;;					interpreted as normal characters. This
;;;					way the definition identifier can
;;;					include those characters.
;;;	09/09/23	P. Madeira	`kill-line' -> `sc-kill-line'.
;;;					`previous-line' -> `forward-line' (EMACS 23).
;;;-----------------------------------------------------------------------------
(defun set-definition-undo ()
  (let* ((mode-fillers             (mode-fillers))
	 (comment                  (nth 0 mode-fillers))
	 (filler                   (nth 1 mode-fillers))
	 (multi-line-comment-close (nth 3 mode-fillers))
	 (definition               (save-excursion (get-definition-id)))
	 (found                    nil))
    
    ;; Undo stuff at the definition header.
    (save-excursion 
      (beginning-of-definition)
      (forward-line -1)
      (when multi-line-comment-close
	(forward-line -1))
      (if (looking-at filler)
	  (let ((look (format "%s	%s" comment (current-mod-date-string)))
		(found nil))
	    (when (search-backward (format "%s	Date		Author		Description" comment) nil t)
	      (line-feed)
	      (while (not (looking-at filler))
		(if (looking-at look)
		    (progn
		      (beginning-of-line)
		      (sc-kill-line)
		      (sc-kill-line)
		      (while (not (looking-at filler))
			(sc-kill-line)
			(sc-kill-line)))
		    (line-feed))))
	    
	    ;; Undo stuff at the file header.
	    (let ((comment (nth 4 mode-fillers))
		  (filler  (nth 5 mode-fillers)))
	      (beginning-of-buffer)
	      (setq look (format "%s					  %s" comment definition))
	      (when (search-forward (format "%s	%s	%s" comment (current-mod-date-string) (current-mod-author)) nil t)
		(line-feed)
		(while (and (or (not found) (not (looking-at filler))) (< (point) (point-max)))
		  (if (looking-literally-at look)
		      (progn
			(beginning-of-line)
			(sc-kill-line)
			(sc-kill-line)
			(setq found t))
		      (line-feed)))
		(when (looking-at filler)
		  (forward-line -1)
		  (when (looking-at (format "%s	%s	%s" comment (current-mod-date-string) (current-mod-author)))
		    (sc-kill-line)
		    (sc-kill-line)))))
	    ))
      )))

;;;-----------------------------------------------------------------------------
;;;SET-EXPORT-ADDED
;;;Description
;;;	Set the current symbol as exported from package
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-export-added ()
  (let ((definition (save-excursion (get-definition-id)))
	(symbol (save-excursion (get-symbol-id))))
    (when (and definition symbol)
      (save-excursion (set-system-mod-line definition "Add export symbols" symbol))
      (save-excursion (set-definition-modified definition "Changed definitions"))
      (set-mod-file-changed-if)
      (inform-big-brother "C" (current-modif-request) definition))))

;;;-----------------------------------------------------------------------------
;;;SET-EXPORT-DELETED
;;;Description
;;;	Set the current exported symbol as deleted
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-export-deleted ()
  (let ((definition (save-excursion (get-definition-id)))
	(symbol (save-excursion (remove-symbol-id))))
    (when (and definition symbol)
      (save-excursion (set-system-mod-line definition "Remove export symbols" symbol))
      (save-excursion (set-definition-modified definition "Changed definitions"))
      (set-mod-file-changed-if)
      (inform-big-brother "C" (current-modif-request) definition))))

;;;-----------------------------------------------------------------------------
;;;SET-SOURCE-ADDED
;;;Description
;;;	Set the current system source path as added
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/09/01	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-source-added ()
  (let ((source (save-excursion (get-source-id))))
    (when source
      (save-excursion (set-system-mod-line nil "Add system sources" source))
      (save-excursion (set-definition-modified source "Add system sources"))
      (set-mod-file-changed-if)
      (inform-big-brother "C" (current-modif-request) source))))

;;;-----------------------------------------------------------------------------
;;;SET-SOURCE-DELETED
;;;Description
;;;	Set the current system source path as deleted
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/09/01	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-source-deleted ()
  (let ((source (save-excursion (remove-source-id))))
    (when source
      (save-excursion (set-system-mod-line nil "Removed system sources" source))
      (save-excursion (set-definition-modified source "Removed system sources"))
      (set-mod-file-changed-if)
      (inform-big-brother "C" (current-modif-request) source))))

;;;-----------------------------------------------------------------------------
;;;SET-KEYWORD-CHANGE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun set-keyword-change ()
  (let ((definition (save-excursion (get-keyword-id))))
    (when definition
      (save-excursion (set-definition-modified definition "Changed keywords"))
      (set-mod-file-changed-if))))

;;;-----------------------------------------------------------------------------
;;;SET-KEYWORD-ADDED
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun set-keyword-added ()
  (let ((definition (save-excursion (get-keyword-id))))
    (when definition
      (save-excursion (set-definition-modified definition "Added keywords"))
      (set-mod-file-changed-if))))

;;;-----------------------------------------------------------------------------
;;;SET-KEYWORD-DELETED
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun set-keyword-deleted ()
  (let ((symbol (save-excursion (remove-keyword-id))))
    (when symbol
      (save-excursion (set-definition-modified symbol "Removed keywords"))
      (set-mod-file-changed-if))))

;;;-----------------------------------------------------------------------------
;;;*HISTORY-LABEL*
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *history-label* "")

;;;-----------------------------------------------------------------------------
;;;SET-HISTORY-LABEL
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun set-history-label ()
  (setq *history-label* (read-string "History Label: ")))

;;;-----------------------------------------------------------------------------
;;;ADD-HEADER-CHANGE-ENTRY
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	
;;;	99/01/29	A. Frazao	When looking for the date or history
;;;					skip the comment characters
;;;	99/08/31	J. P. Varandas	Chama a função 'add-definition-entry'
;;;-----------------------------------------------------------------------------
(defun add-header-change-entry ()
  (add-definition-entry nil nil)
  (set-mod-file-changed-if))

;;;-----------------------------------------------------------------------------
;;;ADD-MOD-FILE-HEADER
;;;Description
;;;	Inserts a header in the beginning of the current file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	
;;;	99/08/31	J. P. Varandas	Inserts the current year on the header
;;;	01/03/02	Toni		Added support for HTML-MODE, PHP-MODE
;;;					and VB-MODE by generalising.
;;;					Doc updated
;;;	01/10/01	Dario		Actualized file header with the new
;;;					Siscog adress.
;;;	08/07/29	A. Frazao	Calls beginning-of-file-header
;;;	09/09/23	P. Madeira	Replaced cardinal character with its code (EMACS 23)
;;;	10/01/05	A. Frazao	Changed Lda to SA
;;;-----------------------------------------------------------------------------
(defun add-mod-file-header ()
  (let* ((mode-fillers             (mode-fillers))
	 (comment                  (nth 4 mode-fillers))
	 (filler                   (nth 5 mode-fillers))
	 (multi-line-comment-open  (nth 6 mode-fillers))
	 (multi-line-comment-close (nth 7 mode-fillers)))
    (save-excursion
      (beginning-of-file-header)
      (when multi-line-comment-open
	(insert multi-line-comment-open 10))
      (insert filler 10)
      (insert comment 10)
      (insert comment "           Copyright (C) " (format "%s" (current-year)) ", SISCOG - Sistemas Cognitivos, SA" 10)
      (insert comment "                           All rights reserved" 10)
      (insert comment 10)
      (insert filler 10)
      (insert comment 10)
      (insert comment "                         RESTRICTED RIGHTS LEGEND" 10)
      (insert comment 10)
      (insert filler 10)
      (insert comment 10)
      (insert comment "     Use, duplication or disclosure is subject to authorization by" 10)
      (insert comment 10)
      (insert comment "                 SISCOG - Sistemas Cognitivos, SA" 10)
      (insert comment "                      Campo Grande 378, 3" 186 10)
      (insert comment "                        1700-097 LISBOA" 10)
      (insert comment "                           PORTUGAL" 10)
      (insert comment 10)
      (insert filler 10)
      (insert comment " Description" 10)
      (insert comment 9 10)
      (insert comment " History" 10)
      (insert comment 9 "Date" 9 9 "Author" 9 9 "Description" 10)
      (insert filler 10)
      (when multi-line-comment-close
	(insert multi-line-comment-close 10))
      (insert 10)
      )))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; DISPLAYING A MODIFICATION
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;DISPLAY-MODIF-REQUEST
;;;Description
;;;	Displays in a buffer the contents of the given modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Added patch
;;;	05/03/21	J. P. Varandas	Added reference field
;;;	05/10/13	J. P. Varandas	Added versions field
;;;	09/02/10	J. P. Varandas	Changed function name: display-crews-mod -> display-modif-request
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun display-modif-request (mod)
  (let ((files (modif-request-files mod)))
    (insert "--------------------------------------------------------------------------------" 10)
    (insert "Modification request" 10)
    (insert "Name:               " (modif-request-name mod) 10)
    (insert "Author:             " (modif-request-author mod) 10)
    (insert "Original directory: " (modif-request-org-dir mod) 10)
    (insert "Source directory:   " (modif-request-src-dir mod) 10)
    (insert "Changes directory:  " (modif-request-dir mod) 10)
    (insert "Modification date:  " (modif-request-date mod) 10)
    (when (modif-request-reference mod)
      (insert "Reference:          " (or (modif-request-reference mod) "") 10))
    
    (when (modif-request-versions mod)
      (insert "Versions:           ")
      (dolist (version (modif-request-versions mod))
	(when version
	  (insert version 32)))
      (insert 10))
    (if (modif-request-patch mod)
	(insert "Patch:              " (modif-request-patch mod) 10))
    (insert 10)
    (if files
	(progn
	  (insert "Current files" 10)
	  (dolist (file files)
	    (let ((status (file-status file)))
	      (insert (format "  %s" (print-mod-file file)) 10)
	      (if (listp status)
		  (progn
		    (insert "    * ERROR: " (car status) " *" 10)
		    (dolist (line (cdr status))
		      (insert "             " line 10)))
		  (cond ((= status ERR_DEL_FILE)    (insert "    * ERROR: Original file does not exist *" 10))
			((= status ERR_NEW_FILE)    (insert "    * ERROR: Original file exists *" 10))
			((= status ERR_NO_SRC_FILE) (insert "    * ERROR: Source file does not exist *" 10))
			((= status ERR_NO_MOD_FILE) (insert "    * ERROR: Modification file does not exist *" 10))
			((= status ERR_NO_ORG_FILE) (insert "    * ERROR: Original file does not exist *" 10))
			((= status ERR_SIGNATURE)   (insert "    * ERROR: Signatures are not correct *" 10)))))))
	(insert "No modification files" 10))
    (insert "--------------------------------------------------------------------------------" 10)))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; SELECTION BY MEANS OF A MENU
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SELECT-MOD-FILE
;;;Description
;;;	Presents a menu for user to choose a file from the modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;	\return-types
;;;		A mod-file
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	Splits the items when they are too many
;;;	98/11/04	A. Frazao	Uses new menu implementation
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun select-mod-file (mod)
  (if (modif-request-files mod)
      (let ((items nil))
	(dolist (file (modif-request-files mod))
	  (push (sc-make-menu-item (print-mod-file file) file) items))
	(setq items (nreverse items))
	(sc-popup-menu *current-x-arg* "Select a file" items nil 40))
      (progn
	(beep-message "No files to select")
	nil)))


;;;-----------------------------------------------------------------------------
;;;SELECT-ALL-MOD-FILE
;;;Description
;;;	Selects a file from all modifications.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A mod-file or nil
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun select-all-mod-file ()
  (let ((items nil))
    (dolist (mod (all-modif-requests))
      (dolist (file (modif-request-files mod))
	(let ((str (format "%s - %s" (modif-request-name mod) (print-mod-file file))))
	  (push (sc-make-menu-item str (list file mod)) items))))
    (setq items (nreverse items))
    (if items
	(sc-popup-menu *current-x-arg* "Select a file" items nil 40)
	(progn
	  (beep-message "No files to select")
	  nil))))


;;;-----------------------------------------------------------------------------
;;;SELECT-MODIF-REQUEST
;;;Description
;;;	Allows to select a \elem{modif-request}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \elem{modif-request}
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Uses new menu implementation
;;;	05/10/13	J. P. Varandas	Display those modifications that have a mail-file in a different way
;;;	06/05/29	A. Frazao	Display those modifications that have been sent in a different way
;;;	08/02/22	Tom Weissmann	Added title argument (POA 12701.0).
;;;	09/02/10	J. P. Varandas	Changed function name: select-crews-mod -> select-modif-request
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun select-modif-request (&optional title)
  (sc-popup-menu
   *current-x-arg*
   (or title "Select a modification")
   (mapcar '(lambda (mod)
	     (sc-make-menu-item (format "%s%s%s"
				 (modif-request-name mod)
				 (if (modif-request-mail-file mod) " (*)" "") 
				 (if (modif-request-mail-sent mod) " SENT" ""))
	      mod))
	   *all-modif-requests*)))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; CHANGING A MODIFICATION'S BASIC DATA
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

(defun read-default-string (prompt default error)
  (let ((string nil))
    (while prompt
      (setq string (cond ((and default error)
			  (read-string (format "[%s] %s [Default %s]: " error prompt default)))
			 (default (read-string (format "%s [Default %s]: " prompt default)))
			 (error (read-string (format "[%s] %s: " error prompt)))
			 (t (read-string (format "%s: " prompt)))))
      (if (string= string "")
	  (if default
	      (progn
		(setq string default)
		(setq prompt nil))
	      (setq error "Cannot be empty"))
	  (setq prompt nil)))
    string))

;;;-----------------------------------------------------------------------------
;;;READ-MOD-NAME
;;;Description
;;;	Reads from user the bane of the new modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{current-mod} is a \elem{modif-request}
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun read-mod-name (&optional current-mod)
  (let ((default (if current-mod
		     (modif-request-name current-mod)))
	(name nil)
	(end nil)
	(error nil))
    (while (not end)
      (setq name (read-default-string "Enter name" default error))
      (unless (dolist (mod *all-modif-requests*)
		(when (and (not (eq mod current-mod))
			   (string= name (modif-request-name mod)))
		  (setq error (format "Duplicated name %s" name))
		  (return t)))
	(setq end t)))
    name))

(defun read-mod-dir (str &optional default)
  (let ((name nil)
	(end nil)
	(error nil))
    (while (not end)
      (setq name (read-default-string (format "Enter %s directory" str) default error))
      (if (file-directory-p name)
	  (setq end t)
	  (setq error (format "%s is not a directory" name))))
    name))

;;;-----------------------------------------------------------------------------
;;;CHANGE-MODIF-REQUEST-NAME
;;;Description
;;;	Allows to change the current modification name
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed function name: change-crews-mod-name -> change-modif-request-name
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun change-modif-request-name ()
  (let ((mod (current-modif-request)))
    (setf (modif-request-name mod) (read-mod-name mod))
    (save-modifications)))

;;;-----------------------------------------------------------------------------
;;;CHANGE-MODIF-REQUEST-AUTHOR
;;;Description
;;;	Allows to change the current modification author
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed function name: change-crews-mod-author -> change-modif-request-author
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun change-modif-request-author ()
  (let ((mod (current-modif-request)))
    (setf (modif-request-author mod) (read-default-string "Enter author" (modif-request-author mod) nil))
    (save-modifications)))

;;;-----------------------------------------------------------------------------
;;;CHANGE-MODIF-REQUEST-ORG-DIR
;;;Description
;;;	Allows to change the current modification original directory
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed function name: change-crews-mod-org-dir -> change-modif-request-org-dir
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun change-modif-request-org-dir ()
  (let ((mod (current-modif-request)))
    (setf (modif-request-org-dir mod) (read-mod-dir "original" (modif-request-org-dir mod)))
    (save-modifications)))

;;;-----------------------------------------------------------------------------
;;;CHANGE-MODIF-REQUEST-SRC-DIR
;;;Description
;;;	Allows to change the current modification source directory
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed function name: change-crews-mod-src-dir -> change-modif-request-src-dir
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun change-modif-request-src-dir ()
  (let ((mod (current-modif-request)))
    (setf (modif-request-src-dir mod) (read-mod-dir "source" (modif-request-src-dir mod)))
    (save-modifications)))

;;;-----------------------------------------------------------------------------
;;;CHANGE-MODIF-REQUEST-DIR
;;;Description
;;;	Allows to change the current modification directory
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed function name: change-crews-mod-dir -> change-modif-request-dir
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun change-modif-request-dir ()
  (let ((mod (current-modif-request)))
    (setf (modif-request-dir mod) (read-mod-dir "modification" (modif-request-dir mod)))
    (save-modifications)))

;;;-----------------------------------------------------------------------------
;;;CHANGE-MODIF-REQUEST-DATE
;;;Description
;;;	Allows to change the current modification date
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	change-crews-mod-date -> change-modif-request-date
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun change-modif-request-date ()
  (let ((mod (current-modif-request)))
    (setf (modif-request-date mod) (read-default-string "Enter date" (modif-request-date mod) nil))
    (save-modifications)))

;;;-----------------------------------------------------------------------------
;;;CHANGE-MODIF-REQUEST-REFERENCE
;;;Description
;;;	Allow the user to change the modification reference.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	05/03/21	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed function name: change-crews-mod-reference -> change-modif-request-reference
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun change-modif-request-reference ()
  (let ((mod (current-modif-request)))
    (let ((new-value (read-string "Enter reference: " (modif-request-reference mod))))
      (when (string= new-value "")
	(setf new-value nil))
      (setf (modif-request-reference mod) new-value))
    (save-modifications)))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; OPERATIONS OVER THE CURRENT FILE
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;CHECK-ALL-MODS-SOURCE-FILE
;;;Description
;;;	Check the correctness of all files of the modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{source-file} is a mod file
;;;		
;;;	\return-types
;;;		A \elem{boolean}
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun check-all-mods-source-file (source-file)
  (let ((current-mod (current-modif-request)))
    (dolist (mod (all-modif-requests) t)
      (when (member* source-file (modif-request-files mod) :key 'file-src :test 'string-equal)
	(if (eq mod current-mod)
	    (progn
	      (beep-message (format "File %s is already in modification %s" source-file (modif-request-name mod)))
	      (return nil))
	    (return (x-beep-confirm source-file (format "File already in modification %s (not current). Proceed?" (modif-request-name mod)))))))))

;;;-----------------------------------------------------------------------------
;;;SET-MOD-FILE-CHANGED-IF
;;;Description
;;;	Mark a file as changed when marking a definition inside that file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/09/01	J. P. Varandas	If the file does not exists on the original 
;;;					directory it marks it as NEW (asking first)
;;;	05/03/21	J. P. Varandas	Check the correctness of the signatures
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-mod-file-changed-if ()
  (let ((mod (current-modif-request))
	(filename (buffer-file-name (current-buffer))))
    (if mod
	(if (string-match (format "^%s/" (modif-request-src-dir mod)) filename)
	    (let ((source-file (substring filename (match-end 0))))
	      (when (and (not (member* source-file (modif-request-files mod) :key 'file-src :test 'string-equal))
			 (check-all-mods-source-file source-file))
		(let ((mod-file (make-mod-file-name filename)))
		  (if (or (file-exists-p (format "%s/%s" (modif-request-org-dir mod) source-file))
			  (x-beep-confirm source-file (format "This file does not exist in the original dir %s" 
							      source-file (modif-request-org-dir mod)) "Do you want to mark it as NEW?"))
		      (let* ((file (make-mod-file source-file mod-file 0 0 0))
			     (res (save-excursion (check-src-file mod file))))
			(when (consp res)
			  (beep-message (car res)))
			(setf (modif-request-files mod) (sort (cons file (modif-request-files mod)) 'file-src<))
			(save-modifications))))))
	    (beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir mod))))
	(beep-message "There is no selected modification"))))

;;;-----------------------------------------------------------------------------
;;;SET-MOD-FILE-CHANGED
;;;Description
;;;	Mark the file as changed.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	05/03/21	J. P. Varandas	Check the correctness of the signatures
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-mod-file-changed ()
  (let ((mod (current-modif-request))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/" (modif-request-src-dir mod)) filename)
	(let ((source-file (substring filename (match-end 0))))
	  (when (save-excursion (check-all-mods-source-file source-file))
	    (let ((mod-file (make-mod-file-name filename)))
	      (if (file-exists-p (format "%s/%s" (modif-request-org-dir mod) source-file))
		  (let* ((file (make-mod-file source-file mod-file 0 0 0))
			 (res (save-excursion (check-src-file mod file))))
		    (when (consp res)
		      (beep-message (car res)))
		    (setf (modif-request-files mod) (sort (cons file (modif-request-files mod)) 'file-src<))
		    (save-modifications))
		  (beep-message (format "The file %s does not exist in the original dir %s" source-file (modif-request-org-dir mod)))))))
	(beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir mod))))))

;;;-----------------------------------------------------------------------------
;;;SET-MOD-FILE-NEW
;;;Description
;;;	Mark the file as new.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-mod-file-new ()
  (let ((mod (current-modif-request))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/"  (modif-request-src-dir mod)) filename)
	(let ((source-file (substring filename (match-end 0))))
	  (when (check-all-mods-source-file source-file)
	    (let ((mod-file (make-mod-file-name filename)))
	      (if (file-exists-p (format "%s/%s" (modif-request-org-dir mod) source-file))
		  (beep-message (format "The file %s exists in the original dir %s" source-file (modif-request-org-dir mod)))
		  (progn
		    (setf (modif-request-files mod) (sort (cons (make-mod-file source-file mod-file 0 1 0)
							    (modif-request-files mod)) 'file-src<))
		    (save-modifications))))))
	(beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir mod))))))

;;;-----------------------------------------------------------------------------
;;;SET-MOD-FILE-DEL
;;;Description
;;;	Mark the file as deleted
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-mod-file-del ()
  (let ((mod (current-modif-request))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/"  (modif-request-src-dir mod)) filename)
	(let ((source-file (substring filename (match-end 0))))
	  (when (check-all-mods-source-file source-file)
	    (if (file-exists-p (format "%s/%s" (modif-request-org-dir mod) source-file))
		(progn
		  (setf (modif-request-files mod) (sort (cons (make-mod-file source-file "" 1 0 0)
							  (modif-request-files mod)) 'file-src<))
		  (save-modifications))
		(beep-message (format "The file %s does not exist in the original dir %s" source-file (modif-request-org-dir mod))))))
	(beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir mod))))))

;;;-----------------------------------------------------------------------------
;;;SET-MOD-FILE-TO-PATCH
;;;Description
;;;	Sets the current buffer as the patch of the modification
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	01/07/04	J. P. Varandas	Created
;;;	02/06/07	J. P. Varandas	Confirms first before executing the action.
;;;	04/04/23	A. Frazao	If it is a numbered patch names the modification
;;;					patch as the current file name with a new
;;;					version number
;;;	05/04/08	Rui Mestre	Added &optional not.ask.p
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-mod-file-to-patch (&optional not.ask.p)
  (let* ((mod (current-modif-request))
	 (name (file-name-sans-extension (file-name-nondirectory (buffer-file-name (current-buffer)))))
	 (new-name (cond ((string-match "-[0-9][0-9][0-9][0-9]$" name)
			  (format "%s-1.lisp" name))
			 ((string-match "-[0-9][0-9][0-9][0-9]-[0-9]*$" name)
			  (string-match "-[0-9]*$" name)
			  (let ((basic-name (substring name 0 (1+ (match-beginning 0))))
				(number (car (read-from-string (substring name (1+ (match-beginning 0)) (match-end 0))))))
			    (format "%s%d.lisp" basic-name (1+ number))))))
	 (patch (get-patch-file mod new-name))
	 (mod-dir (modif-request-dir mod))
	 (filename (buffer-file-name (current-buffer))))
    (when (or not.ask.p
	      (x-beep-confirm "Do you really want to set this FILE as the patch of the modification?"))
      (sc-copy-file filename (format "%s/%s" mod-dir patch)))))


;;;-----------------------------------------------------------------------------
;;;REM-MOD-FILE
;;;Description
;;;	Removes the current buffer file from the modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun rem-mod-file ()
  (let ((mod (current-modif-request))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/"  (modif-request-src-dir mod)) filename)
	(let* ((source-file (substring filename (match-end 0)))
	       (file (car (member* source-file (modif-request-files mod) :key 'file-src :test 'string-equal))))
	  (if file
	      (progn
		(setf (modif-request-files mod) (remove file (modif-request-files mod)))
		(save-modifications))
	      (beep-message (format "File %s is not in modification %s" source-file (modif-request-name mod)))))
	(beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir mod))))))


;;;-----------------------------------------------------------------------------
;;;MOVE-MOD-FILE
;;;Description
;;;	Moves the current file to another modification
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/06	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun move-mod-file ()
  (let ((mod (current-modif-request))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/"  (modif-request-src-dir mod)) filename)
	(let* ((source-file (substring filename (match-end 0)))
	       (file (car (member* source-file (modif-request-files mod) :key 'file-src :test 'string-equal))))
	  (if file
	      (let ((to-mod (select-modif-request)))
		(cond ((eq to-mod mod)
		       (beep-message (format "Selected the same modification: %s" (modif-request-name mod))))
		      ((null to-mod)
		       (beep-message (format "No modification was selected")))
		      (t (setf (modif-request-files mod) (remove file (modif-request-files mod)))
			 (setf (modif-request-files to-mod) (sort (cons file (modif-request-files to-mod)) 'file-src<))
			 (save-modifications))))
	      (beep-message (format "File %s is not in modification %s" source-file (modif-request-name mod)))))
	(beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir mod)))))
  )

;;;-----------------------------------------------------------------------------
;;;SELECT-MODIF-REQUEST-FROM-FILE
;;;Description
;;;	Selects the modification of the current file.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed function name: select-crews-mod-from-file -> select-modif-request-from-file
;;;					Changed ADT: crews-mod -> modif-request
;;;	09/09/23	P. Madeira	Set `file' variable in a let
;;;-----------------------------------------------------------------------------
(defun select-modif-request-from-file ()
  (let ((current-mod (current-modif-request))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/"  (modif-request-src-dir current-mod)) filename)
	(let ((source-file (substring filename (match-end 0)))
	      (foundp nil))
	  (dolist (mod (all-modif-requests) t)
	    (let ((file (car (member* source-file (modif-request-files mod) :key 'file-src :test 'string-equal))))
	      (when file
		(setf foundp t)
		(when (x-beep-confirm source-file (format "Change to modification %s. Proceed?" 
							  (modif-request-name mod)))
		  (set-current-modif-request mod))
		(return))))
	  (unless foundp
	    (beep-message "Unable to select the modification of the file!")))
	(beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir current-mod)))))
  )

;;;-----------------------------------------------------------------------------
;;;MAKE-MODIF-REQUEST-WITH-FILE
;;;Description
;;;	Creates a modification for the current file removing it from a previous 
;;;	modification if any.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed function name: make-crews-mod-with-file -> make-modif-request-with-file
;;;					Changed ADT: crews-mod -> modif-request
;;;					make-crews-modification -> make-modification-request
;;;	09/09/23	P. Madeira	Set `file' variable in a let
;;;-----------------------------------------------------------------------------
(defun make-modif-request-with-file ()
  (let ((current-mod (current-modif-request))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/"  (modif-request-src-dir current-mod)) filename)
	(let ((source-file (substring filename (match-end 0)))
	      (previous-mod nil)
	      (file nil))
	  (dolist (mod (all-modif-requests) t)
	    (let ((file (car (member* source-file (modif-request-files mod) :key 'file-src :test 'string-equal))))
	      (when file
		(if (x-beep-confirm source-file (format "File already in modification %s (not current). Proceed?" 
							(modif-request-name mod)))
		    (setf previous-mod mod)
		    (setf previous-mod :exit))
		(return))))
	  (unless (eq previous-mod :exit)
	    (make-modification-request)
	    (let ((new-mod (current-modif-request))
		  (mod-file (make-mod-file-name filename)))
	      (setf (modif-request-files new-mod) (list (make-mod-file source-file mod-file 0 0 0)))
	      (when previous-mod
		(setf (modif-request-files previous-mod) (remove file (modif-request-files previous-mod))))
	      (save-modifications)
	      (set-current-modif-request current-mod))))
	(beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir current-mod))))))
  
		    
;;;-----------------------------------------------------------------------------
;;;APPEND-MOD-DEFINITIONS-PATCH
;;;Description
;;;	Re-creates a patch for the current modification with all its changes.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun append-mod-definitions-patch ()
  (recreate-mod-patch t))


;;;-----------------------------------------------------------------------------
;;;APPEND-DEFINITIONS-PATCH
;;;Description
;;;	Re-creates a patch for the current modification with all its changes.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/03/21	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun append-definitions-patch ()
  (let* ((mod (current-modif-request))
	 (comment (car (mode-fillers)))
	 (regexp (format "%s%c%s%c%s" comment 9 (modif-request-date mod) 9 (modif-request-author mod))))
    (save-excursion
      (progn
	(beginning-of-buffer)
	(when (re-search-forward regexp nil t 1)
	  (while (re-search-forward regexp nil t 1)
	    (set-definition-to-patch)))))))


;;;-----------------------------------------------------------------------------
;;;EDIT-MOD-ALL-SOURCE-FILE
;;;Description
;;;	Edits all source files from the current modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		T
;;;History
;;;	Date		Author		Description
;;;	05/03/21	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-mod-all-source-file ()
  (let ((mod (current-modif-request)))
    (dolist (file (modif-request-files mod))
      (unless (delete-file-p file)
	(setq file (format "%s/%s" (modif-request-src-dir mod) (file-src file)))
	(if (file-exists-p file)
	    (switch-to-buffer (find-file-noselect file))
	    (beep-message (format "Could not open file %s" file))))))
  t)


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; OPERATIONS OVER THE CURRENT MODIFICATION
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;REMOVE-MOD-PATCH
;;;Description
;;;	Removes the patch from the current modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun remove-mod-patch ()
  (let* ((mod (current-modif-request))
	 (patch (modif-request-patch mod)))
    (if patch
	(let ((buf (get-file-buffer (format "%s/%s" (modif-request-dir mod) patch))))
	  (when buf
	    (kill-buffer buf))
	  (setf (modif-request-patch mod) nil)
	  (save-modifications))
	(beep-message "No patch in current modification"))))

;;;-----------------------------------------------------------------------------
;;;COPY-SOURCE-MODIFICATION-FILES
;;;Description
;;;	Copies the affected files of the current modification from the source to the modification directory.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dont-ask} is a \emph{boolean}.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	Replaced RUN-PROGRAM by SHELL-COMMAND
;;;	98/11/04	A. Frazao	Uses MY-COPY-FILE
;;;	99/01/12	A. Frazao	MY-COPY-FILE -> SC-COPY-FILE
;;;	99/08/31	J. P. Varandas	Argumento opcional para não perguntar
;;;	04/03/12	A. Frazao	Doc updated
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;					Save buffer before copy
;;;-----------------------------------------------------------------------------
(defun copy-source-modification-files (&optional dont-ask)
  (let* ((mod (current-modif-request))
	 (src-dir (modif-request-src-dir mod))
	 (mod-dir (modif-request-dir mod)))
    (when (or dont-ask
	      (x-beep-confirm (modif-request-name mod) "Copy the SOURCE files to the MODIFICATION dir. Proceed?"))
      (dolist (file (modif-request-files mod))
	(let ((buf (find-buffer-visiting (format "%s/%s" (modif-request-src-dir mod) (file-src file)))))
	  (when (and buf (buffer-modified-p buf)
		     (x-beep-confirm (file-src file) "The buffer will be saved before copy. Continue?"))
	    (save-buffer buf)))
	(unless (delete-file-p file)
	  (sc-copy-file (format "%s/%s" src-dir (file-src file))
			(format "%s/%s" mod-dir (file-mod file)))))))
  t)

;;;-----------------------------------------------------------------------------
;;;COPY-ALL-SOURCE-MODIFICATION-FILES
;;;Description
;;;	Copies the affected files of all modifications from the source to the modification directory.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dont-ask} is a \emph{boolean}.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun copy-all-source-modification-files (&optional dont-ask)
  (unless (all-modif-requests)
    (load-modifications))
  (if (all-modif-requests)
      (when (or dont-ask
		(x-beep-confirm "Confirm" "Copy the SOURCE files to the MODIFICATION dir for all modifications. Proceed?"))
	(let ((current-mod (current-modif-request)))
	  (unwind-protect
	      (dolist (mod (all-modif-requests))
		(set-current-modif-request mod)
		(copy-source-modification-files t))
	    (set-current-modif-request current-mod))))
      (beep-message "There are no modifications")))

;;;-----------------------------------------------------------------------------
;;;COPY-MODIFICATION-SOURCE-FILES
;;;Description
;;;	Copies the affected files of the current modification from the modification to the source directory.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dont-ask} is a \emph{boolean}.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	Replaced RUN-PROGRAM by SHELL-COMMAND
;;;	98/11/04	A. Frazao	Uses MY-COPY-FILE, MY-DELETE-FILE and
;;;					MY-MAKE-DIR
;;;	99/01/12	A. Frazao	MY-COPY-FILE -> SC-COPY-FILE
;;;					MY-DELETE-FILE -> SC-DELETE-FILE
;;;					MY-MAKE-DIR -> SC-MAKE-DIRECTORY
;;;	04/03/12	A. Frazao	Added argument dont-ask
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun copy-modification-source-files (&optional dont-ask)
  (let* ((mod (current-modif-request))
	 (src-dir (modif-request-src-dir mod))
	 (mod-dir (modif-request-dir mod)))
    (when (or dont-ask
	      (x-beep-confirm (modif-request-name mod) "Copy the MODIFICATION files to the SOURCE dir. Proceed?"))
      (dolist (file (modif-request-files mod))
	(if (delete-file-p file)
	    (when (file-exists-p (format "%s/%s" src-dir (file-src file)))
	      (sc-delete-file (format "%s/%s" src-dir (file-src file))))
	    (progn
	      (when (and (new-file-p file)
			 (not (file-directory-p (format "%s/%s" src-dir (file-name-directory (file-src file))))))
		(sc-make-directory (format "%s/%s" src-dir (file-name-directory (file-src file)))))
	      (sc-copy-file (format "%s/%s" mod-dir (file-mod file))
			    (format "%s/%s" src-dir (file-src file))))))))
  t)

;;;-----------------------------------------------------------------------------
;;;COPY-ALL-MODIFICATION-SOURCE-FILES
;;;Description
;;;	Copies the affected files of all modifications from the modification to the source directory.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dont-ask} is a \emph{boolean}.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun copy-all-modification-source-files (&optional dont-ask)
  (unless (all-modif-requests)
    (load-modifications))
  (if (all-modif-requests)
      (when (or dont-ask
		(x-beep-confirm "Confirm" "Copy the MODIFICATION files to the SOURCE dir for all modifications. Proceed?"))
	(let ((current-mod (current-modif-request)))
	  (unwind-protect
	      (dolist (mod (all-modif-requests))
		(set-current-modif-request mod)
		(copy-modification-source-files t))
	    (set-current-modif-request current-mod))))
      (beep-message "There are no modifications")))


;;;-----------------------------------------------------------------------------
;;;COPY-ORIGINAL-SOURCE-FILES
;;;Description
;;;	Copies the affected files of the current modification from the original to the source directory.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dont-ask} is a \emph{boolean}.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	Replaced RUN-PROGRAM by SHELL-COMMAND
;;;	98/11/04	A. Frazao	Uses MY-COPY-FILE
;;;	99/01/12	A. Frazao	MY-COPY-FILE -> SC-COPY-FILE
;;;	04/03/12	A. Frazao	Added argument dont-ask
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun copy-original-source-files (&optional dont-ask)
  (let* ((mod (current-modif-request))
	 (src-dir (modif-request-src-dir mod))
	 (org-dir (modif-request-org-dir mod)))
    (when (or dont-ask
	      (x-beep-confirm (modif-request-name mod) "Copy the ORIGINAL files to the SOURCE dir. Proceed?"))
      (dolist (file (modif-request-files mod))
	(unless (new-file-p file)
	  (sc-copy-file (format "%s/%s" org-dir (file-src file))
			(format "%s/%s" src-dir (file-src file)))))))
  t)


;;;-----------------------------------------------------------------------------
;;;COPY-ALL-ORIGINAL-SOURCE-FILES
;;;Description
;;;	Copies the affected files of all modifications from the original to the source directory.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dont-ask} is a \emph{boolean}.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	04/03/12	A. Frazao	Added argument dont-ask
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun copy-all-original-source-files (&optional dont-ask)
  (unless (all-modif-requests)
    (load-modifications))
  (if (all-modif-requests)
      (when (or dont-ask
		(x-beep-confirm "Confirm" "Copy the ORIGINAL files to the SOURCE dir for all modifications. Proceed?"))
	(let ((current-mod (current-modif-request)))
	  (unwind-protect
	      (dolist (mod (all-modif-requests))
		(set-current-modif-request mod)
		(copy-original-source-files t))
	    (set-current-modif-request current-mod))))
      (beep-message "There are no modifications")))

;;;-----------------------------------------------------------------------------
;;;EDIT-SOURCE-MOD-FILES
;;;Description
;;;	Allows the user to edit a choosen file from the modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	96/08/12	A. Frazao	Use *emacs-init-file*
;;;	97/07/25	A. Frazao	Changed name
;;;	98/11/04	A. Frazao	Uses RUN-PROGRAM
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-source-mod-files ()
  (let* ((mod (current-modif-request))
	 (file (select-mod-file mod)))
    (when file
      (cond ((delete-file-p file)
	     (run-program *emacs-program* "-i" "-l" *emacs-init-file* (format "%s/%s" (modif-request-src-dir mod) (file-src file))))
	    ((new-file-p file)
	     (run-program *emacs-program* "-i" "-l" *emacs-init-file* (format "%s/%s" (modif-request-dir mod) (file-mod file))))
	    (t (run-program *emacs-program* "-i" "-l" *emacs-init-file*
			    (format "%s/%s" (modif-request-src-dir mod) (file-src file))
			    (format "%s/%s" (modif-request-dir mod) (file-mod file)))))))
  t)

;;;-----------------------------------------------------------------------------
;;;EDIT-ORIGINAL-MOD-FILES
;;;Description
;;;	Allows the user to edit the corresponding original file of a choosen 
;;;	file from the modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	96/08/12	A. Frazao	Use *emacs-init-file*
;;;	97/07/25	A. Frazao	Changed name
;;;	98/11/04	A. Frazao	Uses RUN-PROGRAM
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-original-mod-files ()
  (let* ((mod (current-modif-request))
	 (file (select-mod-file mod)))
    (when file
      (cond ((delete-file-p file)
	     (run-program *emacs-program* "-i" "-l" *emacs-init-file* (format "%s/%s" (modif-request-org-dir mod) (file-src file))))
	    ((new-file-p file)
	     (run-program *emacs-program* "-i" "-l" *emacs-init-file* (format "%s/%s" (modif-request-dir mod) (file-mod file))))
	    (t (run-program *emacs-program* "-i" "-l" *emacs-init-file*
			    (format "%s/%s" (modif-request-org-dir mod) (file-src file))
			    (format "%s/%s" (modif-request-dir mod) (file-mod file)))))))
  t)

;;;-----------------------------------------------------------------------------
;;;EDIT-ORIGINAL-SOURCE-FILES
;;;Description
;;;	Allows the user to edit the corresponding source file of a choosen 
;;;	original file from the modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	Created
;;;	98/11/04	A. Frazao	Uses RUN-PROGRAM
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-original-source-files ()
  (let* ((mod (current-modif-request))
	 (file (select-mod-file mod)))
    (when file
      (cond ((delete-file-p file)
	     (run-program *emacs-program* "-i" "-l" *emacs-init-file* (format "%s/%s" (modif-request-org-dir mod) (file-src file))))
	    ((new-file-p file)
	     (run-program *emacs-program* "-i" "-l" *emacs-init-file* (format "%s/%s" (modif-request-src-dir mod) (file-src file))))
	    (t (run-program *emacs-program* "-i" "-l" *emacs-init-file*
			    (format "%s/%s" (modif-request-org-dir mod) (file-src file))
			    (format "%s/%s" (modif-request-src-dir mod) (file-src file)))))))
  t)

;;;-----------------------------------------------------------------------------
;;;EDIT-MOD-SOURCE-FILE
;;;Description
;;;	Edits a source file from the current modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-mod-source-file ()
  (let* ((mod (current-modif-request))
	 (file (select-mod-file mod)))
    (when file
      (setq file (format "%s/%s" (modif-request-src-dir mod) (file-src file)))
      (if (file-exists-p file)
	  (switch-to-buffer (find-file-noselect file))
	  (beep-message (format "Could not open file %s" file)))))
  t)

;;;-----------------------------------------------------------------------------
;;;EDIT-ALL-MOD-SOURCE-FILE
;;;Description
;;;	Edits a source file from all modifications
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-all-mod-source-file ()
  (let ((value (select-all-mod-file)))
    (when value
      (let ((file (car value))
	    (mod (cadr value)))
	(setq file (format "%s/%s" (modif-request-src-dir mod) (file-src file)))
	(if (file-exists-p file)
	    (switch-to-buffer (find-file-noselect file))
	    (beep-message (format "Could not open file %s" file))))))
  t)


;;;-----------------------------------------------------------------------------
;;;EDIT-MOD-ORIGINAL-FILE
;;;Description
;;;	Edits an original file from the current modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-mod-original-file ()
  (let* ((mod (current-modif-request))
	 (file (select-mod-file mod)))
    (when file
      (setq file (format "%s/%s" (modif-request-org-dir mod) (file-src file)))
      (if (file-exists-p file)
	  (switch-to-buffer (find-file-noselect file))
	  (beep-message (format "Could not open file %s" file)))))
  t)


;;;-----------------------------------------------------------------------------
;;;EDIT-ALL-MOD-ORIGINAL-FILE
;;;Description
;;;	Edits an original file from all modifications
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-all-mod-original-file ()
  (let ((value (select-all-mod-file)))
    (when value
      (let ((file (car value))
	    (mod (cadr value)))
	(setq file (format "%s/%s" (modif-request-org-dir mod) (file-src file)))
	(if (file-exists-p file)
	    (switch-to-buffer (find-file-noselect file))
	    (beep-message (format "Could not open file %s" file))))))
  t)


;;;-----------------------------------------------------------------------------
;;;EDIT-MOD-FILE
;;;Description
;;;	Edits a modification file from the current modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-mod-file ()
  (let* ((mod (current-modif-request))
	 (file (select-mod-file mod)))
    (when file
      (if (delete-file-p file)
	  (beep-message (format "Mod file is to delete %s" (file-src file)))
	  (progn
	    (setq file (format "%s/%s" (modif-request-dir mod) (file-mod file)))
	    (if (file-exists-p file)
		(switch-to-buffer (find-file-noselect file))
		(beep-message (format "Could not open file %s" file)))))))
  t)

;;;-----------------------------------------------------------------------------
;;;EDIT-ALL-MOD-FILE
;;;Description
;;;	Edits a modification file from all modifications
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-all-mod-file ()
  (let ((value (select-all-mod-file)))
    (when value
      (let ((file (car value))
	    (mod (cadr value)))
	(if (delete-file-p file)
	  (beep-message (format "Mod file is to delete %s" (file-src file)))
	  (progn
	    (setq file (format "%s/%s" (modif-request-dir mod) (file-mod file)))
	    (if (file-exists-p file)
		(switch-to-buffer (find-file-noselect file))
		(beep-message (format "Could not open file %s" file))))))))
  t)


;;;-----------------------------------------------------------------------------
;;;EDIT-PATCH-FILE
;;;Description
;;;	Edits a patch file from the current modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-patch-file ()
  (let* ((mod (current-modif-request))
	 (file (modif-request-patch mod)))
    (if file
	(progn
	  (setq file (format "%s/%s" (modif-request-dir mod) file))
	  (if (file-exists-p file)
	      (switch-to-buffer (find-file-noselect file))
	      (beep-message (format "Could not open file %s" file))))
	(beep-message (format "No patch in current modification"))))
  t)


;;;-----------------------------------------------------------------------------
;;;EDIT-ALL-PATCH-FILE
;;;Description
;;;	Edits a patch file from all modifications
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;	        void
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-all-patch-file ()
  (let ((mod (select-modif-request)))
    (when mod
      (let ((file (modif-request-patch mod)))
	(if file
	    (progn
	      (setq file (format "%s/%s" (modif-request-dir mod) file))
	      (if (file-exists-p file)
		  (switch-to-buffer (find-file-noselect file))
		  (beep-message (format "Could not open file %s" file))))
	    (beep-message (format "No patch in current modification"))))))
  t)



;;;-----------------------------------------------------------------------------
;;;EDIT-ALL-MOD-MAIL-FILE
;;;Description
;;;	Edits a modification mail file from all modifications
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	06/05/29	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-all-mod-mail-file ()
  (let ((mod (select-modif-request)))
    (when mod
      (let ((file (modif-request-mail-file mod)))
	(if file
	    (if (file-exists-p file)
		(switch-to-buffer (find-file-noselect file))
		(beep-message (format "Could not open file %s" file)))
	    (beep-message (format "No modification mail file created yet for current modification"))))))
  t)



;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; EDIFF STUFF
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;EDIFF-SOURCE-MOD-FILES
;;;Description
;;;	Selects a modification file and runs EDIFF-FILES (merge) for the source and modification files.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/03/24	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun ediff-source-mod-files ()
  (let* ((mod (current-modif-request))
	 (file (select-mod-file mod)))
    (when file
      (cond ((delete-file-p file)
	     (beep-message (format "Cannot merge files marked as deleted: %s" file)))
	    ((new-file-p file)
	     (beep-message (format "Cannot merge new files: %s" file)))
	    (t (ediff-files (format "%s/%s" (modif-request-src-dir mod) (file-src file))
			    (format "%s/%s" (modif-request-dir mod) (file-mod file)))))))
  t)

;;;-----------------------------------------------------------------------------
;;;EDIFF-ORIGINAL-MOD-FILES
;;;Description
;;;	Selects a modification file and runs EDIFF-FILES (merge) for the original and modification files.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/03/24	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun ediff-original-mod-files ()
  (let* ((mod (current-modif-request))
	 (file (select-mod-file mod)))
    (when file
      (cond ((delete-file-p file)
	     (beep-message (format "Cannot merge files marked as deleted: %s" file)))
	    ((new-file-p file)
	     (beep-message (format "Cannot merge new files: %s" file)))
	    (t (ediff-files (format "%s/%s" (modif-request-org-dir mod) (file-src file))
			    (format "%s/%s" (modif-request-dir mod) (file-mod file)))))))
  t)

;;;-----------------------------------------------------------------------------
;;;EDIFF-ORIGINAL-SOURCE-FILES
;;;Description
;;;	Selects a modification file and runs EDIFF-FILES (merge) for the original and source files.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/03/24	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun ediff-original-source-files ()
  (let* ((mod (current-modif-request))
	 (file (select-mod-file mod)))
    (when file
      (cond ((delete-file-p file)
	     (beep-message (format "Cannot merge files marked as deleted: %s" file)))
	    ((new-file-p file)
	     (beep-message (format "Cannot merge new files: %s" file)))
	    (t (ediff-files (format "%s/%s" (modif-request-org-dir mod) (file-src file))
			    (format "%s/%s" (modif-request-src-dir mod) (file-src file)))))))
  t)

;;;-----------------------------------------------------------------------------
;;;EDIFF-FILE-WITH-ORIGINAL
;;;Description
;;;	Runs EDIFF-FILES (merge) for the current file and the original.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/03/24	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun ediff-file-with-original ()
  (let ((mod (current-modif-request))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/" (modif-request-src-dir mod)) filename)
	(let ((source-file (substring filename (match-end 0))))
	  (if (file-exists-p (format "%s/%s" (modif-request-org-dir mod) source-file))
	      (ediff-files (format "%s/%s" (modif-request-org-dir mod) source-file) filename)
	      (beep-message (format "The file %s does not exist in the original dir %s" source-file (modif-request-org-dir mod)))))
	(beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir mod)))))
  )


;;;-----------------------------------------------------------------------------
;;;EDIFF-FILE-WITH-OLD-VERSION
;;;Description
;;;	Runs EDIFF-FILES (merge) for the current file and the selected version of the file
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	06/04/11	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;					Use function 'sc-rplan-system?'
;;;-----------------------------------------------------------------------------
(defun ediff-file-with-old-version ()
  (let ((mod (current-modif-request))
	(filename (buffer-file-name (current-buffer)))
	(result nil)
	(system nil))
    (block exit
      ;; Determina qual a versão para comparar
      (dolist (sys *sc-all-systems*)
	(when (sc-rplan-system? sys)
	  (dolist (version-dir (sc-system-src-version-dirs sys))
	    (let ((dir (if (consp version-dir)
			   (eval version-dir)
			   version-dir)))
	      (when (string-match dir filename)
		(let ((items nil))
		  (dolist (version (sc-system-versions sys))
		    (setf items (cons (sc-make-menu-item version version) items)))
		  (setf result (sc-popup-menu *current-x-arg* "Select" items nil))
		  (setf system sys)
		  (return-from exit)
		  )))))))
    (when result
      (let ((name (downcase (sc-system-name system))))
	(if (string-match (format "^%s/%s-vdev/" (modif-request-src-dir mod) name) filename)
	    (let ((original-dir (system2original-dir system result))
		  (source-file (substring filename (match-end 0))))
	      (if (file-exists-p (format "%s/%s" original-dir source-file))
		  (ediff-files (format "%s/%s" original-dir source-file) filename)
		  (beep-message (format "The file %s does not exist in the original dir %s" source-file (format "%s/%s-%s" (modif-request-org-dir mod) name result)))))
	    (beep-message (format "The file %s does not match the source dir %s" filename (format "%s/%s-vdev/" (modif-request-src-dir mod) name))))))))


;;;-----------------------------------------------------------------------------
;;;EDIFF-FILE-WITH-MODIFICATION
;;;Description
;;;	Runs EDIFF-FILES (merge) for the current file and the modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/03/24	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun ediff-file-with-modification ()
  (let ((mod (current-modif-request))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/"  (modif-request-src-dir mod)) filename)
	(let* ((source-file (substring filename (match-end 0)))
	       (file (car (member* source-file (modif-request-files mod) :key 'file-src :test 'string-equal))))
	  (if file
	      (if (file-exists-p (format "%s/%s" (modif-request-dir mod) (file-mod file)))
		  (ediff-files (format "%s/%s" (modif-request-dir mod) (file-mod file)) filename)
		  (beep-message (format "Modification file %s does not exist" (format "%s/%s" (modif-request-dir mod) (file-mod file)))))
	      (beep-message (format "File %s is not in modification %s" source-file (modif-request-name mod)))))
	(beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir mod))))))


;;;-----------------------------------------------------------------------------
;;;SC-MAKE-FILE-WRITABLE
;;;Description
;;;	Change the permitions of the \arg{file} to be writable
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{file} is a \elem{pathname}.
;;;		
;;;	\return-types	
;;;		void
;;;History
;;;	Date		Author		Description
;;;	01/07/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun sc-make-file-writable (file)
  (let ((cur-mode (file-modes file))
	(writable-mask 146))		;146 = 444 octal
    (set-file-modes file (logior cur-mode writable-mask))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; GENERATION OF THE MODIFICATION HEADER
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;GET-FILE-FROM-ORIGINAL
;;;Description
;;;	Copy the original version of the current file to its place.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/07/04	J. P. Varandas	Created
;;;	06/04/11	J. P. Varandas	Ask first if the user is certain of he/she will do
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun get-file-from-original ()
  (when (x-beep-confirm "Get from original"
		      "Do you really want to get copy this file from the original?")
    (let ((mod (current-modif-request))
	  (filename (buffer-file-name (current-buffer))))
      (if (string-match (format "^%s/" (modif-request-src-dir mod)) filename)
	  (let ((source-file (substring filename (match-end 0))))
	    (if (file-exists-p (format "%s/%s" (modif-request-org-dir mod) source-file))
		(let ((file (format "%s/%s" (modif-request-org-dir mod) source-file)))
		  (sc-make-file-writable filename)
		  (delete-file filename)
		  (sc-copy-file file filename)
		  (revert-buffer t t))
		(beep-message (format "The file %s does not exist in the original dir %s" source-file (modif-request-org-dir mod)))))
	  (beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir mod))))))
  )

;;;-----------------------------------------------------------------------------
;;;GET-FILE-FROM-MODIFICATION
;;;Description
;;;	Copy the modification version of the current file to its place.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/07/04	J. P. Varandas	Created
;;;	06/04/11	J. P. Varandas	Ask first if the user is certain of he/she will do
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun get-file-from-modification ()
  (when (x-beep-confirm "Get from original"
			"Do you really want to get copy this file from the modifcation?")
    (let ((mod (current-modif-request))
	  (filename (buffer-file-name (current-buffer))))
      (if (string-match (format "^%s/"  (modif-request-src-dir mod)) filename)
	  (let* ((source-file (substring filename (match-end 0)))
		 (file (car (member* source-file (modif-request-files mod) :key 'file-src :test 'string-equal))))
	    (if file
		(if (file-exists-p (format "%s/%s" (modif-request-dir mod) (file-mod file)))
		    (let ((file (format "%s/%s" (modif-request-dir mod) (file-mod file))))
		      (sc-make-file-writable filename)
		      (delete-file filename)
		      (sc-copy-file file filename)
		      (revert-buffer t t))
		    (beep-message (format "Modification file %s does not exist" (format "%s/%s" (modif-request-dir mod) (file-mod file)))))
		(beep-message (format "File %s is not in modification %s" source-file (modif-request-name mod)))))
	  (beep-message (format "The file %s does not match the source dir %s" filename (modif-request-src-dir mod)))))))


;; ------------------------------------------------------------------------------
;; CRIACAO DE CABECALHO PARA MENSAGEM
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;GET-SYSTEM
;;;Description
;;;	Computes the list of systems and potencial applicability of the given modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;	\return-types
;;;		A pair of lists of strings
;;;History
;;;	Date		Author		Description
;;;	99/10/19	J. P. Varandas	Created
;;;	05/10/13	J. P. Varandas	'process-system' only returns one value
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun get-system (mod)
  (let ((patch (modif-request-patch mod))
	(dir (modif-request-src-dir mod))
	(results nil))
    (dolist (file (modif-request-files mod))
      (setq results (cons (get-system-of (format "%s/%s" dir (file-src file))) results)))
    (list (process-system results (modif-request-files mod) patch) (process-sub-system results))))


;;;-----------------------------------------------------------------------------
;;;WRITE-MODITICATION-HEADER
;;;Description
;;;	Writes into a buffer the header of the modification file that will be 
;;;	sent to SISCOG.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;	\return-types
;;;		void
;;;	
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Added patch
;;;	99/02/03	A. Frazao	Check if there are affected files
;;;	99/10/19	J. P. Varandas	Use of function 'get-system' that helps on
;;;					filling the header.
;;;	00/05/05	A. Vasconcelos	Added fileds "Test performed:" and "Tested by:"
;;;	00/09/06	A. Frazao	Added field "Functionality:"
;;;	05/03/21	J. P. Varandas	Writes the mod reference
;;;	05/08/05	Fernando	Comented out recipients list
;;;	05/09/16	A. Frazao	Commented (insert "Enviar a:" 10)
;;;	05/10/12	A. Frazao	Removed "Enviar a:"
;;;	05/10/13	J. P. Varandas	If there is a patch in the modification tries 
;;;					to guess the applicability of it
;;;	05/10/19	J. P. Varandas	Only writes the sentence about replacing patches 
;;;					if there is a patch and the system knows which version is used
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun write-moditication-header (mod)
  (let* ((files (modif-request-files mod))
	 (date (current-date-string))
	 (system (get-system mod))
	 (modif-request-patch (modif-request-patch mod))
	 (mod-versions (modif-request-versions mod))
	 (patch-applicability (and modif-request-patch (get-patch-applicability mod)))
	 (old-patch-name (and patch-applicability (car patch-applicability)))
	 (applicability (and patch-applicability (car (cdr patch-applicability))))
	 )
    (insert "--------------------------------------------------------------------------------" 10)
    (insert "System:		")
    (if mod-versions
	(dolist (version mod-versions)
	  (when version
	    (insert version 32)))
	(insert (car system)))
    (insert 10)
    (insert "Sub-system:	" )
    (if patch-applicability
	(dolist (sub-system applicability)
	  (insert sub-system 32))
	(dolist (sub-system (cadr system))
	  (insert sub-system 32)))
    (insert 10)
    (insert "Reference:	" (or (modif-request-reference mod) "") 10)
    (insert "Description:	" 10)
    (insert "Affected files:	")
    (if files
	(progn
	  (cond ((delete-file-p (car files))
		 (insert (file-src (car files)) " DEL" 10))
		((new-file-p (car files))
		 (insert (file-src (car files)) 32 (file-mod (car files)) " NEW" 10))
		(t (insert (file-src (car files)) 32 (file-mod (car files)) 10)))
	  (dolist (file (cdr files))
	    (cond ((delete-file-p file)
		   (insert 9 9 (file-src file) " DEL" 10))
		  ((new-file-p file)
		   (insert 9 9 (file-src file) 32 (file-mod file) " NEW" 10))
		  (t (insert 9 9 (file-src file) 32 (file-mod file) 10)))))
	(insert 10))
    (if modif-request-patch
	(insert "Patch file:	" modif-request-patch 10)
	(insert "Patch file:	" 10))
    (insert "Functionality:	" 10)
    (insert "Test performed:	" 10)
    (if (and modif-request-patch
	     mod-versions
	     old-patch-name)
	(insert "Comments:	CAUTION: The patch replaces the following" 10 9 9 32 32 
		(cadr mod-versions) 
		" - " 
		old-patch-name
		10)
	(insert "Comments:	" 10))
    (insert "--------------------------------------------------------------------------------" 10)
    (insert "		Date		Name			Signature" 10)
    (insert "Author:		" date 9 (modif-request-author mod) 10)
    (insert "Tested by:	" 10)
    (insert "Verified:	" 10)
    (insert "Instalation:	" 10)
    (insert "--------------------------------------------------------------------------------" 10 10 10)
    ))


;;;-----------------------------------------------------------------------------
;;;GET-PATCH-APPLICABILITY
;;;Description
;;;	Tries to get the applicability of the patch based on the current modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;	\return-types
;;;		A pair with one string and a list of strings
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;	09/09/23	P. Madeira	Fix for SISCOG-UTIL sub-systems
;;;-----------------------------------------------------------------------------
(defun get-patch-applicability (mod)
  (let ((mod-vdev    (car (modif-request-versions mod)))
	(mod-version (cadr (modif-request-versions mod)))
	(new-patch-name (file-name-sans-extension (modif-request-patch mod)))
	(old-patch-name nil)
	(patch-name nil)
	(applicability nil))
    
    (when new-patch-name
      (when (string-match "-[0-9][0-9][0-9][0-9]-[0-9]*$" new-patch-name)
	(string-match "-[0-9]*$" new-patch-name)
	(setf patch-name (substring new-patch-name 0 (match-beginning 0)))
	(let ((number (car (read-from-string (substring new-patch-name (1+ (match-beginning 0)) (match-end 0))))))
	  (if (= number 1)
	      (setf old-patch-name patch-name)
	      (setf old-patch-name (format "%s-%d" patch-name (1- number))))))
      
      (when mod-version
	(let* ((name (substring mod-vdev 0 (- (length mod-vdev) 5)))
	       (system (sc-find-system (substring mod-vdev 0 (- (length mod-vdev) 5))))
	       (version (substring mod-version (+ (length name) 1))))
	  (let ((src-dir (system2patches-original-dir system version))
		(top-dir (system-source-top-dir system)))
	    
	    (let ((patches-data (first (directory-files src-dir nil (format "patches-%s.data" (downcase name))))))
	      (if patches-data
		  (let* ((sub-systems nil)
			 (patches-data (concat src-dir "/" patches-data))
			 (curr-buf (current-buffer))
			 (old-buf (get-file-buffer patches-data))
			 (buf (or old-buf
				  (find-file-noselect patches-data))))
		    (when buf
		      (set-buffer buf)
		      (when (search-forward patch-name nil t)
			(while (not (= (char-after (point)) ?"))
			  (forward-char 1))
			(forward-char 1)
			(let ((start (point)))
			  (forward-sexp 1)
			  (setf sub-systems (car (read-from-string (buffer-substring-no-properties start (+ (point) 1)))))
			  (unless (listp sub-systems)
			    (setf sub-systems (list sub-systems)))))

		      (unless old-buf
			(kill-buffer buf))
		      (set-buffer curr-buf)
		      (list old-patch-name sub-systems)))))))))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; CHANGING THE OCCURRENCES OF THE DEVELOPER DATE TO THE CURRENT DATE
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;UPDATE-MODIFICATION-FILES-DATE
;;;Description
;;;	Updates the modification "date" into the current real and actual date.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	
;;;	99/01/29	A. Frazao	Changed second argument to REPLACE-MATCH
;;;	99/08/31	J. P. Varandas	Simplifica o metodo e muda as datas do ficheiro de patch
;;;	99/09/01	J. P. Varandas	Correcção da expressão 'regexp'
;;;	05/03/21	J. P. Varandas	Do not kill the buffer after the replacement if it is already open
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun update-modification-files-date ()
  (let* ((old-buf (current-buffer))
	 (mod (current-modif-request))
	 (dir (modif-request-dir mod))
	 (comment (car (mode-fillers)))
	 (regexp (format "%s%c%s%c%s" comment 9 (modif-request-date mod) 9 (modif-request-author mod)))
	 (replacement (format "%s%c%s%c%s" comment 9 (current-date-string) 9 (modif-request-author mod))))
    
    (dolist (file (modif-request-files mod))
      (unless (delete-file-p file)
	(let ((real-file (format "%s/%s" dir (file-mod file))))
	  (if (file-exists-p real-file)
	      (let* ((old-buffer (get-file-buffer real-file))
		     (buf (or old-buffer (find-file-noselect real-file))))
		(set-buffer buf)
		(beginning-of-buffer)
		(replace-string regexp replacement)
		(save-buffer buf)
		(unless old-buffer
		  (kill-buffer buf)))
	      (beep-message (format "File %s does not exist" real-file))))))
    (when (modif-request-patch mod)
      (let ((real-file (format "%s/%s" dir (modif-request-patch mod))))
	(if (file-exists-p real-file)
	    (let* ((old-buffer (get-file-buffer real-file))
		   (buf (or old-buffer (find-file-noselect real-file))))
	      (set-buffer buf)
	      (beginning-of-buffer)
	      (replace-string regexp replacement)
	      (save-buffer buf)
	      (unless old-buffer
		(kill-buffer buf)))
	    (beep-message (format "File %s does not exist" real-file)))))
    (set-buffer old-buf))
  t)

;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; CREATING A PATCH FOR THE MODIFICATION
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SET-NEW-FILE-TO-PATCH
;;;Description
;;;	Copies a complete file into a patch.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun set-new-file-to-patch ()
  (let* ((mode-filler (mode-fillers))
	 (filler (cadr mode-filler))
	 (mod (current-modif-request))
	 (filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/" (modif-request-src-dir mod)) filename)
	(let ((source-file (substring filename (match-end 0)))
	      (patch (get-patch-file mod))
	      start
	      end
	      def-str)
	  (save-excursion
	    (beginning-of-buffer)
	    (setq start (point))
	    (end-of-buffer)
	    (setq end (point))
	    (setf def-str (buffer-substring start end)))
	  (let* ((file (format "%s/%s" (modif-request-dir mod) patch))
		 (old-buf (current-buffer))
		 (buf (or (get-file-buffer file)
			  (find-file-noselect file))))
	    (set-buffer buf)
	    (my-end-of-buffer)
	    (insert filler 10)
	    (insert (car mode-filler) " File: " source-file 10)
	    (insert filler 10 10)
	    (insert def-str 10 10)
	    (save-buffer)
	    (set-buffer old-buf)))
	(beep-message "Cannot match the source file"))))

;;;-----------------------------------------------------------------------------
;;;RECREATE-MOD-PATCH
;;;Description
;;;	Re-creates a patch for the current modification with all its changes
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dont-delete-patch-first} is a boolean
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	04/03/26	A. Frazao	Re-uses the original patch name
;;;	05/10/13	J. P. Varandas	Have a new argument that prevents to delete the patch
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun recreate-mod-patch (&optional dont-delete-patch-first)
  (let* ((mod (current-modif-request))
	 (comment (car (mode-fillers)))
	 (regexp (format "%s%c%s%c%s" comment 9 (modif-request-date mod) 9 (modif-request-author mod)))
	 (src-dir (modif-request-src-dir mod)))
    (when (and (modif-request-patch mod)
	       (not dont-delete-patch-first))
      (let* ((file (format "%s/%s" (modif-request-dir mod) (modif-request-patch mod)))
	     (buf (find-buffer-visiting file)))
	(if (file-exists-p file)
	    (delete-file file))
	(if buf
	    (kill-buffer buf))))
    (save-excursion
      (dolist (file (modif-request-files mod))
	(unless (delete-file-p file)
	  (let* ((src-file (format "%s/%s" src-dir (file-src file)))
		 (old-buf (get-file-buffer src-file))
		 (buf (or old-buf
			  (find-file-noselect src-file))))
	    (set-buffer buf)
	    (if (new-file-p file)
		(set-new-file-to-patch)
		(progn
		  (beginning-of-buffer)
		  (when (re-search-forward regexp nil t 1)
		    (while (re-search-forward regexp nil t 1)
		      (set-definition-to-patch)))))
	    (unless old-buf
	      (kill-buffer buf))))))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; CREATING A MODIFICATION FROM A MAIL FILE
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------



;;;-----------------------------------------------------------------------------
;;;PROCESS-SELECTED-DATA-ITEM
;;;Description
;;;	If the given \arg{result} is a directory ask user to select another 
;;;	path inside the directory, if not returns the pathname.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{result} is a pair (pathname, type of pathname)
;;;		
;;;	\return-types
;;;		A \emph{pathname} or NIL
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun process-selected-data-item (result)
  (let ((path (car result))
	(type (cadr result)))
    (case type
      (0 path)
      (1 (select-dir-file path)))))


;;;-----------------------------------------------------------------------------
;;;SELECT-DIR-FILE
;;;Description
;;;	Ask user to select a file inside the directories tree starting from \arg{path}
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{path} is a \emph{pathname}
;;;		
;;;	\return-types
;;;		A \emph{pathname} or NIL
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun select-dir-file (path)
  (let ((items (append (directories-items path) (files-items path))))
    (if items
	(let ((result (sc-popup-menu *current-x-arg* "Select" items nil 40)))
	  (if result
	      (process-selected-data-item result)))
	(beep))))


;;;-----------------------------------------------------------------------------
;;;MAKE-MODIFICATION-FROM-MAIL-FILE
;;;Description
;;;	Creates a modification based on a modification file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Created
;;;	99/09/13	J. P. Varandas	Protege o caso em que não há 'Affected files:'
;;;	99/10/19	J. P. Varandas	Use the variable *crews-reverted-mail*
;;;	05/10/13	J. P. Varandas	Allow to select a modification file to revert
;;;	05/10/19	J. P. Varandas	Do not use the information computed by the 
;;;					X-Mail-Tool concerning the number of lines of each file.
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;					make-crews-modification -> make-modification-request
;;;					*crews-mail-dir* -> *modifs-mail-dir*
;;;	09/09/23	P. Madeira	`previous-line' -> `forward-line' (EMACS 23)
;;;	10/08/06	P. Madeira	`insert-file-contents' -> `insert-buffer-substring'
;;;-----------------------------------------------------------------------------
(defun make-modification-from-mail-file ()
  (let ((file (select-dir-file *modifs-mail-dir*)))
    (if (and file (file-exists-p file))
	(progn
	  (make-modification-request)
	  (let ((mod (current-modif-request))
		(old-buf (current-buffer))
		(buf (find-file-noselect file))
		(start 0)
		(end 0)
		(source-file-name nil)
		(mod-file-name nil))
	    (set-buffer buf)
	    (beginning-of-buffer)
	    ;; Posiciona na lista de ficheiros afectados
	    (search-forward "Affected files:" nil t)
	    (while (not (looking-at "Patch file:"))
	      (setf source-file-name (next-str))
	      (if (looking-at " file:")  ;; Não há Affected files
		  (forward-line -1)
		  (if (looking-at " DEL")
		      (setf (modif-request-files mod) (cons (make-mod-file source-file-name "" 1 0 0) (modif-request-files mod)))
		      (progn
			(setf mod-file-name (next-str))
			(if (looking-at " NEW")
			    (setf (modif-request-files mod) (cons (make-mod-file source-file-name mod-file-name 0 1 0) (modif-request-files mod)))
			    (setf (modif-request-files mod) (cons (make-mod-file source-file-name mod-file-name 0 0 0) (modif-request-files mod)))))))
	      (line-feed))
	    ;; Ordena ficheiros
	    (setf (modif-request-files mod) (sort (modif-request-files mod) 'file-src<))
	    
	    ;; Trata do patch
	    (forward-char (length "Patch file:"))
	    (while (or (= (char-after (point)) 32)
		       (= (char-after (point)) 9))
	      (forward-char 1))
	    ;; Obtem o nome do ficheiro de patch
	    (if (not (= (char-after (point)) 10))
		(setf mod-file-name (next-str))
		(setf mod-file-name nil))
	    ;; Obtem o posicionamento do ficheiro de patch dentro do ficheiro de modificação
	    (when mod-file-name
	      (beginning-of-buffer)
	      (when (search-forward (format "X-Sun-Data-Name: %s" mod-file-name) nil t)
		(line-feed)
		(while (looking-at "X-Sun")
		  (line-feed))
		(line-feed)
		(setf start (point))
		(cond ((search-forward "X-Sun" nil t)
		       (forward-line -2)
		       (beginning-of-line)
		       (setf end (point)))
		      (t
		       (end-of-buffer)
		       (setf end (point))))
		;; Insere a informação na modificação
		(setf mod-file-name (make-mod-file-name mod-file-name))
		(setf (modif-request-patch mod) mod-file-name)
		;; Cria e preenche o buffer do respectivo ficheiro
		(let ((mod-buf (get-buffer mod-file-name)))
		  (when mod-buf
		    (kill-buffer mod-buf))
		  (setq mod-buf (get-buffer-create mod-file-name))
		  (set-buffer mod-buf)
		  (erase-buffer)
		 (insert-buffer-substring buf start end)
		  (write-file (format "%s/%s" (modif-request-dir mod) mod-file-name))
		  (set-buffer buf)
		  (kill-buffer mod-buf))))
	    
	    ;; Trata dos ficheiros de modificacoes
	    (dolist (mod-file (modif-request-files mod))
	      (unless (delete-file-p mod-file)
		(beginning-of-buffer)
		(when (search-forward (format "X-Sun-Data-Name: %s" (file-mod mod-file)) nil t)
		  (line-feed)
		  (while (looking-at "X-Sun")
		    (line-feed))
		  (line-feed)
		  (setf start (point))
		  (cond ((search-forward "X-Sun" nil t)
			 (beginning-of-line)
			 (forward-line -1)
			 (setf end (point)))
			(t
			 (end-of-buffer)
			 (setf end (point))))
		  ;; Insere a informação na modificação
		  (setf mod-file-name (make-mod-file-name (format "%s/%s" (modif-request-src-dir mod) (file-src mod-file))))
		  (set-file-mod mod-file mod-file-name)
		  ;; Cria e preenche o buffer do respectivo ficheiro
		  (let ((mod-buf (get-buffer mod-file-name)))
		    (when mod-buf
		      (kill-buffer mod-buf))
		    (setq mod-buf (get-buffer-create mod-file-name))
		    (set-buffer mod-buf)
		    (erase-buffer)
		    (insert-buffer-substring buf start end)
		    (write-file (format "%s/%s" (modif-request-dir mod) mod-file-name))
		    (set-buffer buf)
		    (kill-buffer mod-buf))
		  )))
	    (set-buffer old-buf)
	    (kill-buffer buf)
	    (save-modifications)))
	(beep-message (format "File %s does not exist" file))
      )))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; GENERATING THE MAIL FILE
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;GET-MODIF-REQUEST-MAIL-FILE
;;;Description
;;;	Builds and returns the string that identifies the modification file.
;;;	The string is stored in \arg{mod} as the \emph{mail-file}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{mod} is a \elem{modif-request}
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/09/14	A. Frazao	Created
;;;	05/09/16	A. Frazao	Added *windows-cri*
;;;	05/10/13	J. P. Varandas	After setting the mail-file it saves the modifications
;;;	09/02/10	J. P. Varandas	Changed function name: get-crews-mod-mail-file -> get-modif-request-mail-file
;;;					Changed ADT: crews-mod -> modif-request
;;;					Removed *windows-cri*
;;;					*crews-mail-dir* -> *modifs-mail-dir*
;;;					*crews-mail-name* -> *modif-mail-name*
;;;-----------------------------------------------------------------------------
(defun get-modif-request-mail-file (mod)
  (let* ((time-list (decode-time))
	 (time-string (format "%02d%02d%02d-%02d%02d%02d"
			      (elt time-list 5)
			      (elt time-list 4)
			      (elt time-list 3)
			      (elt time-list 2)
			      (elt time-list 1)
			      (elt time-list 0)))
	 (mail-file (format "%s/%s-%s-%s" *modifs-mail-dir* *modif-mail-name* (user-login-name) time-string)))
    (setf (modif-request-mail-file mod) mail-file)
    (save-modifications)
    mail-file))


;;;-----------------------------------------------------------------------------
;;;MAKE-MAIL-FILE
;;;Description
;;;	Creates the modification mail file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Passou a informar o big-brother...
;;;	98/11/04	A. Frazao	Added patch
;;;	99/03/17	Dario		Only replace ^M on end of line.
;;;	99/03/18	Dario		Simplified previous change.
;;;	99/08/31	J. P. Varandas	O apagar do ^M é feito apagando o caracter 13
;;;	99/09/28	A. Frazao	Calls REMOVE-CR
;;;	99/10/19	J. P. Varandas	Add functionality to store the old modification mails
;;;	05/08/05	Fernando	Added label "CRI" to "Subject:"
;;;	05/09/14	A. Frazao	Calls get-crews-mod-mail-file
;;;	05/10/13	J. P. Varandas	Remove use of *crews-mail-versions*
;;;	05/10/14	A. Frazao	Create buffer in normal mode (non binary)
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;					*crews-mail-from* -> *modif-mail-from*
;;;-----------------------------------------------------------------------------
(defun make-mail-file ()
  (let* ((old-buf (current-buffer))
	 (mod (current-modif-request))
	 (dir (modif-request-dir mod))
	 (file (get-modif-request-mail-file mod))
	 (buf (get-buffer (file-name-nondirectory file)))
	 (subject (modif-request-name mod)))
    (when buf
      (kill-buffer buf))
    (setq buf (find-file-noselect file))
    (set-buffer buf)
    (erase-buffer)
    (insert "From " *modif-mail-from* 10)
    (insert "Subject: CRI - " subject 10)
    (insert "Content-Type: X-sun-attachment" 10)
    (insert "Status: O" 10 10)
    
    (insert "----------" 10)
    (insert "X-Sun-Data-Type: text" 10)
    (insert "X-Sun-Data-Description: text" 10)
    (insert "X-Sun-Data-Name: text" 10)
    (insert "X-Sun-Charset: us-ascii" 10 10)
    
    (write-moditication-header mod)
    (update-modification-files-date)
    
    (dolist (file (modif-request-files mod))
      (unless (delete-file-p file)
	(insert "----------" 10)
	(insert "X-Sun-Data-Type: default" 10)
	(insert "X-Sun-Data-Description: default" 10)
	(insert "X-Sun-Data-Name: " (file-mod file) 10)
	(insert "X-Sun-Charset: us-ascii" 10 10)
	(let ((tem (insert-file-contents (format "%s/%s" dir (file-mod file)))))
	  (forward-char (car (cdr tem)))
	  (backward-char)
	  (if (not (= (char-after (point)) 10))
	      (progn
		(forward-char)
		(insert 10))
	      (forward-char)))
	))

    (when (modif-request-patch mod)
      (insert "----------" 10)
      (insert "X-Sun-Data-Type: default" 10)
      (insert "X-Sun-Data-Description: default" 10)
      (insert "X-Sun-Data-Name: " (modif-request-patch mod) 10)
      (insert "X-Sun-Charset: us-ascii" 10 10)
      (let ((tem (insert-file-contents (format "%s/%s" dir (modif-request-patch mod)))))
	(forward-char (car (cdr tem)))
	(backward-char)
	(if (not (= (char-after (point)) 10))
	    (progn
	      (forward-char)
	      (insert 10))
	    (forward-char))))
    
    (save-buffer buf)
    (kill-buffer buf)
;;;    (when *windows-emacs*
;;;      (setq buf (find-file-binary file))
;;;      (remove-cr)
;;;      (save-buffer buf)
;;;      (kill-buffer buf))
    (set-buffer old-buf)

    (inform-big-brother "END_M" mod))
  t)


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; PROCESSING FOR THE GENERATION OF THE MAIL FILE
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;PROCESS-MAIL-FILE
;;;Description
;;;	Processing for the generation of the mail file.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	99/09/13	J. P. Varandas	Replace beep-confirm by y-or-n-p
;;;	01/07/04	J. P. Varandas	When it is only a patch it also deletes the modification
;;;	02/06/07	J. P. Varandas	Show the buffer with the mail file
;;;	05/09/14	A. Frazao	Calls crews-mod-mail-file
;;;	05/10/13	J. P. Varandas	'check-src-file' also checks by documentation
;;;	06/04/11	J. P. Varandas	Do not delete the modification
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;	10/09/06	Rui Patrocinio	Check source file even if to be deleted
;;;-----------------------------------------------------------------------------
(defun process-mail-file ()
  (let ((mod (current-modif-request)))
    (cond ((modif-request-files mod)
	   (dolist (file (modif-request-files mod))
	     (check-src-file mod file t))
	   (copy-source-modification-files t)
	   (switch-to-buffer (edit-current-modif-request)))
	  ((and (modif-request-patch mod)
		(y-or-n-p "Do you want to send the PATCH FILE"))
	   (make-mail-file)
	   (switch-to-buffer (other-buffer))
	   (let ((file (modif-request-mail-file mod)))
	     (switch-to-buffer (find-file-noselect file)))))))


;;;-----------------------------------------------------------------------------
;;;EDIT-MOD-MAIL-FILE
;;;Description
;;;	Allows to edit the modification mail file if it exists.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Created
;;;	06/05/29	A. Frazao	Check if file exists
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun edit-mod-mail-file ()
  (let ((file (modif-request-mail-file (current-modif-request))))
    (if file
	(if (file-exists-p file)
	    (switch-to-buffer (find-file-noselect file))
	    (beep-message (format "Could not open file %s" file)))
	(beep-message "This modification does not have a mail file yet"))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; DISPLAYING THE EDITOR OF THE CURRENT MODIFICATION
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;MOD-MENU-MODE-MAP
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar mod-menu-mode-map nil "")

(if mod-menu-mode-map
    ()
    (setq mod-menu-mode-map (make-keymap))
    (suppress-keymap mod-menu-mode-map t)
    (define-key mod-menu-mode-map "q" 'mod-menu-quit)
    (define-key mod-menu-mode-map "e" 'mod-menu-edit)
    (define-key mod-menu-mode-map "x" 'mod-menu-execute)
    (define-key mod-menu-mode-map "r" 'mod-menu-remove-file)
    (define-key mod-menu-mode-map "d" 'mod-menu-delete-file)
    (define-key mod-menu-mode-map "c" 'mod-menu-change-file)
    (define-key mod-menu-mode-map "s" 'mod-menu-check-signature)
    (define-key mod-menu-mode-map "h" 'mod-menu-help)
    )

(put 'mod-menu-mode 'mode-class 'special)

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-MODE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun mod-menu-mode ()
  (kill-all-local-variables)
  (use-local-map mod-menu-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'mod-menu-mode)
  (setq mode-name "Modification Menu")
  (run-hooks 'mod-menu-mode-hook))
  
;;;-----------------------------------------------------------------------------
;;;EDIT-CURRENT-MODIF-REQUEST
;;;Description
;;;	Presents a interactive buffer to edit the modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A buffer
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed function name: edit-current-crews-mod -> edit-current-modif-request
;;;					Changed ADT: crews-mod -> modif-request
;;;					*edit-crews-modification* -> *edit-modification-request*
;;;-----------------------------------------------------------------------------
(defun edit-current-modif-request ()
  (interactive)
  (let ((buf (get-buffer "*edit-modification-request*")))
    (when buf
      (kill-buffer buf))
    (setq buf (get-buffer-create "*edit-modification-request*"))
    (switch-to-buffer buf)
    (erase-buffer)
    (modif-request-editor)
    (pop-to-buffer buf)
    (beginning-of-buffer)
    (forward-line 2)
    (mod-menu-mode)
    (mod-menu-help)
    buf))

;;;-----------------------------------------------------------------------------
;;;IS-THE-FILE-CORRECT
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun is-the-file-correct (status)
  (if (or (null status)
	  (and (not (listp status))
	       (= status ERR_CORRECT)))
      "OK "
      "ERR"))

;;;-----------------------------------------------------------------------------
;;;MODIF-REQUEST-EDITOR
;;;Description
;;;	Displays the current modification in the interactive buffer
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed function name: crews-mod-editor -> modif-request-editor
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun modif-request-editor ()
  (let* ((mod (current-modif-request))
	 (files (modif-request-files mod)))
    (insert " OK CHR STA FILE -> MODIFICATION" 10 " == === === ====    ============" 10)
    (dolist (file files)
      (insert "    " (editor-file-status file) 32 (is-the-file-correct (file-status file)) 32 (file-src file))
      (if (delete-file-p file)
	  (insert 10)
	  (insert 9 (file-mod file) 10)))))

;;;-----------------------------------------------------------------------------
;;;EDITOR-FILE-STATUS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun editor-file-status (file)
  (cond ((delete-file-p file) "DEL")
	((new-file-p file) "NEW")
	(t "CHG")))

(defvar mod-menu-mod-column 12)
(defvar mod-menu-modif-column 1)
(defvar mod-menu-char-column 4)

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-MOD-FILE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun mod-menu-mod-file ()
  "Return mod described by this line of mod menu."
  (save-excursion
    (beginning-of-line)
    (forward-char mod-menu-mod-column)
    (let ((start (point)))
      (next-space)
      (buffer-substring start (point)))))

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-MODIFIED
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun mod-menu-modified ()
  "Return mod described by this line of mod menu."
  (save-excursion
    (beginning-of-line)
    (forward-char mod-menu-modif-column)
    (let ((start (point)))
      (forward-char 1)
      (string-equal "*" (buffer-substring start (point))))))

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-FILE-CHAR
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun mod-menu-file-char ()
  "Return mod described by this line of mod menu."
  (save-excursion
    (beginning-of-line)
    (forward-char mod-menu-char-column)
    (let ((start (point)))
      (forward-char 3)
      (buffer-substring start (point)))))

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-SELECT-MOD-FILE
;;;Description
;;;	Selects a modification file from the interactive buffer
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun mod-menu-select-mod-file ()
  (let ((mod (current-modif-request))
	(name (mod-menu-mod-file)))
    (dolist (file (modif-request-files mod))
      (when (string-equal (file-src file) name)
	(return file)))))

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-REDISPLAY
;;;Description
;;;	Selects a modification file from the interactive buffer
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun mod-menu-redisplay ()
  (let ((buffer-read-only nil))
    (erase-buffer)
    (modif-request-editor)
    (pop-to-buffer buffer)
    (beginning-of-buffer)
    (forward-line 2)))

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-QUIT
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun mod-menu-quit ()
  "Quit the mod menu."
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-buffer (other-buffer))
    (kill-buffer buffer)))

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-EDIT
;;;Description
;;;	Edits the files and when there is a change in a file it compares the files.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	99/09/10	J. P. Varandas	QUando se edita um ficheiro compara-se 
;;;					com o original e não com a source
;;;	01/07/04	J. P. Varandas	Não abre um novo emacs mas edita os ficheiros na mesma sessão
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun mod-menu-edit ()
  (interactive)
  (beginning-of-line)
  (if (looking-at " [=O]")
      (ding)
      (let ((mod (current-modif-request))
	    (file (mod-menu-select-mod-file)))
	(let ((buffer-read-only nil))
	  (delete-char 2)
	  (insert " *")
	  (forward-line 1))
	(cond ((delete-file-p file)
	       (switch-to-buffer (find-file-noselect (format "%s/%s" (modif-request-src-dir mod) (file-src file)))))
	      ((new-file-p file)
	       (switch-to-buffer (find-file-noselect (format "%s/%s" (modif-request-dir mod) (file-mod file)))))
	      (t (ediff-files (format "%s/%s" (modif-request-org-dir mod) (file-src file))
			      (format "%s/%s" (modif-request-dir mod) (file-mod file))))))))


;;;-----------------------------------------------------------------------------
;;;MOD-MENU-EXECUTE
;;;Description
;;;	Creates the mail file with the selected files
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	01/07/04	J. P. Varandas	Selects the mail file buffer
;;;	05/09/14	A. Frazao	Calls crews-mod-mail-file
;;;	05/10/13	J. P. Varandas	Does not delete the crews modification
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun mod-menu-execute ()
  (interactive)
  (let ((buffer (current-buffer)))
    (make-mail-file)
    (switch-to-buffer (other-buffer))
    (kill-buffer buffer))
  (let ((*current-x-arg* t))
    (copy-modification-source-files))
  (let ((file (modif-request-mail-file (current-modif-request))))
    (switch-to-buffer (find-file-noselect file))))


;;;-----------------------------------------------------------------------------
;;;MOD-MENU-REMOVE-FILE
;;;Description
;;;	Removes the file from the modification in the interactive buffer
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun mod-menu-remove-file ()
  (interactive)
  (beginning-of-line)
  (if (looking-at " [=O]")
      (ding)
      (let ((mod (current-modif-request))
	    (file (mod-menu-select-mod-file))
	    (buffer (current-buffer)))
	(setf (modif-request-files mod) (remove file (modif-request-files mod)))
	(save-modifications)
	(switch-to-buffer buffer)
	(mod-menu-redisplay))))

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-DELETE-FILE
;;;Description
;;;	Marks the file as deleted
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun mod-menu-delete-file ()
  (interactive)
  (beginning-of-line)
  (if (looking-at " [=O]")
      (ding)
      (let ((mod (current-modif-request))
	    (file (mod-menu-select-mod-file))
	    (char (mod-menu-file-char))
	    (buffer (current-buffer)))
	(if (string-equal char "NEW")
	    (ding)
	    (if (string-equal char "CHG")
		(progn
		  (setf (modif-request-files mod) (remove file (modif-request-files mod)))
		  (setf (modif-request-files mod) (sort (cons (make-mod-file (file-src file) "" 1 0 0)
							  (modif-request-files mod)) 'file-src<))
		  (save-modifications)
		  (switch-to-buffer buffer)
		  (mod-menu-redisplay))
		(ding))))))

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-CHANGE-FILE
;;;Description
;;;	Allow to change a deleted file into a changed file
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	05/10/13	J. P. Varandas	'check-src-file' also checks by documentation
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun mod-menu-change-file ()
  (interactive)
  (beginning-of-line)
  (if (looking-at " [=O]")
      (ding)
      (let ((mod (current-modif-request))
	    (file (mod-menu-select-mod-file))
	    (char (mod-menu-file-char))
	    (buffer (current-buffer)))
	(let ((*current-x-arg* t))
	  (beep-message (format "%s" file)))
	(if (string-equal char "NEW")
	    (ding)
	    (if (string-equal char "DEL")
		(let ((mod-file (make-mod-file-name (file-src file))))
		  (setf (modif-request-files mod) (remove file (modif-request-files mod)))
		  (setf file (make-mod-file (file-src file) mod-file 0 0 0))
		  (setf (modif-request-files mod) (sort (cons file (modif-request-files mod)) 'file-src<))
		  (check-src-file mod file t)
		  (save-modifications)
		  (switch-to-buffer buffer)
		  (mod-menu-redisplay))
		(ding))))))

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-CHECK-SIGNATURE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun mod-menu-check-signature ()
  (interactive)
  (beginning-of-line)
  (if (looking-at " [=O]")
      (ding)
      (let* ((file (mod-menu-select-mod-file))
	     (status (file-status file)))
	(when status
	  (unless (listp status)
	    (cond ((= status ERR_DEL_FILE)    (setq status (list "Original file does not exist")))
		  ((= status ERR_NEW_FILE)    (setq status (list "Original file exists")))
		  ((= status ERR_NO_SRC_FILE) (setq status (list "Source file does not exist")))
		  ((= status ERR_NO_MOD_FILE) (setq status (list "Modification file does not exist")))
		  ((= status ERR_NO_ORG_FILE) (setq status (list "Original file does not exist")))
		  ((= status ERR_SIGNATURE)   (setq status (list "Signatures are not correct")))
		  (t (setq status nil)))))
	(when status
	  (let ((*current-x-arg* t))
	    (apply 'beep-message status))))))

;;;-----------------------------------------------------------------------------
;;;MOD-MENU-HELP
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun mod-menu-help ()
  (interactive)
  (message "Commands: e edit; r remove file; d mark delete; c mark change; s show error; q quit; x execute; h help"))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; SENDING THE MODIFICATION MAIL
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;GET-MOD-MAIL-SUBJECT
;;;Description
;;;	Retrieves the string with the "Subject" field of the modification mail 
;;;	file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{string}.
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/08/05	Fernando	Created
;;;	05/09/14	A. Frazao	Moved from sc-mail
;;;-----------------------------------------------------------------------------
(defun get-mod-mail-subject ()
  (save-excursion
    (let ((start nil))
      (beginning-of-buffer)
      (when (search-forward "Subject: " nil t)
	(setf start (point))
	(end-of-line)
	(buffer-substring start (point))))))


;;;-----------------------------------------------------------------------------
;;;GET-MOD-MAIL-HEADER
;;;Description
;;;	Retrieves the string with the header of the modification mail file. The
;;;	header will be turned into the body of the modification's mail message.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{string}.
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/08/05	Fernando	Created
;;;	05/09/14	A. Frazao	Moved from sc-mail
;;;-----------------------------------------------------------------------------
(defun get-mod-mail-header ()
  (save-excursion
    (let ((separator "--------------------------------------------------------------------------------")
	  (start nil))
      (beginning-of-buffer)
      (when (search-forward separator nil t)
	(beginning-of-line)
	(setf start (point))
	(forward-line 1)
	(when (and (search-forward separator nil t)
		   (forward-line 1)
		   (search-forward separator nil t))
	  (forward-line 1)
	  (beginning-of-line)
	  (buffer-substring start (point)))))))

  
;;;-----------------------------------------------------------------------------
;;;SEND-MODIF-REQUEST-MAIL
;;;Description
;;;	Sends a crews modification mail
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\emph{to} is a list of \emph{string}, each being the name of a
;;;		SISCOG mail address. The name is to be used in an address that has
;;;		the following syntax <name>@siscog.pt
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/09/14	A. Frazao	Created
;;;	05/09/16	A. Frazao	Added argument to. No longer interactive.
;;;					Updated protocol to sc-mail-send-buffer
;;;	05/10/12	A. Frazao	Updated confirmation message
;;;	06/05/29	A. Frazao	Sets crews-mod-mail-sent
;;;	09/02/10	J. P. Varandas	Changed function name: send-crews-mod-mail -> send-modif-request-mail
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun send-modif-request-mail (to)
  (interactive (list (read-string "Mail address: ")))
  (let ((mod (current-modif-request))
	(*current-x-arg* t))
    (cond ((null (current-modif-request))
	   (beep-message "No modification selected."))
	  ((null (modif-request-mail-file mod))
	   (beep-message (format "%s - No modification mail file." (modif-request-name mod))))
	  (t (let ((current-buffer (current-buffer))
		   (mod-name (modif-request-name mod))
		   (mod-buffer (get-buffer (file-name-nondirectory (modif-request-mail-file mod)))))
	       (cond ((null mod-buffer)
		      (beep-message (format "%s - No modification mail buffer." mod-name)))
		     ((not (eql current-buffer mod-buffer))
		      (beep-message (format "%s - Current buffer is not the modification mail buffer." mod-name)))
		     (t (when (and (x-beep-confirm (format "%s - Do you really want to send the modification to %s?"
							   mod-name
							   to))
				   (or (not (buffer-modified-p mod-buffer))
				       (and (x-beep-confirm "The modification buffer will be saved to disk before sending. Continue?")
					    (or (save-buffer mod-buffer) t))))
			  (let ((subject (get-mod-mail-subject))
				(header (get-mod-mail-header)))
			    (if (sc-mail-send-buffer (list to) (list user-mail-address) subject header mod-buffer)
				(progn
				  (setf (modif-request-mail-sent mod) t)
				  (save-modifications)))))))))
	  )))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; DISPLAYING THE CURRENT MODIFICATION
;;; 
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;DISPLAY-CURRENT-MODIF-REQUEST
;;;Description
;;;	Displays the information of the current modification request
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed function name: display-current-crews-mod -> display-current-modif-request
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun display-current-modif-request ()
  (let ((old-buf (current-buffer))
	(buf (get-buffer-create "*modification-request*")))
    (set-buffer buf)
    (erase-buffer)
    (display-modif-request (current-modif-request))
    (switch-to-buffer-other-window buf)
    (pop-to-buffer old-buf)))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; DISPLAYING ALL MODIFICATIONS
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;DISPLAY-ALL-MODIF-REQUESTS
;;;Description
;;;	Displays the information of all modification requests
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	If the modifications were not yet been loaded
;;;					then loads them
;;;	09/02/10	J. P. Varandas	Changed function name: display-all-crews-mods -> display-all-modif-requests
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun display-all-modif-requests ()
  (unless (all-modif-requests)
    (load-modifications))
  (let ((old-buf (current-buffer))
	(buf (get-buffer-create "*modification-request*")))
    (set-buffer buf)
    (erase-buffer)
    (dolist (mod (all-modif-requests))
      (display-modif-request mod))
    (switch-to-buffer-other-window buf)
    (pop-to-buffer old-buf)))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; SELECTING A MODIFICATION
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SELECT-CURRENT-MODIF-REQUEST
;;;Description
;;;	Allows to select a modification request
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	If the modifications were not yet been loaded
;;;					then loads them
;;;	09/02/10	J. P. Varandas	Changed function name: select-current-crews-mod -> select-current-modif-request
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun select-current-modif-request ()
  (unless (all-modif-requests)
    (load-modifications))
  (let ((mod (select-modif-request)))
    (when mod
      (set-current-modif-request mod))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; CREATING AND SELECTING A MODIFICATION
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;MAKE-MODIFICATION-REQUEST
;;;Description
;;;	Creates a modification request
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{args} is a plist with arguments to create an instance.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Passou a informar o big-brother...
;;;	97/08/29	A. Frazao	Added args
;;;	09/02/10	J. P. Varandas	Change function name: make-crews-modification -> make-modification-request
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun make-modification-request (&rest args)
  (unless *all-modif-requests*
    (load-modifications))
  (let ((mod (apply 'make-modif-request :name (read-mod-name) args)))
    (add-modif-request mod)
    (save-modifications)
    (set-current-modif-request mod)
    (inform-big-brother "NEW_M" mod)))

;;;-----------------------------------------------------------------------------
;;;MAKE-MODIFICATION-REQUEST2
;;;Description
;;;	Creates a modification without selecting it.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{args} is a plist with arguments to create an instance.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Change function name: make-crews-modification2 -> make-modification-request2
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun make-modification-request2 (&rest args)
  (unless *all-modif-requests*
    (load-modifications))
  (let ((mod (apply 'make-modif-request :name (read-mod-name) args)))
    (add-modif-request mod)
    (save-modifications)))


;;;-----------------------------------------------------------------------------
;;;BROWSE-ITEM<
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	Created
;;;	98/11/04	A. Frazao	Uses new menu implementation
;;;-----------------------------------------------------------------------------
(defun browse-item< (item1 item2)
  (string< (sc-menu-item-name item1) (sc-menu-item-name item2)))

;;;-----------------------------------------------------------------------------
;;;BROWSE-DIRECTORIES-ITEMS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	Created
;;;	98/11/04	A. Frazao	Uses new menu implementation
;;;-----------------------------------------------------------------------------
(defun browse-directories-items (dir)
  (let ((items nil))
    (dolist (name (directory-files dir))
      (if (or (not (char-equal (elt name 0) 46))
	      (equal name ".."))
	  (let ((new-dir (format "%s/%s" dir name)))
	    (if (file-directory-p new-dir)
		(push (sc-make-menu-item (format "%s >" name) new-dir) items)))))
    (sort items 'browse-item<)))

;;;-----------------------------------------------------------------------------
;;;BROWSE-FILES-ITEMS
;;;Description
;;;	Creates menu items with the files of the given directory
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a \elem{pathname}
;;;		
;;;	\return-types
;;;		A list of \elem{pathname}
;;;		
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	Created
;;;	98/11/04	A. Frazao	Uses new menu implementation
;;;	99/02/26	A. Frazao	Calls SC-CREWS-FILE-P
;;;	09/02/10	J. P. Varandas	sc-crews-file-p -> sc-rplan-file-p
;;;-----------------------------------------------------------------------------
(defun browse-files-items (dir)
  (let ((items nil))
    (dolist (name (directory-files dir))
      (if (sc-rplan-file-p name) 
	  (let ((file (format "%s/%s" dir name)))
	    (if (not (file-directory-p file))
		(push (sc-make-menu-item name file) items)))))
    (sort items 'browse-item<)))

;;;-----------------------------------------------------------------------------
;;;BROWSE-SELECT-FILE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	Created
;;;	98/11/04	A. Frazao	Uses new menu implementation
;;;-----------------------------------------------------------------------------
(defun browse-select-file (path)
  (let ((items (append (browse-directories-items path) (browse-files-items path))))
    (when items
      (let ((result (sc-popup-menu *current-x-arg* "Select" items nil 40)))
	(when result
	  (if (file-directory-p result)
	      (browse-select-file result)
	    result))))))

;;;-----------------------------------------------------------------------------
;;;COPY-EDIT-ORIGINAL-SOURCE-FILE
;;;Description
;;;	Copy a choosen original file into the souyrce directory
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	Created
;;;	98/11/04	A. Frazao	Uses MY-MAKE-DIR and MY-COPY-FILE
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun copy-edit-original-source-file ()
  (let ((mod (current-modif-request)))
    (when mod
      (let* ((org-dir (modif-request-org-dir mod))
	     (src-dir (modif-request-src-dir mod))
	     (org-file (browse-select-file org-dir)))
	(when (and org-file
		   (string-match (format "^%s/" org-dir) org-file))
	  (let* ((file (substring org-file (match-end 0)))
		 (dir (format "%s/%s" src-dir (file-name-directory file)))
		 (src-file (format "%s/%s" src-dir file))
		 (title (if (file-exists-p src-file)
			    "Copy the ORIGINAL file to the SOURCE dir? (file exists)"
			    "Copy the ORIGINAL file to the SOURCE dir? (file does not exist)")))
	    (when (x-beep-confirm title
				  org-file
				  "to"
				  src-file)
	      (when (not (file-directory-p dir))
		(sc-make-directory dir))
	      (sc-copy-file org-file src-file)
	      (switch-to-buffer (find-file-noselect src-file))
	      )))))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; DELETING A MODIFICATION
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;DELETE-MODIFICATION-REQUEST
;;;Description
;;;	Allow the user to select and delete a modification.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Passou a informar o big-brother...
;;;	99/08/31	J. P. Varandas	If the modifications were not yet been loaded
;;;					then loads them
;;;	08/02/22	Tom Weissmann	Ask the user for confirmation (POA 12704.0).
;;;	09/02/10	J. P. Varandas	Change function name: delete-crews-modification -> delete-modification-request
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun delete-modification-request ()
  (unless (all-modif-requests) (load-modifications))
  (let ((mod (select-modif-request "Select modification to delete")))
    (when (and mod
	       (sc-y-or-n-p (format "Delete modification %s?" (modif-request-name mod))))
      (delete-modif-request mod)
      (save-modifications)
      (inform-big-brother "DEL_M" mod))))


;;;-----------------------------------------------------------------------------
;;;MEASURE-MOD-FILE
;;;Description
;;;	Measure the documentation statistics of the current file.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;	05/10/13	J. P. Varandas	Changed call of function 'measure-file'
;;;-----------------------------------------------------------------------------
(defun measure-mod-file ()
  (let ((filename (buffer-file-name (current-buffer)))
	(old-control-by-file %control-by-file%)
	(old-control-by-element %control-by-element%)
	(old-stat-pretty-print %stat-pretty-print%))
    (when filename
      (get-result-buffer)
      (setq %control-by-file% t)
      (setq %control-by-element% t)
      (setq %stat-pretty-print% t)
      (measure-file filename (make-doc-stat))
      (show-result-buffer)
      (setq %control-by-file% old-control-by-file)
      (setq %control-by-element% old-control-by-element)
      (setq %stat-pretty-print% old-stat-pretty-print))))

;;;-----------------------------------------------------------------------------
;;;MEASURE-MOD-FILE-CHECK-DATE
;;;Description
;;;	Measure the documentation statistics of the changed elements of the current file
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;	05/10/13	J. P. Varandas	Changed call of function 'measure-file'
;;;-----------------------------------------------------------------------------
(defun measure-mod-file-check-date ()
  (let ((filename (buffer-file-name (current-buffer)))
	(old-control-by-file %control-by-file%)
	(old-control-by-element %control-by-element%)
	(old-sample-base-date %sample-base-date%)
	(old-stat-pretty-print %stat-pretty-print%))
    (when filename
      (get-result-buffer)
      (setq %control-by-file% t)
      (setq %control-by-element% t)
      (setq %sample-base-date% (get.mod.date))
      (setq %stat-pretty-print% t)
      (measure-file filename (make-doc-stat))
      (show-result-buffer)
      (setq %control-by-file% old-control-by-file)
      (setq %control-by-element% old-control-by-element)
      (setq %sample-base-date% old-sample-base-date)
      (setq %stat-pretty-print% old-stat-pretty-print))))


;;;-----------------------------------------------------------------------------
;;;MEASURE-SRC-FILES
;;;Description
;;;	Measure the documentation statistics of the source files of the current modification.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun measure-src-files ()
  (let ((old-control-by-file %control-by-file%)
	(old-control-by-element %control-by-element%)
	(old-stat-pretty-print %stat-pretty-print%)
	(stat (make-doc-stat)))
    (get-result-buffer)
    (setq %control-by-file% t)
    (setq %control-by-element% t)
    (setq %stat-pretty-print% t)
    (let* ((mod (current-modif-request))
	   (src-dir (modif-request-src-dir mod)))
      (dolist (file (modif-request-files mod))
	(measure-file (format "%s/%s" src-dir (file-src file)) stat))
      (insert 10 10 (format "Totals for  modification: %s" (modif-request-name mod)) 10)
      (print-stat stat)
      (show-result-buffer))
    (setq %control-by-file% old-control-by-file)
    (setq %control-by-element% old-control-by-element)
    (setq %stat-pretty-print% old-stat-pretty-print)))

;;;-----------------------------------------------------------------------------
;;;MEASURE-MOD-FILES
;;;Description
;;;	Measure the documentation statistics of the modification files of the 
;;;	current modification.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun measure-mod-files ()
  (let ((old-control-by-file %control-by-file%)
	(old-control-by-element %control-by-element%)
	(old-stat-pretty-print %stat-pretty-print%)
	(stat (make-doc-stat)))
    (get-result-buffer)
    (setq %control-by-file% t)
    (setq %control-by-element% t)
    (setq %stat-pretty-print% t)
    (let* ((mod (current-modif-request))
	   (mod-dir (modif-request-dir mod)))
      (dolist (file (modif-request-files mod))
	(measure-file (format "%s/%s" mod-dir (file-mod file)) stat))
      (insert 10 10 (format "Totals for  modification: %s" (modif-request-name mod)) 10)
      (print-stat stat)
      (show-result-buffer))
    (setq %control-by-file% old-control-by-file)
    (setq %control-by-element% old-control-by-element)
    (setq %stat-pretty-print% old-stat-pretty-print)))

;;;-----------------------------------------------------------------------------
;;;MEASURE-MOD-FILES-CHECK-DATE
;;;Description
;;;	Measure the documentation statistics of the changed elements of the 
;;;	current modification
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun measure-mod-files-check-date ()
  (let ((old-control-by-file %control-by-file%)
	(old-control-by-element %control-by-element%)
	(old-sample-base-date %sample-base-date%)
	(old-stat-pretty-print %stat-pretty-print%)
	(stat (make-doc-stat)))
    (get-result-buffer)
    (setq %control-by-file% t)
    (setq %control-by-element% t)
    (setq %sample-base-date% (get.mod.date))
    (setq %stat-pretty-print% t)
    (let* ((mod (current-modif-request))
	   (src-dir (modif-request-src-dir mod)))
      (dolist (file (modif-request-files mod))
	(measure-file (format "%s/%s" src-dir (file-src file)) stat))
      (insert 10 10 (format "Totals for  modification: %s" (modif-request-name mod)) 10)
      (print-stat stat)
      (show-result-buffer))
    (setq %control-by-file% old-control-by-file)
    (setq %control-by-element% old-control-by-element)
    (setq %sample-base-date% old-sample-base-date)
    (setq %stat-pretty-print% old-stat-pretty-print)))

;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; MENUS
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SEARCH-DEFINITION-IN-SOURCE-FILE
;;;Description
;;;	Searches a definition in an original file referenced in a patch-
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	04/06/22	A. Frazao	Created
;;;	09/09/23	P. Madeira	Set `start' variable in a let
;;;-----------------------------------------------------------------------------
(defun search-definition-in-source-file ()
  (let ((definition (save-excursion (get-definition-id))))
    (if definition
	(let ((file nil))
	  (save-excursion
	    (when (search-backward ";;; File: " nil t)
	      (let ((start (match-end 0)))
		(end-of-line)
		(setf file (format "%s/%s" *default-src-dir* (buffer-substring start (point)))))))
	  (when (and file
		     (file-exists-p file))
	    (switch-to-buffer-other-window (find-file-noselect file))
	    (beginning-of-buffer)
	    (search-forward (format ";;;%s" definition) nil t))))))



;;;-----------------------------------------------------------------------------
;;;POSITION-DEFINITION
;;;Description
;;;	Positions starting at a definition.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	96/08/22	A. Frazao	Created
;;;	98/11/04	A. Frazao	Uses new menu implementation
;;;	04/06/22	A. Frazao	Integrated buffer-definitions
;;;-----------------------------------------------------------------------------
(defun position-definition ()
  (let ((items nil))
    (save-excursion
      (beginning-of-buffer)
      (let ((last-point (point))
	    (start-point (+ (point) 1)))
	(while (not (= last-point start-point))
	  (setf last-point start-point)
	  (forward-sexp)
	  (let ((end-point (point)))
	    (save-excursion
	      (beginning-of-definition)
	      (setf start-point (point))
	      (unless (or (= last-point start-point)
			  (looking-at "(in-package"))
		(push (sc-make-menu-item (get-definition-id) end-point) items)))))
	(setf items (nreverse items))))
    (if items
	(let ((choice (sc-popup-menu *current-x-arg* "Select definition" items nil 40)))
	  (when choice
	    (goto-char choice)
	    (beginning-of-definition)))
      (beep))))


;;;-----------------------------------------------------------------------------
;;;SHOW-MOD-HELP
;;;Description
;;;	Displays a help string in buffer *modif-request-help*
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{string} is a \emph{string}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defun show-mod-help (string)
  (let ((old-buf (current-buffer))
	(buf (get-buffer-create "*modif-request-help*")))
    (set-buffer buf)
    (erase-buffer)
    (let ((start 0))
      (do ((end (position 10 string :start start) (position 10 string :start start)))
	  ((null end)
	   (insert (subseq string start) 10))
	(insert (subseq string start end) 10)
	(setf start (1+ end))))
    (beginning-of-buffer)
    (switch-to-buffer-other-window buf)
    (pop-to-buffer old-buf)))



;;;-----------------------------------------------------------------------------
;;;DEFINITION-OPERATIONS-ITEMS-HELP
;;;Description
;;;	Display help for \elem{definition-operations-items}
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	04/06/22	A. Frazao	Added "Search in source"
;;;					Changed "Position" to "Definitions"
;;;	05/03/21	J. P. Varandas	Changed help entry for "Set patch"
;;;-----------------------------------------------------------------------------
(defun definition-operations-items-help ()
  (show-mod-help "<Definition expression>
This menu appears only if the Current Modification is set, the current
buffer file is a Lisp file (ends with '.lisp' '.cl' '.bil' or '.dic') and the 
system identifies a lisp definition. The definition expression (the one that 
is used in the headers) appears as the title of the menu.
--------------------------------------------------------------------------------
Set Changed
	Sets the definition changed and updates the definition header and the
	file header. If there is a selected modification, sets the file modified
	(if not already in the modification).
Set Added
	Sets the definition created and updates the definition header and the
	file header. If there is a selected modification, sets the file modified
	(if not already in the modification).
Set New
	Similar to \"Set Added\" but don't write on the file header (to be used on
	new files)
Set Copy
	Sets the definition created and updates the definition header and the
	file header. If there is a selected modification, sets the file modified
	(if not already in the modification). Preserves the defintion comment.
	(Similar to \"Set Added\").
Set Deleted
	Sets the definition deleted, removes the definition and its header, and
	updates the file header. If there is a selected modification, sets the
	file modified (if not already in the modification).
Set Moved
	Similar to \"Set Deleted\" but writes on the file header that the definition
	was moved to another file.
Set Update
	Writes on the file header that the definition was updated adding an arrow
	in front of the defintion name (eg, change of defintion name). Before updating
	the defintion the user should make \"Set Update\" after changes the defintion
	and call \"Set Added\". Then he should changed the file header and cut/paste
	the added definiton in front of the respective arrow. If there is a selected
	modification, sets the file modified (if not already in the modification).
Set Comment
	Sets the definition commented, comments the definition and updates the file
	header. If there is a selected modification, sets the file modified
	(if not already in the modification).
Set Header
	Inserts the definition header to perform documentation. If there is a selected
	modification, sets the file modified  (if not already in the modification).
Set Patch
	If there is no patch associated with the selected modification it creates
	one. Appends the definition in the patch file with the file reference and
	the package CAUTION: it uses the first (in-package ...) that appears in the
	source file. If the package is changed before the definition in the source
	file, it will not be correct in the patch file.
	All definitions are position groupped by the origin file. The newest 
	entries are position in the bottom of the group of the respective file. 
	If no file reference is already on the patch the system will position it 
	at the end of the patch.
	Adds in the patch header a reference to the copied definition grouped by
	the respecitive origin file.
--------------------------------------------------------------------------------
Search in source
	Searches the definition in the original file. Uses the File information
	before the definition in a patch.
Definitions
	Searches for all definitions in the file. Presents a menu with the names
	of all the definitions found. If the user selects one definition, it jumps
	in the buffer to the start of the definition.
--------------------------------------------------------------------------------
Set history label
	When performing several changes with the same history comment the user
	can store that comment and in all \"Set Changed\" calls that comment will
	be placed in the proper place.
--------------------------------------------------------------------------------
Help
	Displays this text.
--------------------------------------------------------------------------------"))



;;;-----------------------------------------------------------------------------
;;;DEFINITION-OPERATIONS-ITEMS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	96/08/22	A. Frazao	Added position
;;;	97/07/25	A. Frazao	Added test for UNIX-MODE
;;;	98/11/04	A. Frazao	Uses new menu implementation
;;;					Added Set Patch
;;;	99/08/31	J. P. Varandas	Added 'Set New', 'Set Copy', 'Set Moved', 
;;;					'Set Update', 'Set Comment', 'Set Header', 
;;;					and 'Set history label'
;;;	00/01/31	J. P. Varandas	Added "Set Doc Update"
;;;	04/03/12	A. Frazao	Added "Help"
;;;	04/06/22	A. Frazao	Added "Search in source"
;;;					Changed "Position" to "Definitions"
;;;-----------------------------------------------------------------------------
(defvar definition-operations-items
    (list (sc-make-menu-item "Set Changed"    'set-definition-changed)
	  (sc-make-menu-item "Set Added"      'set-definition-added)
	  (sc-make-menu-item "Set New"        'set-definition-new)
	  (sc-make-menu-item "Set Copy"       'set-definition-copy)
	  (sc-make-menu-item "Set Deleted"    'set-definition-deleted)
	  (sc-make-menu-item "Set Moved"      'set-definition-moved)
	  (sc-make-menu-item "Set Update"     'set-definition-update)
	  (sc-make-menu-item "Set Comment"    'set-definition-comment)
	  (sc-make-menu-item "Set Doc Update" 'set-definition-doc-update)
	  (sc-make-menu-item "Set Header"     'set-definition-header)
	  (sc-make-menu-item "Set Patch"      'set-definition-to-patch)
	  ""
	  (sc-make-menu-item "Search in source"  'search-definition-in-source-file)
	  (sc-make-menu-item "Definitions"       'position-definition)
	  ""
	  (sc-make-menu-item "Set history label" 'set-history-label)
	  ""
	  (sc-make-menu-item "Help"              'definition-operations-items-help)))

;;;-----------------------------------------------------------------------------
;;;MOD-DEFINITION-OPERATIONS-ITEMS-HELP
;;;Description
;;;	Display help for \elem{mod-definition-operations-items}.
;;;	
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	04/06/22	A. Frazao	Added "Search in source"
;;;					Changed "Position" to "Definitions"
;;;	05/03/21	J. P. Varandas	Changed help entry for "Set patch"
;;;-----------------------------------------------------------------------------
(defun mod-definition-operations-items-help ()
  (show-mod-help "<Definition expression>
This menu appears only if the Current Modification is set, the current
buffer file is a Lisp file (ends with '.lisp' '.cl' '.bil' or '.dic') and the 
system identifies a lisp definition. The definition expression (the one that 
is used in the headers) appears as the title of the menu.

When the definition was already marked as changed or added then the following
menu appears:
--------------------------------------------------------------------------------
Set Copy
	Sets the definition created and updates the definition header and the
	file header. If there is a selected modification, sets the file modified
	(if not already in the modification). Preserves the defintion comment.
	(Similar to \"Set Added\").
Set Patch
	If there is no patch associated with the selected modification it creates
	one. Appends the definition in the patch file with the file reference and
	the package CAUTION: it uses the first (in-package ...) that appears in the
	source file. If the package is changed before the definition in the source
	file, it will not be correct in the patch file.
	All definitions are position groupped by the origin file. The newest 
	entries are position in the bottom of the group of the respective file. 
	If no file reference is already on the patch the system will position it 
	at the end of the patch.
	Adds in the patch header a reference to the copied definition grouped by
	the respecitive origin file.
Undo Modification
	Cleans the change indication on the file header and on the definition header.
--------------------------------------------------------------------------------
Search in source
	Searches the definition in the original file. Uses the File information
	before the definition in a patch.
Definitions
	Searches for all definitions in the file. Presents a menu with the names
	of all the definitions found. If the user selects one definition, it jumps
	in the buffer to the start of the definition.
--------------------------------------------------------------------------------
Set history label
	When performing several changes with the same history comment the user
	can store that comment and in all \"Set Changed\" calls that comment will
	be placed in the proper place.
--------------------------------------------------------------------------------
Help
	Displays this text.
--------------------------------------------------------------------------------"))


;;;-----------------------------------------------------------------------------
;;;MOD-DEFINITION-OPERATIONS-ITEMS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	04/03/12	A. Frazao	Added "Help"
;;;	04/06/30	A. Frazao	Added "Search in source"
;;;					Changed "Position" to "Definitions"
;;;-----------------------------------------------------------------------------
(defvar mod-definition-operations-items
    (list (sc-make-menu-item "Set Copy"          'set-definition-copy)
	  (sc-make-menu-item "Set Patch"         'set-definition-to-patch)
	  (sc-make-menu-item "Undo Modification" 'set-definition-undo)
	  ""
	  (sc-make-menu-item "Search in source"  'search-definition-in-source-file)
	  (sc-make-menu-item "Definitions"       'position-definition)
	  ""
	  (sc-make-menu-item "Set history label" 'set-history-label)
	  ""
	  (sc-make-menu-item "Help"              'mod-definition-operations-items-help)))

;;;-----------------------------------------------------------------------------
;;;KEYWORD-OPERATIONS-ITEMS-HELP
;;;Description
;;;	Display help for \elem{keyword-operations-items}.
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	05/03/21	J. P. Varandas	Changed help entry for "Set keyword patch"
;;;-----------------------------------------------------------------------------
(defun keyword-operations-items-help ()
  (show-mod-help "<Definition expression>
This menu appears only if the Current Modification is set, the current
buffer file is a Lisp file (ends with '.lisp' '.cl' '.bil' or '.dic') and the 
system identifies a lisp definition. The definition expression (the one that 
is used in the headers) appears as the title of the menu.

When the file is a dictionary file the following menu appears:
--------------------------------------------------------------------------------
Set Add keyword
	Adds the keyword where the cursor is to the list of new keywords in the
	file header (avoids the manual building of this list).
Set Remove keyword
	Adds the keyword where the cursor is to the list of removed keywords in
	the file header (avoids the manual building of this list). It also erases
	the keyword and all its translations.
Set Change keyword
	Adds the keyword where the cursor is to the list of changed keywords in
	the file header (avoids the manual building of this list).
Set keyword Patch
	If there is no patch associated with the selected modification it creates
	one. Appends a call to the function 'add.translation' with the keyword
	in the patch file with the file reference and the package 'traducao'
	All translation keywords are position just after the patch header 
	(the newest entries are position in the bottom of the group).
--------------------------------------------------------------------------------
Help
	Displays this text.
--------------------------------------------------------------------------------"))


;;;-----------------------------------------------------------------------------
;;;KEYWORD-OPERATIONS-ITEMS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	99/09/01	J. P. Varandas	Added "Set keyword Patch"
;;;	04/03/12	A. Frazao	Added "Help"
;;;-----------------------------------------------------------------------------
(defvar keyword-operations-items
    (list (sc-make-menu-item "Set Add keyword"    'set-keyword-added)
	  (sc-make-menu-item "Set Remove keyword" 'set-keyword-deleted)
	  (sc-make-menu-item "Set Change keyword" 'set-keyword-change)
	  (sc-make-menu-item "Set keyword Patch"  'set-keyword-to-patch)
	  ""
	  (sc-make-menu-item "Help"               'keyword-operations-items-help)
	  ))

;;;-----------------------------------------------------------------------------
;;;EXPORTATION-OPERATIONS-ITEMS-HELP
;;;Description
;;;	Display help for \elem{exportation-operations-items}.
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	04/06/22	A. Frazao	Added "Search in source"
;;;					Changed "Position" to "Definitions"
;;;	05/03/21	J. P. Varandas	Changed help entry for "Set Exportation patch"
;;;-----------------------------------------------------------------------------
(defun exportation-operations-items-help ()
  (show-mod-help "<Definition expression>
This menu appears only if the Current Modification is set, the current
buffer file is a Lisp file (ends with '.lisp' '.cl' '.bil' or '.dic') and the 
system identifies a lisp definition. The definition expression (the one that 
is used in the headers) appears as the title of the menu.

When the definition is a 'DEFPACKAGE' ou 'EXPORTATIONS' the following menu appears:
--------------------------------------------------------------------------------
Set Add exportation
	Adds the exported symbol where the cursor is to the list of exported symbols
	in the definition header (avoids the manual building of this list). Sets
	also the defintion as changed.
Set Remove exportation
	Adds the exported symbol where the cursor is to the list of removed exported
	symbols in the definition header (avoids the manual building of this list).
	Erase the line where the cursor is (efective remove of the symbol).
Set Exportation Patch
	If there is no patch associated with the selected modification it creates
	one. Appends the exportation of the symbol in the patch file with the file
	reference and the package CAUTION: it uses the package defined on the
	'defpackage' defintion.
	All symbol exportation are position just after the group of translation 
	keywords and just before the first of the package from which the symbols
	are exported.
--------------------------------------------------------------------------------
Set Changed
	Sets the definition changed and updates the definition header and the
	file header. If there is a selected modification, sets the file modified
	(if not already in the modification).
Set Added
	Sets the definition created and updates the definition header and the
	file header. If there is a selected modification, sets the file modified
	(if not already in the modification).
Set New
	Similar to \"Set Added\" but don't write on the file header (to be used on
	new files)
Set Deleted
	Sets the definition deleted, removes the definition and its header, and
	updates the file header. If there is a selected modification, sets the
	file modified (if not already in the modification).
Set Patch
	If there is no patch associated with the selected modification it creates
	one. Appends the definition in the patch file with the file reference and
	the package CAUTION: it uses the first (in-package ...) that appears in the
	source file. If the package is changed before the definition in the source
	file, it will not be correct in the patch file.
--------------------------------------------------------------------------------
Search in source
	Searches the definition in the original file. Uses the File information
	before the definition in a patch.
Definitions
	Searches for all definitions in the file. Presents a menu with the names
	of all the definitions found. If the user selects one definition, it jumps
	in the buffer to the start of the definition.
--------------------------------------------------------------------------------
Help
	Displays this text.
--------------------------------------------------------------------------------"))


;;;-----------------------------------------------------------------------------
;;;EXPORTATION-OPERATIONS-ITEMS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	99/09/01	J. P. Varandas	Added "Set Exportation Patch" and "Set patch"
;;;	04/03/12	A. Frazao	Added "Help"
;;;	04/06/22	A. Frazao	Added "Search in source"
;;;					Changed "Position" to "Definitions"
;;;-----------------------------------------------------------------------------
(defvar exportation-operations-items
    (list (sc-make-menu-item "Set Add exportation"    'set-export-added)
	  (sc-make-menu-item "Set Remove exportation" 'set-export-deleted)
	  (sc-make-menu-item "Set Exportation Patch"  'set-export-to-patch)
	  ""
	  (sc-make-menu-item "Set Changed"            'set-definition-changed)
	  (sc-make-menu-item "Set Added"              'set-definition-added)
	  (sc-make-menu-item "Set New"                'set-definition-new)
	  (sc-make-menu-item "Set Deleted"            'set-definition-deleted)
	  (sc-make-menu-item "Set Patch"              'set-definition-to-patch)
	  ""
	  (sc-make-menu-item "Search in source"       'search-definition-in-source-file)
	  (sc-make-menu-item "Definitions"            'position-definition)
	  ""
	  (sc-make-menu-item "Help"                   'exportation-operations-items-help)))

;;;-----------------------------------------------------------------------------
;;;SOURCE-OPERATIONS-ITEMS-HELP
;;;Description
;;;	Display help for \elem{source-operations-items}.
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun source-operations-items-help ()
  (show-mod-help "<Definition expression>
Not implemented yet.
--------------------------------------------------------------------------------
Help
	Displays this text.
--------------------------------------------------------------------------------"))


;;;-----------------------------------------------------------------------------
;;;SOURCE-OPERATIONS-ITEMS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/09/01	J. P. Varandas	Created
;;;	04/03/12	A. Frazao	Added "Help"
;;;	04/06/22	A. Frazao	Added "Search in source"
;;;					Changed "Position" to "Definitions"
;;;-----------------------------------------------------------------------------
(defvar source-operations-items
    (list (sc-make-menu-item "Set Add system source"    'set-source-added)
	  (sc-make-menu-item "Set Remove system source" 'set-source-deleted)
	  ""
	  (sc-make-menu-item "Search in source"         'search-definition-in-source-file)
	  (sc-make-menu-item "Definitions"              'position-definition)
	  ""
	  (sc-make-menu-item "Help"                     'source-operations-items-help)))

;;;-----------------------------------------------------------------------------
;;;DEFINITION-OPERATIONS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/01/29	A. Frazao	Added BAT-MODE
;;;	99/08/31	J. P. Varandas	Diferentes comportamentos consoante o contexto
;;;	99/09/01	J. P. Varandas	Different label for the keywords menu 
;;;	99/09/01	J. P. Varandas	Added behaviour for "System Source"
;;;	01/07/04	J. P. Varandas	Os dicionarios sao independentes da definição
;;;	05/12/07	A. Frazao	Protect from errors
;;;	09/09/23	P. Madeira	`cons' -> `sc-make-menu-item'
;;;-----------------------------------------------------------------------------
(defun definition-operations ()
  (if (and (buffer-file-name (current-buffer))
	   (file-mode)
	   (not (= (file-mode) UNIX-MODE))
	   (not (= (file-mode) BAT-MODE)))
      (if (= (file-mode) DIC-MODE)
	  (list (sc-make-menu-item "KEYWORD" keyword-operations-items))
	  (ignore-errors
	   (let ((definition (save-excursion (get-definition-id))))
	     (when definition
	       (if (or (string-match "^PACKAGE" definition)
		       (string-match "^EXPORTATIONS" definition)) 
		   (list (sc-make-menu-item definition exportation-operations-items))
		   (if (string-match "^System Source" definition) 
		       (list (sc-make-menu-item definition source-operations-items))
		       (if (has-modified2)
			   (list (sc-make-menu-item definition mod-definition-operations-items))
			   (list (sc-make-menu-item definition definition-operations-items)))))))))))


;;;-----------------------------------------------------------------------------
;;;FILE-OPERATIONS-ITEMS-HELP
;;;Description
;;;	Display help for \elem{file-operations-items}.
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	04/04/23	A. Frazao	Updated Set Patch
;;;	05/03/21	J. P. Varandas	Added
;;;					  "Append definitions to patch"
;;;					Deleted
;;;					  "Merge with 6-2-0"
;;;					Replaced
;;;					  "Merge with 6-3-0" -> "Merge with 6-8-0"
;;;					  "Merge with 6-4-0" -> "Merge with 6-9-0"
;;;	06/04/11	J. P. Varandas	Removed
;;;					  "Merge with 6-8-0"
;;;					  "Merge with 6-9-0"
;;;					Added
;;;					  "Merge with old version"
;;;-----------------------------------------------------------------------------
(defun file-operations-items-help ()
  (show-mod-help "FILE: <Buffer file name>
This menu appears only if the Current Modification is set and the current
buffer file is a Lisp file (ends with '.lisp' '.cl' '.bil' or '.dic').
It allows the operations in the current buffer file. Warnings are messaged
if the operation is incompatible with the file status in the Original
Directory or if the file is already referenced in any modification.
--------------------------------------------------------------------------------
Set Changed
	Adds the file to the Current Modification and is marked as changed.
	The corresponding file must exist in the Original Directory.
Set Added
	Adds the file to the Current Modification and is marked as new.
	The corresponding file must not exist in the Original Directory.
Set Deleted
	Adds the file to the Current Modification and is marked to delete.
	The corresponding file must exist in the Original Directory.
Set Patch
	Adds the file to the Current Modification as its patch. If the file
	is a numbered patch increments the version to name the file. Usefull
 	to make new versions of patches.
--------------------------------------------------------------------------------
Remove
	Removes the file from the Current Modification.
Change modification
	Moves the current file to another modification. A menu is presented to
	select the new modification where the file should be moved to.
Create modification
	Creates a modification for the current file removing it from a previous
	modification if any.
Select modification
	Selects the modification of the current file.
--------------------------------------------------------------------------------
Measure documentation
	Measure the documentation statistics of the current file
Measure changed documentation
	Measure the documentation statistics of the changed elements of the current
	file
--------------------------------------------------------------------------------
Edit original file
	Opens a new window with the original file corresponding to the current
	buffer (it should be a source file). This dependends on the current
	modification.
Edit source file
	Opens a new window with the source file corresponding to the current buffer
	(it should be a original file). This dependends on the current modification
Edit backup
	Opens a new window with the backup file corresponding to the current buffer.

--------------------------------------------------------------------------------
Merge with original
	Runs EDIFF-FILES (merge) for the current file and the corresponding original
	file.
Merge with old version
	Runs EDIFF-FILES (merge) for the current file and an old version of it selected by the user
Merge with modification
	Runs EDIFF-FILES (merge) for the current file and the corresponding modification
	file.
--------------------------------------------------------------------------------
Get from original
	Replaces the current file by the corresponding original file.
Get from modification
	Replaces the current file by the corresponding modification file.
--------------------------------------------------------------------------------

Add file entry
	Adds an entre (date and author) in the file header.
Add file header
	Adds the modification header to the file.
--------------------------------------------------------------------------------
Append definitions to patch
	Copies all the changed definitions of a modification into the patch. 
	CAUTION: This should be used carefully. Sometimes additional work
	must be done. However, for simple modifications it may be very helpfull.
	- It will include systems definitions (if changed)
	- It will not include changes in dictionary keywords
	- It does not include exportation symbols, but the whole changed
	  package definitions.
--------------------------------------------------------------------------------
Help
	Displays this text.
--------------------------------------------------------------------------------"))


;;;-----------------------------------------------------------------------------
;;;FILE-OPERATIONS-ITEMS
;;;Description
;;;	Returns a list of menu items for the operations over the current file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Uses new menu implementation
;;;	99/08/31	J. P. Varandas	Added three more options
;;;					  "Edit original file"
;;;					  "Edit source file"
;;;					  "Edit backup"
;;;	00/03/24	A. Frazao	Added options
;;;					  "Merge with original"
;;;					  "Merge with modification"
;;;	00/06/06	A. Frazao	Added option
;;;					  "Change modification"
;;;	01/07/04	J. P. Varandas	Added options
;;;					  "Set Patch"
;;;					  "Get from original"
;;;					  "Get from modification"
;;;	02/06/07	J. P. Varandas	Added options
;;;					  "Create modification"
;;;					  "Select modification"
;;;					  "Measure documentation"
;;;					  "Measure changed documentation"
;;;					  "Merge with 6-2-0"
;;;					  "Merge with 6-3-0"
;;;					  "Merge with 6-4-0"
;;;	04/03/12	A. Frazao	Added "Help"
;;;	05/03/21	J. P. Varandas	Added
;;;					  "Append definitions to patch"
;;;					Deleted
;;;					  "Merge with 6-2-0"
;;;					Replaced
;;;					  "Merge with 6-3-0" -> "Merge with 6-8-0"
;;;					  "Merge with 6-4-0" -> "Merge with 6-9-0"
;;;	06/04/11	J. P. Varandas	Removed
;;;					  "Merge with 6-8-0"
;;;					  "Merge with 6-9-0"
;;;					Added
;;;					  "Merge with old version"
;;;	09/02/10	J. P. Varandas	Changed function names:
;;;					  'make-crews-mod-with-file -> 'make-modif-request-with-file
;;;					  'select-crews-mod-from-file -> 'select-modif-request-from-file
;;;-----------------------------------------------------------------------------
(defvar file-operations-items
    (list (sc-make-menu-item "Set Changed"            'set-mod-file-changed)
	  (sc-make-menu-item "Set Added"              'set-mod-file-new)
	  (sc-make-menu-item "Set Deleted"            'set-mod-file-del)
	  (sc-make-menu-item "Set Patch"              'set-mod-file-to-patch)
	  ""
	  (sc-make-menu-item "Remove"                 'rem-mod-file)
	  (sc-make-menu-item "Change modification"    'move-mod-file)
	  (sc-make-menu-item "Create modification"    'make-modif-request-with-file)
	  (sc-make-menu-item "Select modification"    'select-modif-request-from-file)
	  ""
	  (sc-make-menu-item "Measure documentation"  'measure-mod-file)
	  (sc-make-menu-item "Measure changed documentation"  'measure-mod-file-check-date)
	  ""
	  (sc-make-menu-item "Edit original file"     'edit-original-file)
	  (sc-make-menu-item "Edit source file"       'edit-source-file)
	  (sc-make-menu-item "Edit backup"            'edit-backup-file)
	  ""
	  (sc-make-menu-item "Merge with original"     'ediff-file-with-original)
	  (sc-make-menu-item "Merge with old version"  'ediff-file-with-old-version)
	  (sc-make-menu-item "Merge with modification" 'ediff-file-with-modification)
	  ""
	  (sc-make-menu-item "Get from original"       'get-file-from-original)
	  (sc-make-menu-item "Get from modification"   'get-file-from-modification)
	  ""
	  (sc-make-menu-item "Add header entry"        'add-header-change-entry)
	  (sc-make-menu-item "Add file header"         'add-mod-file-header)
	  ""
	  (sc-make-menu-item "Append definitions to patch" 'append-definitions-patch)
	  ""
	  (sc-make-menu-item "Help"                    'file-operations-items-help)))

;;;-----------------------------------------------------------------------------
;;;FILE-OPERATIONS
;;;Description
;;;	Presents a menu with the operations over the current file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of menu items
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;	09/09/23	P. Madeira	`cons' -> `sc-make-menu-item'
;;;-----------------------------------------------------------------------------
(defun file-operations ()
  (if (and (current-modif-request)
	   (buffer-file-name (current-buffer))
	   (file-mode))
      (list (sc-make-menu-item (format "FILE: %s" (buffer-name (current-buffer)))
			       file-operations-items))))

;;;-----------------------------------------------------------------------------
;;;MOD-OPERATIONS-ITEMS1-HELP
;;;Description
;;;	Display help for \elem{mod-operations-items1}.
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	05/03/21	J. P. Varandas	Added
;;;					  "Edit all source file"
;;;	05/04/08	Rui Mestre	Added
;;;					  "Find patch"
;;;	05/10/13	J. P. Varandas	Removed
;;;					  "Make Mail File"
;;;					  "Find patch"
;;;					Added
;;;					  Append definitions to patch
;;;					  Edit mail file
;;;					  Split by patches
;;;	06/04/11	J. P. Varandas	Added
;;;					  "Set patch for version"
;;;					  "Set modification version"
;;;	06/05/29	A. Frazao	Added
;;;					  "Edit mod mail file"
;;;	06/05/29	A. Frazao	Removed duplicated "Edit mod mail file"
;;;-----------------------------------------------------------------------------
(defun mod-operations-items1-help ()
  (show-mod-help "OPERATE MOD: <Modification name>
This menu appears only if the Current Modification is set.
--------------------------------------------------------------------------------
Display
	Show the Current Modification description in a buffer.
--------------------------------------------------------------------------------
Edit source file
	Pops up a menu to select a file from the Current Modification.
	The corresponding file in the Source Directory is loaded into a
	buffer in the current Emacs.
Edit original file
	Pops up a menu to select a file from the Current Modification.
	The corresponding file in the Original Directory is loaded into
	a buffer in the current Emacs.
Edit modification file
	Pops up a menu to select a file from the Current Modification.
	The corresponding file in the Changes Directory is loaded into a
	buffer in the current Emacs.
Edit patch file
	If there is a patch attached to the modification, the
	corresponding file in the Changes Directory is loaded into a
	buffer in the current Emacs.
--------------------------------------------------------------------------------
Check source files
	Checks the dates of the files in the Source Directory with the
	files in the Original Directory. Displays the modification.
Check modification files
	Checks the dates of the files in the Changes Directory with the
	files in the Original Directory. Displays the modification.
--------------------------------------------------------------------------------
Measure source files
	Measure the documentation statistics of the source files of the 
	current modification.
Measure modification files
	Measure the documentation statistics of the modification files of the 
	current modification.
Measure changed documentation
	Measure the documentation statistics of the changed elements of the 
	current modification
--------------------------------------------------------------------------------
Merge source - modification files
	Pops up a menu to select a file from the Current Modification.
	Runs EDIFF-FILES (merge) for the source and modification files
Merge original - modification files
	Pops up a menu to select a file from the Current Modification.
	Runs EDIFF-FILES (merge) for the original and modification files
Merge original - source files
	Pops up a menu to select a file from the Current Modification.
	Runs EDIFF-FILES (merge) for the original and source files
--------------------------------------------------------------------------------
Edit source - modification files
	Pops up a menu to select a file from the Current Modification.
	Launches a new emacs with the file in the Source Directory and
	the corresponding file in the Changes Directory.
Edit original - modification files
	Pops up a menu to select a file from the Current Modification.
	Launches a new emacs with the file in the Original Directory and
	the corresponding file in the Changes Directory.
Edit original - source files
	Pops up a menu to select a file from the Current Modification.
	Launches a new emacs with the file in the Original Directory and
	the corresponding file in the Source Directory.
--------------------------------------------------------------------------------
Copy source - modification
	Copies all the files in the Source Directory to the Changes
	Directory. Asks the user for confirmation.
Copy modification - source
	Copies all the files in the Changes Directory to the Source
	Directory. Asks the user for confirmation.
Copy original - source
	Copies all the files in the Original Directory to the Source
	Directory. Asks the user for confirmation.
--------------------------------------------------------------------------------
Remove patch
	If there is a patch attached to the modification it is removed
Create modification patch
	Creates a patch with all the changed definitions of a modification.
	CAUTION: This should be used carefully. Sometimes additional work
	must be done. However, for simple modifications it may be very helpfull.
	- It will include systems definitions (if changed)
	- It will not include changes in dictionary keywords
	- It does not include exportation symbols, but the whole changed
	  package definitions.
	- The definitions in the patch appear with the same sequence of the
	  affected files as they are in the modification. There may be precedences.
Append definitions to patch
	Appends to the current patch all the changed definitions of a modification.
	CAUTION: This should be used carefully. Sometimes additional work
	must be done. However, for simple modifications it may be very helpfull.
	- It will include systems definitions (if changed)
	- It will not include changes in dictionary keywords
	- It does not include exportation symbols, but the whole changed
	  package definitions.
	- The definitions in the patch appear with the same sequence of the
	  affected files as they are in the modification. There may be precedences.
--------------------------------------------------------------------------------
Split by company
	Split the current modification into modifications for the companies 
	envolved adding the company name as sufix to the current modification name
Split by patches
	Split the current modification into modifications for the companies
	envolved adding the company name as sufix to the current modification name.
	For each company splits the modification into modifications considering the
	the patches description of the user selected version for the correspond company.
Set patch for version
	Finds which patch shall be set to the current modification considering the 
	patches description of the selected version. It increments the version to 
	name the patch file. Usefull when a modification only belongs to a company 
	and a patch was produced for another version.
Set modification version
	Sets the system version to the current modification to help when processing 
	mail file.
Touch current files
	Forces a change in all the affected files. This is usefull before
	compiling the application code to see the compilation warnings for
	the all files of the modification.
--------------------------------------------------------------------------------
Edit all source file
	Opens buffers for all files included in the modification. It is usefull
	for a final check on the modification.
Process mail file
	This option will help the execution of all the process of building the mail
	file. It has the following dinamics:
	- Copy the source files to the modification directory (similar to
	\"Copy source - modification\")
	- Shows a window (like the \"List All Buffers\" window) where the user
	has some available commands.
	  The window layout is the following:
	  - 1st column - Indicates if the file was edited throungh this window.
	  - 2nd column - Indicates if the file is marked was CHANGED, NEW or DELETED
	  - 3rd column - Indicates if there exists problem with the file (eg, signatures)
	  - 4th column - Source file name
	  - 5th column - Modification file name
	With the cursor over a line corresponding to a file the user has the following
	options (pressing the respective character)
	  . \"q\" - quit
	  . \"e\" - edit the file (similar to \"Edit original - modification files\")
	            usefull to solve problems with signatures
	  . \"x\" - Creates the mail file
	  . \"r\" - Remove the file from the modification
	  . \"d\" - Marks the file was DELETED
	  . \"c\" - Msrks the file was CHANGED
	  . \"s\" - Shows a text with the problems of the file
	  . \"h\" - Shows a line with help
	
	After executing the request to build the mail file the system creates the mail
	file and then ask two questions:
	(1) If the user wants to copy the modification file to the source location
	    (similar with \"Copy modification - source\") ;
	(2) If the user wants to delete de modification (similar to \"Delete modification\")
Edit mail file
	If the modification mail file have been created already the 
	corresponding file is loaded into a buffer in the current Emacs.
--------------------------------------------------------------------------------
Help
	Displays this text.
--------------------------------------------------------------------------------"))


;;;-----------------------------------------------------------------------------
;;;MOD-OPERATIONS-ITEMS1
;;;Description
;;;	Returns a list of menu items for the operations over the current modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Created
;;;					Added "Remove patch", "Edit patch file"
;;;	98/11/26	A. Frazao	Commented "Copy & edit original file"
;;;	99/08/31	J. P. Varandas	Added "Process mail file"
;;;	00/03/24	A. Frazao	Added options
;;;					  "Merge source - modification files"
;;;					  "Merge original - modification files"
;;;					  "Merge original - source files"
;;;	02/06/07	J. P. Varandas	Added options
;;;					  "Measure source files"
;;;					  "Measure modification files"
;;;					  "Measure changed documentation"
;;;					  "Split by company"
;;;	03/07/28	A. Frazao	Added options
;;;					  "Create modification patch"
;;;					  "Touch current files"
;;;	04/03/12	A. Frazao	Added "Help"
;;;	05/03/21	J. P. Varandas	Added
;;;					  "Edit all source file"
;;;	05/04/08	Rui Mestre	Added
;;;					  "Find patch"
;;;	05/10/13	J. P. Varandas	Removed
;;;					  "Mail Mail File"
;;;					  "Find patch"
;;;					Added 
;;;					  "Split by patches" 
;;;					  "Append definitions to patch"
;;;					  "Edit mail file"
;;;	06/04/11	J. P. Varandas	Added
;;;					  "Set patch for version"
;;;					  "Set modification version"
;;;	06/05/29	A. Frazao	Added
;;;					  "Edit mod mail file"
;;;	06/05/29	A. Frazao	Removed duplicated "Edit mod mail file"
;;;	09/02/10	J. P. Varandas	Changed function names:
;;;					  'display-current-crews-mod -> 'display-current-modif-request
;;;					  'split-crews-mod -> 'split-modif-request
;;;					  'split-crews-mod-by-patches -> 'split-modif-request-by-patches
;;;					  'set-mod-crews-version -> 'set-modif-request-version
;;;-----------------------------------------------------------------------------
(defvar mod-operations-items1
    (list (sc-make-menu-item "Display"                            'display-current-modif-request)
	  ""
	  (sc-make-menu-item "Edit source file"                   'edit-mod-source-file)
	  (sc-make-menu-item "Edit original file"                 'edit-mod-original-file)
	  (sc-make-menu-item "Edit modification file"             'edit-mod-file)
	  (sc-make-menu-item "Edit patch file"                    'edit-patch-file)
	  ""
	  (sc-make-menu-item "Check source files"                 'check-source-files)
	  (sc-make-menu-item "Check modification files"           'check-modification-files)
	  ""
	  (sc-make-menu-item "Measure source files"               'measure-src-files)
	  (sc-make-menu-item "Measure modification files"         'measure-mod-files)
	  (sc-make-menu-item "Measure changed documentation"      'measure-mod-files-check-date)
	  ""
	  (sc-make-menu-item "Merge source - modification files"   'ediff-source-mod-files)
	  (sc-make-menu-item "Merge original - modification files" 'ediff-original-mod-files)
	  (sc-make-menu-item "Merge original - source files"       'ediff-original-source-files)
	  ""
	  (sc-make-menu-item "Edit source - modification files"   'edit-source-mod-files)
	  (sc-make-menu-item "Edit original - modification files" 'edit-original-mod-files)
	  (sc-make-menu-item "Edit original - source files"       'edit-original-source-files)
	  ""
	  ;;(sc-make-menu-item "Copy & edit original file"        'copy-edit-original-source-file)
	  (sc-make-menu-item "Copy source - modification"         'copy-source-modification-files)
	  (sc-make-menu-item "Copy modification - source"         'copy-modification-source-files)
	  (sc-make-menu-item "Copy original - source"             'copy-original-source-files)
	  ""
	  (sc-make-menu-item "Remove patch"                       'remove-mod-patch)
	  (sc-make-menu-item "Create modification patch"          'recreate-mod-patch)
	  (sc-make-menu-item "Append definitions to patch"        'append-mod-definitions-patch)
	  ""
	  (sc-make-menu-item "Split by company"                   'split-modif-request)
	  (sc-make-menu-item "Split by patches"                   'split-modif-request-by-patches)
	  (sc-make-menu-item "Set patch for version"              'get-patch-for-version)
	  (sc-make-menu-item "Set modification version"           'set-modif-request-version)

	  (sc-make-menu-item "Touch current files"                'touch-current-mod-source-files)
	  ""
	  (sc-make-menu-item "Edit all source file"               'edit-mod-all-source-file)
	  (sc-make-menu-item "Process mail file"                  'process-mail-file)
	  (sc-make-menu-item "Edit mail file"                     'edit-mod-mail-file)
	  ""
	  (sc-make-menu-item "Help"                               'mod-operations-items1-help)))


;;;-----------------------------------------------------------------------------
;;;MOD-OPERATIONS-ITEMS2-HELP
;;;Description
;;;	Display help for \elem{mod-operations-items2}.
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	05/03/21	J. P. Varandas	Added "Reference"
;;;-----------------------------------------------------------------------------
(defun mod-operations-items2-help ()
  (show-mod-help "CHANGE MOD: <Modification name>
This menu appears only if the Current Modification is set.
Allows to modify some of the Current Modification parameters. After each operation
the ~/.modif-requests file is updated.
--------------------------------------------------------------------------------
Name
	Prompts for a new name.
	Is the label of the modification. It must be unique in all modifications.
Author
	Prompts for a new author.
	Names the author of the modification. It is used to fill the headers.
	(variable *DEFAULT-AUTHOR*)
Original directory
	Prompts for a new original directory.
	Is the products directory where the application files are installed.
	By default is /home/siscog.
	(variable *DEFAULT-ORG-DIR*)
Source directory
	Prompts for a new source directory.
	Is the products directory where the source files are modified. This directory
	belongs to the user.
	(variable *DEFAULT-SRC-DIR*)
Changes directory
	Prompts for a new changes directory.
	Is the directory where the modified files are stored (when backed up).
	When creating the mail message, the files attached are those in this
	directory.
	(variable *DEFAULT-MOD-DIR*)
Date
	Prompts for a new date.
	It is a fixed date and is used to fill the headers. The user can identify
	the changes made in a file by searching for this date. When creating the
	mail message, the system will replace this date by the current date in
	all the modification files.
	(variable *DEFAULT-MOD-DATE*)
Reference
	Prompts for a new modification reference
	It is a label that will be appended to all definition change comments.
	This label is also be inserted in the 'Reference' field of the 
	modification file.
 	Usually it is used to indicate the POA or Task under which the changes 
	are being made.
--------------------------------------------------------------------------------
Help
	Displays this text.
--------------------------------------------------------------------------------"))


;;;-----------------------------------------------------------------------------
;;;MOD-OPERATIONS-ITEMS2
;;;Description
;;;	Returns a list of menu items for the operations over the current modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Created
;;;	04/03/12	A. Frazao	Added "Help"
;;;	05/03/21	J. P. Varandas	Added "Reference"
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defvar mod-operations-items2 
    (list (sc-make-menu-item "Name"               'change-modif-request-name)
	  (sc-make-menu-item "Author"             'change-modif-request-author)
	  (sc-make-menu-item "Original directory" 'change-modif-request-org-dir)
	  (sc-make-menu-item "Source directory"   'change-modif-request-src-dir)
	  (sc-make-menu-item "Changes directory"  'change-modif-request-dir)
	  (sc-make-menu-item "Reference"          'change-modif-request-reference)
	  (sc-make-menu-item "Date"               'change-modif-request-date)
	  ""
	  (sc-make-menu-item "Help"               'mod-operations-items2-help)))

;;;-----------------------------------------------------------------------------
;;;MOD-OPERATIONS
;;;Description
;;;	Presents a menu with the operations over the current modification
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of menu items
;;;History
;;;	Date		Author		Description
;;;	97/07/25	A. Frazao	Changed Edit files functions
;;;	97/08/29	A. Frazao	Added BROWSE-COPY-ORIGINAL-SOURCE-FILE
;;;	98/11/04	A. Frazao	Uses MOD-OPERATIONS-ITEMS1 and
;;;					MOD-OPERATIONS-ITEMS2
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;	09/09/23	P. Madeira	`cons' -> `sc-make-menu-item'
;;;-----------------------------------------------------------------------------
(defun mod-operations ()
  (if (current-modif-request)
      (list (sc-make-menu-item (format "OPERATE MOD: %s" (modif-request-name (current-modif-request)))
			       mod-operations-items1)
	    (sc-make-menu-item (format "CHANGE MOD: %s" (modif-request-name (current-modif-request)))
			       mod-operations-items2))))

;;;-----------------------------------------------------------------------------
;;;SEND-MOD-OPERATIONS-ITEMS
;;;Description
;;;	Builds a list of menu items for sending the modification mail.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of menu items
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/09/16	A. Frazao	Created
;;;	05/09/19	A. Frazao	Added call-interactively
;;;	05/10/12	A. Frazao	Changed responsible -> send-to
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;					'send-crews-mod-mail -> 'send-modif-request-mail
;;;-----------------------------------------------------------------------------
(defun send-mod-operations-items ()
  (let ((items nil))
    (dolist (system *sc-all-systems*)
      (when (sc-system-send-to system)
	(push (sc-make-menu-item (format "%s (%s)" (sc-system-name system) (sc-system-send-to system))
				 (list 'send-modif-request-mail (sc-system-send-to system)))
	      items)))
    (push (sc-make-menu-item "Other" '(call-interactively 'send-modif-request-mail)) items)
    (nreverse items)))


;;;-----------------------------------------------------------------------------
;;;SEND-MOD-OPERATIONS
;;;Description
;;;	Builds a list of menu items for sending the modification mail.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of menu items
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/09/16	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed ADT: crews-mod -> modif-request
;;;					Removed *windows-cri*
;;;	09/09/23	P. Madeira	`cons' -> `sc-make-menu-item'
;;;-----------------------------------------------------------------------------
(defun send-mod-operations ()
  (let ((mod (current-modif-request)))
    (when (and mod
	       (modif-request-mail-file mod))
      (let ((current-buffer (current-buffer))
	    (mod-buffer (get-buffer (file-name-nondirectory (modif-request-mail-file mod)))))
	(when (and mod-buffer
		   (eql current-buffer mod-buffer))
	  (list (sc-make-menu-item (format "SEND MOD: %s" (modif-request-name mod)) (send-mod-operations-items))))))))


;;;-----------------------------------------------------------------------------
;;;GENERIC-OPERATIONS-ITEMS1-HELP
;;;Description
;;;	Display help for \elem{generic-operations-items1}.
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/03/12	A. Frazao	Created
;;;	06/05/29	A. Frazao	Added options
;;;					  "Edit all mod mail file"
;;;	09/02/10	J. P. Varandas	*crews-mail-dir* -> *modifs-mail-dir*
;;;-----------------------------------------------------------------------------
(defun generic-operations-items1-help ()
  (show-mod-help "GENERIC
This menu handles generic operations over the modifications
--------------------------------------------------------------------------------
Load
	Loads the modifications.
Create
	Creates a modification. Prompts for its name. The .modif-requests file is
	updated. The modification is NOT automatically selected.
Create & Select
	Creates a modification. Prompts for its name. The .modif-requests file is
	updated. The modification is automatically selected.
Create from mail file
	A modification is created from the mail file used to send a modification.
	Prompts for the modification name and reads the file
	   *modifs-mail-dir*/modif-request-mail
	All the attachments are copied to the modifications directory *DEFAULT-MOD-DIR*.
	It may use new names if attachment names are already in use in other
	modifications. The ~/.modif-requests file is updated. The modification becomes
	current.
Select modification
	A menu pops up. Selecting a modification it will become the Current
	Modification. The ~/.modif-requests file is updated.
Delete modification
	A menu pops up. Selecting a modification it will be deleted. If it is the
	Current Modification, it will be unset. The ~/.modif-requests file is updated.
--------------------------------------------------------------------------------
Display all modifications
	Shows all the modifications descriptions in a buffer.
Edit all source file
	Pops up a menu to select a file from all the modifications. The corresponding
	file in the Source Directory is loaded into a buffer in the current Emacs.
Edit all original file
	Pops up a menu to select a file from all the modifications. The corresponding
	file in the Original Directory is loaded into a buffer in the current Emacs.
Edit all modification file
	Pops up a menu to select a file from all the modifications. The corresponding
	file in the Changes Directory is loaded into a buffer in the current Emacs.
Edit all patch file
	Pops up a menu to select a modification. If there is a patch attached to the
	modification, the corresponding file in the Changes Directory is loaded into
	a buffer in the current Emacs.
Edit all mod mail file
	Pops up a menu to select a modification. If there is a modification mail file
	for the modification, the corresponding file is loaded into a buffer in the
 	current Emacs.
--------------------------------------------------------------------------------
Check all mods source files
	Checks all the modifications files

Copy all source - modification
	Copies all the files in the Source Directory to the Changes Directory for
	all modifications.
Copy all modification - source
	Copies all the files in the Changes Directory to the Source Directory for
	all modifications.
Copy all original - source
	Copies all the files in the Original Directory to the Source Directory
	for all modifications.
--------------------------------------------------------------------------------
Touch all mods files
	Forces a change in all the modifications affected files. This is usefull
	before compiling the application code to see the compilation warnings for
	the all files of the modifications.
--------------------------------------------------------------------------------
Help
	Displays this text.
--------------------------------------------------------------------------------"))

;;;-----------------------------------------------------------------------------
;;;GENERIC-OPERATIONS-ITEMS1
;;;Description
;;;	Is a list of menu items with the generic CRM operations.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Created
;;;	98/11/26	A. Frazao	Commented "Create & select (orig)"
;;;	01/07/04	J. P. Varandas	Remove option "Load"
;;;	01/11/24	A. Frazao	Added option "Load" again
;;;	02/06/07	J. P. Varandas	Added option
;;;					  "Create"
;;;	03/07/28	A. Frazao	Removed "Create & select (orig)"
;;;					Added options
;;;					  "Touch all mods files"
;;;					  "Check all mods source files"
;;;	04/03/12	A. Frazao	Added options
;;;					  "Copy all source - modification"
;;;					  "Copy all modification - source"
;;;					  "Copy all original - source"
;;;					  "Edit all source file"
;;;					  "Edit all original file"
;;;					  "Edit all modification file"
;;;					  "Edit all patch file"
;;;					  "Help"
;;;	06/05/29	A. Frazao	Added options
;;;					  "Edit all mod mail file"
;;;	06/05/29	A. Frazao	Changed options
;;;					  "Edit all mod mail file" -> "Edit all mail file"
;;;	09/02/10	J. P. Varandas	Change function names:
;;;					  'make-crews-modification -> 'make-modification-request
;;;					  'make-crews-modification2 -> 'make-modification-request2
;;;					  'delete-crews-modification -> 'delete-modification-request
;;;					  'select-current-crews-mod -> 'select-current-modif-request
;;;					  'delete-crews-modification -> 'delete-modification-request
;;;					  'display-all-crews-mods -> 'display-all-modif-requests
;;;					Changed ADT: crews-mod -> modif-request
;;;-----------------------------------------------------------------------------
(defvar generic-operations-items1
    (list (sc-make-menu-item "Load"                      'load-modifications)
	  (sc-make-menu-item "Create"                    'make-modification-request2)
	  (sc-make-menu-item "Create + select"           'make-modification-request)
	  (sc-make-menu-item "Create from mail file"     'make-modification-from-mail-file)
	  (sc-make-menu-item "Select modification"       'select-current-modif-request)
	  (sc-make-menu-item "Delete modification"       'delete-modification-request)
	  (sc-make-menu-item "Display all modifications" 'display-all-modif-requests)
	  ""
	  (sc-make-menu-item "Edit all source file"              'edit-all-mod-source-file)
	  (sc-make-menu-item "Edit all original file"            'edit-all-mod-original-file)
	  (sc-make-menu-item "Edit all modification file"        'edit-all-mod-file)
	  (sc-make-menu-item "Edit all patch file"               'edit-all-patch-file)
	  (sc-make-menu-item "Edit all mail file"                'edit-all-mod-mail-file)
	  ""
	  (sc-make-menu-item "Check all mods source files" 'check-all-modifications-source-files)
	  ""
	  (sc-make-menu-item "Copy all source - modification"  'copy-all-source-modification-files)
	  (sc-make-menu-item "Copy all modification - source"  'copy-all-modification-source-files)
	  (sc-make-menu-item "Copy all original - source"      'copy-all-original-source-files)
	  ""
	  (sc-make-menu-item "Touch all mods files"        'touch-all-mods-source-files)
	  ""
	  (sc-make-menu-item "Help"                        'generic-operations-items1-help)))

;;;-----------------------------------------------------------------------------
;;;GENERIC-OPERATIONS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/08/29	A. Frazao	Added MAKE-CREWS-MODIFICATION-ORIG
;;;	98/11/04	A. Frazao	Uses GENERIC-OPERATIONS-ITEMS1 and
;;;					GENERIC-OPERATIONS-ITEMS2
;;;	99/08/31	J. P. Varandas	Uses only the GENERIC-OPERATIONS-ITEMS1
;;;	09/09/23	P. Madeira	`cons' -> `sc-make-menu-item'
;;;-----------------------------------------------------------------------------
(defun generic-operations ()
  (list (sc-make-menu-item "GENERIC" generic-operations-items1)))

;;;-----------------------------------------------------------------------------
;;;SELECT-MODIF-REQUEST-ACTION
;;;Description
;;;	Displays the Change Request Manager's main menu, waits for the user
;;;	choice and executes the operation selected by the user.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{arg} is a \emph{boolean}
;;;		
;;;	\return-types
;;;		void
;;;	
;;;	\refs
;;;		g-definition-language g-position-of-end-of-definition
;;;		g-position-of-beginning-of-definition
;;;		
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Uses new menu implementation
;;;	00/05/05	A. Vasconcelos	Replaced "Crews Change Operations" by "Change Request Manager"
;;;					Added version.
;;;	01/03/02	Toni		Added re-initialisation of the following
;;;					global variables: 'g-definition-language',
;;;					'g-position-of-end-of-definition' and
;;;					'g-position-of-beginning-of-definition'.
;;;					The goal of the use of these variables
;;;					is to avoid the overhead of repeating
;;;					several times the same calculations
;;;					while performing the operation selected
;;;					by the user.
;;;					Updated partially the documentation.
;;;	05/09/16	A. Frazao	Added send-mod-operations
;;;	05/09/16	A. Frazao	Corrected function call
;;;	09/02/10	J. P. Varandas	Changed function name: select-crews-mod-action -> select-modif-request-action
;;;					Changed ADT: crews-mod -> modif-request
;;;	10/01/26	P. Madeira	Reload modifications before showing menu
;;;-----------------------------------------------------------------------------
(defun select-modif-request-action (arg)
  (interactive "e")
  (ignore-errors (load-modifications)) ; needed if sc-emacs used in multiple emacs clients at the same time
  (let* (;; Re-initialises the 'g-definition-language', 'g-position-of-end-of-definition'
	 ;; and 'g-position-of-beginning-of-definition' global variables.
	 (g-definition-language                 nil)
	 (g-position-of-end-of-definition       nil)
	 (g-position-of-beginning-of-definition nil)
	 ;; Creates the CRM pop-up menu and waits for the user choice.
	 ;; The 'selection' variable receives the function that implements the
	 ;; operation choosed by the user (if one is choosed).
	 (selection (sc-popup-menu
		     arg
		     (format "Change Request Manager - Version %s" *crm-version*)
		     (append (definition-operations) (file-operations) (mod-operations) (send-mod-operations) (generic-operations))
		     t)))
    (and selection
	 (let ((*current-x-arg* arg))
	   (if (consp selection)
	       (eval selection)
	       (funcall selection))))))

