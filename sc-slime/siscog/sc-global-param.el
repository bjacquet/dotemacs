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
;;;	Defines global parameters to be read from SISCOG repository.
;;;	
;;; History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	03/09/01	Carlos Ribeiro	Changed definitions
;;;					  "CREWS-BRISA"
;;;	03/09/10	Dario		Added Recorder.
;;;	03/09/19	A. Frazao	Changed definitions
;;;					  (SETF *SC-ALL-FILE-SUBSYSTEMS*)
;;;	03/09/30	A. Frazao	Changed definitions
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;	03/11/14	A. Frazao	Changed definitions
;;;					  "CREWS-VR"
;;;					  "CREWS-STOG"
;;;					  "CREWS-NS"
;;;					Added more definitions to the indent-hook
;;;	04/06/22	A. Frazao	Changed definitions
;;;					  "CREWS-NSB"
;;;					  "CREWS-VR"
;;;					  "CREWS-NS"
;;;					Added more indentation definitions
;;;	04/06/22	A. Frazao	Changed definitions
;;;					  "CREWS-DSB"
;;;					  "CREWS-NSB"
;;;					  "CREWS-STOG"
;;;					  "CREWS-WAGN"
;;;	04/10/22	Carlos Ribeiro	Changed definitions
;;;					  "CREWS-BRISA"
;;;	04/10/29	A. Frazao	Changed definitions
;;;					  "CREWS-VR"
;;;					  "CREWS-STOG"
;;;					  "CREWS-CP"
;;;					  "CREWS-ML"
;;;					  "CREWS-DSB"
;;;	04/11/15	A. Frazao	Changed definitions
;;;					  "CREWS-VR"
;;;	04/12/07	A. Frazao	Changed definitions
;;;					  "CREWS"
;;;	04/12/10	A. Frazao	Changed definitions
;;;					  "CREWS-NS"
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;	05/03/07	A. Frazao	Changed definitions
;;;					  "CREWS-DSB"
;;;					  "CREWS-NS"
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;	05/03/21	J. P. Varandas	Deleted from the indent-hook
;;;				       	  WITH.COMPONENTS.VALUES
;;;					Added more definitions to the indent-hook
;;;					  CHECK.ADD.BUTTON.END
;;;					  DEF.CLASS
;;;					  DO-IN-LIST
;;;					  DO.EACH.YCODE
;;;					  DO.IN.ROUTE
;;;					  DO.LIST
;;;					  DO.MOVEMENTS
;;;					  LETV-SLOTS
;;;					  WHILE.INHIBITING
;;;					  WITH-TRANSLATION-OBJECTS-IN
;;;					  WITH.CLIPPING.RECTANGLE
;;;					  WITH.FORCE.OUTPUT
;;;					  WITH.MODAL.WINDOW
;;;					  WITH.OBJECT.GCONTEXT
;;;					  WITHIN.WORLD.CONTEXT
;;;					Changed definitions
;;;					  "CREWS-NSB"
;;;					  "CREWS-BRISA"
;;;	05/06/25	A. Frazao	Changed definitions
;;;					  "CREWS-DSB"
;;;					  "CREWS-WAGN"
;;;					  "CREWS-NS"
;;;	05/10/12	A. Frazao	Deleted definitions
;;;					  (SETF *SC-ALL-FILE-RESPONSIBLES*)
;;;					Changed definitions
;;;					  "CREWS"
;;;					  "PMS"
;;;					  "EMACS"
;;;					  "CREWS-BRISA"
;;;					  "CREWS-VR"
;;;					  "CREWS-DSB"
;;;					  "CREWS-STOG"
;;;					  "CREWS-ML"
;;;					  "CREWS-NSB"
;;;					  "CREWS-WAGN"
;;;					  "CREWS-CP"
;;;					  "CREWS-NS"
;;;					  "CREWS-SISCOG"
;;;	05/10/12	A. Frazao	Changed definitions
;;;					  "CREWS"
;;;	05/10/13	J. P. Varandas	Changed definitions
;;;					  "CREWS-DSB"
;;;					Added definitions
;;;					  'INHIBITING.SYSTEM.BACKUP
;;;	05/10/14	A. Frazao	Changed definitions
;;;					  "CREWS"
;;;	05/11/08	Sergio Pozzetti	Added definitions
;;;					  "CREWS-LUL"
;;;	05/11/11	Carlos Ribeiro	Changed definitions
;;;					  "CREWS-LUL"
;;;	05/11/21	A. Frazao	Changed definitions
;;;					  "CREWS-NS"
;;;	05/11/30	A. Frazao	Changed definitions
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;	06/04/06	J. P. Varandas	Changed definitions
;;;					  "CREWS-DSB"
;;;					  "CREWS-VR"
;;;					  "CREWS-STOG"
;;;	06/04/12	J. P. Varandas	Changed definitions
;;;					  "CREWS-STOG"
;;;					  "CREWS-LUL"
;;;					  "CREWS-BRISA"
;;;					  "CREWS-VR"
;;;					  "CREWS-DSB"
;;;					  "CREWS-NSB"
;;;					  "CREWS-SISCOG"
;;;					Added definitions
;;;					  *NEW-ODBC-NAMES*
;;;	06/04/17	Sonia Pedrosa	Changed definitions
;;;					  "CREWS-VR"
;;;	06/07/07	A. Frazao	Changed definitions
;;;					  APPLICATIONS
;;;	06/08/24	J. P. Varandas	Changed definitions
;;;					  *NEW-ODBC-NAMES*
;;;	06/11/06	A. Frazao	Changed definitions
;;;					  "CREWS"
;;;					  "CREWS-NS"
;;;					  "CREWS-SISCOG"
;;;	06/12/12	RAurelio	Changed definitions
;;;					  APPLICATIONS
;;;	07/01/11	Hugo Santos	Changed definitions
;;;					  "CREWS-STOG"
;;;	07/01/24	Pedro Matos	Changed definitions
;;;					  "CREWS-VR"
;;;	07/02/05	Ana Pacheco	Changed definitions
;;;					  "CREWS-NSB"
;;;	07/03/21	R. Magalhães	Changed definitions
;;;					  "CREWS-VR"
;;;	07/04/02	Tiago Loureiro	Changed definitions
;;;					  "CREWS-STOG"
;;;	07/04/02	A. Frazao	Changed definitions
;;;					  "CREWS-NS"
;;;	07/04/03	Tiago Loureiro	Changed definitions
;;;					  "CREWS-STOG"
;;;	07/05/14	A. Frazao	Changed definitions
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;	07/05/19	RAurelio	Changed definitions
;;;					  "CREWS-SISCOG"
;;;					  APPLICATIONS
;;;	07/05/21	A. Frazao	Changed definitions
;;;					  "CREWS-DSB"
;;;	07/07/02	A. Frazao	Changed definitions
;;;					  (SETF *SC-ALL-FILE-SUBSYSTEMS*)
;;;	07/07/12	R. Magalhães	Changed definitions
;;;					  "CREWS-VR"
;;;	07/10/30	A. Frazao	Changed definitions
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;	08/03/20	P. Madeira	Changed definitions
;;;					  "CREWS-LUL"
;;;	08/08/01	Fausto		Added definitions
;;;					  "WEB-SERVER"
;;;					Changed definitions
;;;					  "CREWS-DSB"
;;;					  (SETF *SC-ALL-FILE-SUBSYSTEMS*)
;;;					  "CREWS-SISCOG"
;;;	08/09/02	Sonia Pedrosa	Changed definitions
;;;					  "CREWS-STOG"
;;;					  "CREWS-VR"
;;;	08/09/16	RAurelio	Changed definitions
;;;					  "CREWS-DSB"
;;;	08/10/31	A. Frazao	Changed definitions
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;	08/11/03	A. Frazao	Changed definitions
;;;					  "ROSTER-MT"
;;;					  (SETF *SC-ALL-FILE-SUBSYSTEMS*)
;;;	08/11/03	A. Frazao	Added definitions
;;;					  "CRI"
;;;					  "SISCOG-UTIL"
;;;					Changed definitions
;;;					  "CREWS-NS"
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;	08/12/26	MCoutinho	Changed definitions
;;;					  "CREWS-BRISA"
;;;					  "CREWS-STOG"
;;;					  "CRI"
;;;					  "EMACS"
;;;					  "CREWS-LUL"
;;;					  "CREWS-VR"
;;;					  "CREWS-NSB"
;;;					  "CREWS-NS"
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;					  "SISCOG-UTIL"
;;;	08/12/31	P. Filipe	Changed definitions
;;;					  "SISCOG-UTIL"
;;;	09/01/13	J. P. Varandas	Added to 'fi:common-lisp-indent-hook'
;;;					  'while.inhibited
;;;					  'without.on.change
;;;					  'do.panels
;;;					  'without.flickering
;;;					  'with.last.date.transfered.to.dispatcher
;;;					  'with.components.without.on-change
;;;					Deleted definitions
;;;					  *NEW-ODBC-NAMES*
;;;					Changed definitions
;;;					  "CREWS-LUL"
;;;					  "CREWS-BRISA"
;;;					  "CREWS-VR"
;;;					  "CREWS-DSB"
;;;					  "CREWS-STOG"
;;;					  "CREWS-NSB"
;;;					  "CREWS-SISCOG"
;;;	09/01/22	F. Cantarinha	Changed definitions
;;;					  "CREWS-NSB"
;;;	09/01/26	Rui Patrocínio	Changed definitions
;;;					  "CREWS-ML"
;;;	09/02/04	J. P. Varandas	Removed from 'fi:common-lisp-indent-hook'
;;;					  'awhen
;;;					Changed definitions
;;;					  "CRI"
;;;					  "CREWS-LUL"
;;;					  "CREWS-VR"
;;;					  "CREWS-DSB"
;;;					  "CREWS-STOG"
;;;					  "CREWS-ML"
;;;					  "CREWS-NSB"
;;;					  "CREWS-NS"
;;;					  "CREWS-SISCOG"
;;;					  "DISPATCHER-SERVER"
;;;					  "APPLICATION-CONTROLLER-DB"
;;;					  "WEB-SERVER"
;;;					  "RECORDER"
;;;					  "WORK-RECORDER"
;;;					  "DISPATCHER"
;;;					  "SCHEDULER-ST"
;;;					  "ALLOCATOR"
;;;					  "DATA-MANAGER-ST"
;;;					  "ROSTER"
;;;					  "PARTITION-SERVER"
;;;					  "SCHEDULER"
;;;					  "DATA-MANAGER"
;;;					Added definitions
;;;					  "FLEET-ML"
;;;					  "FLEET-SISCOG"
;;;					  "FPLANNER"
;;;					  "EMACS"
;;;					  "PMS"
;;;					  "SISCOG-UTIL"
;;;					  "ONTIME"
;;;					  "FLEET"
;;;					  "CREWS"
;;;					Comment definitions
;;;					  "CREWS-BRISA"
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  "FLEET"
;;;					  "CREWS"
;;;	09/02/13	J. P. Varandas	Added to 'fi:common-lisp-indent-hook'
;;;					  'if
;;;	09/03/03	Alex E.Santo	Changed definitions
;;;					  "CREWS-ML"
;;;	09/04/22	J. P. Varandas	Added application
;;;					  "FPLANNER-ST"
;;;					Changed definitions
;;;					  "FLEET-SISCOG"
;;;					  "FLEET"
;;;	09/05/13	Hugo Santos	Changed definitions
;;;					  "CREWS-NSB"
;;;	09/05/27	P. Fernandes	Changed definitions
;;;					  "CREWS-STOG"
;;;	09/06/09	P. Fernandes	Changed definitions
;;;					  "CREWS-STOG"
;;;	09/06/09	F. Cantarinha	Changed definitions
;;;					  "CREWS-VR"
;;;	09/07/06	Alex E.Santo	Changed definitions
;;;					  "CREWS-ML"
;;;	09/07/14	José Leal	Changed definitions
;;;					  "CREWS-DSB"
;;;	09/07/28	Ricardo Roda	Changed definitions
;;;					  "CREWS-ML"
;;;	09/11/09	J. P. Varandas	Added to 'fi:common-lisp-indent-hook'
;;;					  'with.boxes
;;;					  'with.positions
;;;	09/11/17	A. Frazao	Changed definitions
;;;					  "CREWS-NS"
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;					  "SISCOG-UTIL"
;;;	10/01/08	Alex E.Santo	Changed definitions
;;;					  "CREWS-ML"
;;;	10/01/20	A. Mesquita	Changed definitions (POA 17681.0)
;;;					  "CREWS-NSB"
;;;	10/02/01	A. Mesquita	Changed definitions (POA 17681.0)
;;;					  "CREWS-NSB"
;;;	10/02/23	F. Cantarinha	Changed definitions
;;;					  "CREWS-VR"
;;;	10/04/01	A. Frazao	Changed definitions
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;					  "SISCOG-UTIL"
;;;	10/04/13	A. Frazao	Changed definitions
;;;					  "CREWS-NS"
;;;	10/05/05	Alex E.Santo	Changed definitions
;;;					  "CREWS-ML"
;;;	10/06/25	Ruben Vaz	Changed definitions
;;;					  "CREWS-STOG"
;;;	10/07/14	R. Magalhães	Changed definitions
;;;					  "CREWS-NS"
;;;	10/07/21	Carlos Santos	Changed definitions
;;;					  "CREWS-NSB"
;;;	10/07/27	F. Cantarinha	Changed definitions
;;;					  "CREWS-VR"
;;;	10/09/22	A. Frazao	Changed definitions
;;;					  "CREWS"
;;;					  "CREWS-SISCOG"
;;;					  "CREWS-NSB"
;;;					  (SETF *SC-ALL-FILE-SUBSYSTEMS*)
;;;					Deleted definitions
;;;					  "WORK-RECORDER"
;;;	10/09/28	A. Frazao	Changed definitions
;;;					  "SISCOG-UTIL"
;;;					  "CREWS"
;;;					  "CREWS-NS"
;;;	10/10/18	A. Frazao	Changed definitions
;;;					  "CREWS-SISCOG"
;;;					  "CREWS"
;;;					  "SISCOG-UTIL"
;;;	10/11/10	F. Cantarinha	Changed definitions
;;;					  "CREWS-VR"
;;;	82/06/20	P. Madeira	Moved indentation settings to sc-allegro-eli.el
;;;					Sorted product versions: descending
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Definition of the PRODUCTS
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;PRODUCTS
;;;Description
;;;	Defines the SISCOG products
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(sc-define-product "CREWS"
		   "crews"
		   t)
(sc-define-product "FLEET"
		   "fleet"
		   t)
(sc-define-product "ONTIME"
		   "ontime"
		   t)
(sc-define-product "SISCOG-UTIL"
		   "siscog-util"
		   nil)
(sc-define-product "PMS"
		   "pms"
		   nil)
(sc-define-product "Emacs"
		   "emacs"
		   nil)
(sc-define-product "CRI"
		   "cri"
		   nil)

;;;-----------------------------------------------------------------------------
;;; Definition of the APPLICATIONS
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;APPLICATIONS
;;;Description
;;;	Defines the SISCOG applications
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	06/07/07	A. Frazao	Added su to dispatcher
;;;	06/12/12	RAurelio	Added dispatcher-server
;;;	07/05/19	RAurelio	Changed dispatcher-server display text
;;;	08/08/01	Fausto		Added web-server
;;;	08/11/03	A. Frazao	Removed ROSTER-MT and RSR-CONVERTER
;;;	09/02/04	J. P. Varandas	sc-define-crews-application -> sc-define-application
;;;					Added "FPLANNER" application
;;;-----------------------------------------------------------------------------
(sc-define-application "data-manager"
		       "Data Manager"
		       "data-manager-win"
		       '("su" "mu" "db" "tdmgr-db"))
(sc-define-application "scheduler"
		       "Scheduler"
		       "scheduler-win"
		       '("su" "mu" "db" "tdmgr-db"))
(sc-define-application "partition-server"
		       "Partition Server"
		       "partition-server"
		       '("su"))
(sc-define-application "roster"
		       "Roster"
		       "roster-win"
		       '("db" "tdmgr-db"))
(sc-define-application "data-manager-st"
		       "Data Manager ST"
		       "data-manager-st-win"
		       '("db"))
(sc-define-application "allocator"
		       "Allocator"
		       "allocator-win"
		       '("db"))
(sc-define-application "scheduler-st"
		       "Scheduler ST"
		       "scheduler-st-win"
		       '("db"))
(sc-define-application "dispatcher"
		       "Dispatcher"
		       "dispatcher-win"
		       '("su" "db"))
(sc-define-application "recorder"
		       "Recorder"
		       "recorder-win"
		       '("db"))
(sc-define-application "web-server"
		       "Web Server"
		       "web-server"
		       '("db"))
(sc-define-application "application-controller-db"
		       "Appl Controller DB"
		       "application-controller-db-win"
		       '("db"))
(sc-define-application "dispatcher-server"
		       "Dispatcher Server"
		       "dispatcher-server"
		       '("db"))


(sc-define-application "fplanner"
		       "F-Planner"
		       "fplanner-win"
		       '("su" "db"))

(sc-define-application "fplanner-st"
		       "F-Planner ST"
		       "fplanner-st-win"
		       '("su" "db"))


;;;-----------------------------------------------------------------------------
;;; Definition of the SYSTEMS
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;"SISCOG-UTIL"
;;;Description
;;;	Defines the SISCOG-UTIL System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	08/11/03	A. Frazao	Created
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	08/12/31	P. Filipe	Changed v1-1-0 to v1-0-0
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;	09/11/17	A. Frazao	Added versions "v1-0-1" "v1-2-0"
;;;	10/04/01	A. Frazao	Added version "v1-3-0"
;;;					Removed version "v1-2-0"
;;;	10/09/28	A. Frazao	Added "v1-3-1"
;;;	10/10/18	A. Frazao	Added "v1-4-0"
;;;-----------------------------------------------------------------------------
(sc-define-system "SISCOG-UTIL"
		  "SISCOG-UTIL"
		  nil
		  '("v1-0-0" "v1-0-1" "v1-3-0" "v1-3-1" "v1-4-0")
		  nil
		  nil
		  '((getenv "SISCOG_UTIL_DIR"))
		  "crews@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;; Systems of product CREWS
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;"CREWS"
;;;Description
;;;	Defines the CREWS System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/09/30	A. Frazao	Added version "v6-8-0"
;;;	04/12/07	A. Frazao	Added version "v6-9-0"
;;;	04/12/10	A. Frazao	Removed versions "v6-3-0", "v6-6-0" and "v6-7-0"
;;;	05/03/07	A. Frazao	Removed version "v6-5-0"
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	05/10/12	A. Frazao	Corrected address
;;;	05/10/14	A. Frazao	Updated address
;;;	05/11/30	A. Frazao	Added version "v7-0-0"
;;;	06/11/06	A. Frazao	Added version "v7-1-0"
;;;	07/05/14	A. Frazao	Added version "v8-0-0"
;;;	07/10/30	A. Frazao	Removed versions "v7-0-0" and "v6-8-0"
;;;	08/10/31	A. Frazao	Added version "v8-1-0"
;;;	08/11/03	A. Frazao	Changed version dir
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;					Removed version "v6-9-0"
;;;	09/02/10	J. P. Varandas	Indicate all applications available on the product
;;;	09/11/17	A. Frazao	Added version "v8-2-0"
;;;	10/04/01	A. Frazao	Added version "v8-3-0"
;;;					Removed version "v8-2-0"
;;;	10/09/22	A. Frazao	Removed work-recorder
;;;	10/09/28	A. Frazao	Added "v8-3-1"
;;;	10/10/18	A. Frazao	Added "v8-4-0"
;;;-----------------------------------------------------------------------------
(sc-define-system "CREWS"
		  "CREWS"
		  nil
		  '("v7-1-0" "v8-0-0" "v8-1-0" "v8-3-0" "v8-3-1" "v8-4-0")
		  nil
		  '("data-manager" "scheduler" "roster"
		    "data-manager-st" "allocator" "scheduler-st" "dispatcher"
		    "recorder" "web-server"
		    "application-controller-db" "partition-server" "dispatcher-server")
		  '((getenv "CREWS_DIR"))
		  "crews@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-SISCOG"
;;;Description
;;;	Defines the CREWS-SISCOG System
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/09/30	A. Frazao	Added version "v6-8-0" and application "recorder"
;;;	04/12/10	A. Frazao	Removed versions "v6-6-0" and "v6-7-0"
;;;					Added version "v6-9-0"
;;;	05/03/07	A. Frazao	Removed version "v6-5-0"
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	05/11/30	A. Frazao	Added version "v7-0-0"
;;;	06/04/12	J. P. Varandas	Handles the new ODBC names
;;;	06/11/06	A. Frazao	Added version "v7-1-0"
;;;	07/05/14	A. Frazao	Added version "v8-0-0"
;;;	07/05/19	RAurelio	Added dispatcher-server
;;;	07/10/30	A. Frazao	Removed versions "v7-0-0" and "v6-8-0"
;;;	08/08/01	Fausto		Added "web-server"
;;;	08/10/31	A. Frazao	Added version "v8-1-0"
;;;	08/11/03	A. Frazao	Changed version dir
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	09/01/13	J. P. Varandas	Removed the use of var '*new-odbc-names*' because now is always T
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;					Removed version "v6-9-0"
;;;	09/11/17	A. Frazao	Added version "v8-2-0"
;;;	10/04/01	A. Frazao	Added version "v8-3-0"
;;;					Removed version "v8-2-0"
;;;	10/09/22	A. Frazao	Removed "work-recorder"
;;;	10/10/18	A. Frazao	Added "v8-4-0"
;;;-----------------------------------------------------------------------------
(sc-define-system "CREWS-SISCOG"
		  "CREWS"
		  "siscog"
		  '("v7-1-0" "v8-0-0" "v8-1-0" "v8-3-0" "v8-4-0")
		  '(("siscog009database" "DB018iCRW"))
		  '("data-manager" "scheduler" "partition-server" "roster"
		    "data-manager-st" "allocator" "scheduler-st" "dispatcher"
		    "recorder" "web-server" "application-controller-db" "dispatcher-server")
		  '((getenv "CREWS_SISCOG_DIR"))
		  "crews@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-NS"
;;;Description
;;;	Defines the CREWS NS System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/11/14	A. Frazao	Added version "v3-6-0"
;;;	04/06/22	A. Frazao	Removed version "v3-5-0"
;;;	04/12/10	A. Frazao	Added version "v3-7-0"
;;;	05/03/07	A. Frazao	Removed version "v3-6-0"
;;;	05/06/25	A. Frazao	Changed "v3-7-0" to "v3-7-1"
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	05/11/21	A. Frazao	Changed "v3-7-1" to "v3-7-2"
;;;	06/11/06	A. Frazao	Added version "v3-8-0"
;;;	07/04/02	A. Frazao	Removed version "v3-7-2"
;;;	08/11/03	A. Frazao	Changed version dir
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;	09/11/17	A. Frazao	Updated version to "v4-0-0"
;;;	10/04/13	A. Frazao	Added DB definitions
;;;	10/07/14	R. Magalhães	Added version "v5-0-0"
;;;	10/09/28	A. Frazao	Added version "v4-0-1"
;;;-----------------------------------------------------------------------------
(sc-define-system "CREWS-NS"
		  "CREWS"
		  "ns"
		  '("v4-0-0" "v4-0-1" "v5-0-0")
		  '(("ns011database" "alvor_crw")
		    ("ns011database" "db0511gcrw"))
		  '("data-manager" "scheduler" "partition-server" "data-manager-st" "scheduler-st" "dispatcher" "dispatcher-server" "application-controller-db")
		  '((getenv "CREWS_NS_DIR"))
		  "ns@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-CP"
;;;Description
;;;	Defines the CREWS CP System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/10/29	A. Frazao	Commented
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;-----------------------------------------------------------------------------
;;;(sc-define-system "CREWS-CP"
;;;		  "CREWS"
;;;		  "cp"
;;;		  '("v2-0-0")
;;;		  '(("cp006tdmgr" "crw"))
;;;		  '("data-manager" "scheduler" "partition-server" "roster" "roster-mt")
;;;		  '((getenv "CREWS_CP_VERSION_DIR"))
;;;		  "cp@alfama.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-WAGN"
;;;Description
;;;	Defines the CREWS WAGN System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/06/22	A. Frazao	Added version "v1-4-0"
;;;	05/06/25	A. Frazao	Commented
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;-----------------------------------------------------------------------------
;;;(sc-define-system "CREWS-WAGN"
;;;		  "CREWS"
;;;		  "wagn"
;;;		  '("v1-3-0" "v1-4-0")
;;;		  nil
;;;		  '("rsr-converter" "data-manager" "scheduler" "partition-server")
;;;		  '((getenv "CREWS_WAGN_VERSION_DIR"))
;;;		  "wagn@alfama.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-NSB"
;;;Description
;;;	Defines the CREWS NSB System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/06/22	A. Frazao	Added version "v2-2-0"
;;;					Removed versions "v2-0-0" "v2-1-0"
;;;	04/06/22	A. Frazao	Changed database
;;;	05/03/21	J. P. Varandas	Removed version "2-2-0" and add version "2-3-0"
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	06/04/12	J. P. Varandas	Handles the new ODBC names
;;;	07/02/05	Ana Pacheco	Added version "2-4-0".
;;;					Removed "rsr-converter"
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	09/01/13	J. P. Varandas	Removed the use of var '*new-odbc-names*' because now is always T
;;;	09/01/22	F. Cantarinha	Removed version "v2-3-0" and added
;;; 					version "v2-5-0".
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;	09/05/13	Hugo Santos	Added "web-server" (POA.13388.0)
;;;	10/01/20	A. Mesquita	Added "v2-5-1" version and "dispatcher"
;;;					application; removed "v2-4-0" version
;;;					(POA 17681.0)
;;;	10/02/01	A. Mesquita	Added "v2-6-0" and removed "v2-5-1" versions;
;;;					added "dispatcher-server" application;
;;;					updated data source names (POA 17681.0)
;;;	10/07/21	Carlos Santos	Removed version "v2-5-0" (POA 17681.0).
;;;	10/09/22	A. Frazao	Removed work-recorder
;;;-----------------------------------------------------------------------------
(sc-define-system "CREWS-NSB"
		  "CREWS"
		  "nsb"
		  '("v2-6-0")
		  '(("nsb006database" "db0511gcrw" "SISCOG")
		    ("nsb006database" "db0510gora1" "NSB"))
		  '("data-manager" "scheduler" "roster"
		    "data-manager-st" "allocator" "scheduler-st" "dispatcher"
		    "web-server" "application-controller-db" "dispatcher-server")
		  '((getenv "CREWS_NSB_VERSION_DIR"))
		  "nsb@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-ML"
;;;Description
;;;	Defines the CREWS ML System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/10/29	A. Frazao	Commented
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	09/01/26	Rui Patrocínio	Uncommented; added database and new email
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;	09/03/03	Alex E.Santo	Added version 0.1.0.
;;;	09/07/06	Alex E.Santo	Added version 0.1.1.
;;;					Removed version 0.0.0.
;;;	09/07/28	Ricardo Roda	Added SHORT-TERM applications. (POA.16346.0.3)
;;;	10/01/08	Alex E.Santo	Removed version 0.1.0;
;;;					Added version 0.2.0.
;;;					Changed ML Database reference
;;;	10/05/05	Alex E.Santo	Removed version 0.1.1.
;;;					Added version 1.0.0
;;;-----------------------------------------------------------------------------
(sc-define-system "CREWS-ML"
		  "CREWS"
		  "ml"
		  '("v0-2-0" "v1-0-0")
		  '(("ML011DATABASE" "DB0511gORA1" "ML Database"))
		  '("data-manager" "scheduler" "roster"
		    "data-manager-st" "allocator" "scheduler-st"
		    "application-controller-db")
		  '((getenv "CREWS_ML_VERSION_DIR"))
		  "ml@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-STOG"
;;;Description
;;;	Defines the CREWS STOG System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/11/14	A. Frazao	Added version "v1-4-0"
;;;	04/06/22	A. Frazao	Added version "v1-1-0"
;;;					Changed database
;;;	04/10/29	A. Frazao	Added descriptions
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	06/04/06	J. P. Varandas	Removed
;;;					  Version 1.1.0
;;;					  Application WORK-RECORDER
;;;	06/04/12	J. P. Varandas	Handles the new ODBC names
;;;	07/01/11	Hugo Santos	Added version "v1-5-0", updated odbc
;;;					names. (POA.10404.0)
;;;	07/04/02	Tiago Loureiro	Added version "v1-5-1" (POA 10404.0)
;;;	07/04/03	Tiago Loureiro	Removed version "v1-5-0" (POA 10404.0)
;;;	08/09/02	Sonia Pedrosa	Removed "v1-4-0". Added "v2-0-0".
;;;					Updated odbc names.
;;;					(POA 13450.0)
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	09/01/13	J. P. Varandas	Removed the use of var '*new-odbc-names*' because now is always T
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;	09/05/27	P. Fernandes	Removed "v1-5-1". Updated odbc names.  
;;;                                     (OT.444.4)
;;;	09/06/09	P. Fernandes	Removed duplication of the odbc names (OT.444.4)
;;;	10/06/25	Ruben Vaz	Added "v2-1-0".
;;;-----------------------------------------------------------------------------
(sc-define-system "CREWS-STOG"
		  "CREWS"
		  "stog"
		  '("v2-0-0" "v2-1-0")
		  '(("stog009database" "DB0410gORA2" "STOG Production")
		    ("dsb009database" "DB0410gORA2" "SISCOG 2.0.0"))		  
		  '("data-manager" "scheduler" "roster" "data-manager-st"
		    "allocator" "scheduler-st" "dispatcher" "recorder"
		    "application-controller-db")
		  '((getenv "CREWS_STOG_VERSION_DIR"))
		  "stog@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-DSB"
;;;Description
;;;	Defines the CREWS DSB System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/06/22	A. Frazao	Added version "v2-1-0"
;;;	04/10/29	A. Frazao	Sets "v2-2-0"
;;;	05/03/07	A. Frazao	Added DSB database
;;;	05/06/25	A. Frazao	Added "v2-3-0"
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	05/10/13	J. P. Varandas	work-recorder -> recorder
;;;	06/04/06	J. P. Varandas	Removed version "v2-2-0"
;;;	06/04/12	J. P. Varandas	Handles the new ODBC names
;;;	07/05/21	A. Frazao	Added "dispatcher-server" and "dispatcher"
;;;					Added "v3-0-0"
;;;	08/08/01	Fausto		Added "web-server"
;;;	08/09/16	RAurelio	Updated email server: ALFAMA -> MOURARIA
;;;	09/01/13	J. P. Varandas	Removed the use of var '*new-odbc-names*' because now is always T
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;	09/07/14	José Leal	Removed "v2-3-0" and updated data source names
;;;-----------------------------------------------------------------------------
(sc-define-system "CREWS-DSB"
		  "CREWS"
		  "dsb"
		  '("v3-0-0")
		  '(("pdsdsb009database" "DB0410gORA1" "SISCOG")
		    ("pdsdsb009database" "DB0410gCRW" "DSB-PROD"))
		  '("data-manager" "scheduler" "roster"
		    "data-manager-st" "allocator" "scheduler-st" "dispatcher"
		    "recorder" "web-server" "dispatcher-server" "application-controller-db")
		  '((getenv "CREWS_DSB_VERSION_DIR"))
		  "dsb@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-VR"
;;;Description
;;;	Defines the CREWS VR system
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/11/14	A. Frazao	Added version "v1-2-0"
;;;					Added ("vr009database" "baixa")
;;;	04/06/22	A. Frazao	Removed version "v1-1-1"
;;;	04/10/29	A. Frazao	Added descriptions
;;;	04/11/15	A. Frazao	Updated DB descriptions
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	06/04/06	J. P. Varandas	Add version "1-3-0"
;;;	06/04/12	J. P. Varandas	Handles the new ODBC names
;;;	06/04/17	Sonia Pedrosa	Add version "v1-4-0"
;;;	07/01/24	Pedro Matos	Removed versions 1-2-0 and 1-4-0.
;;;	07/03/21	R. Magalhães	Removed version 1-5-0 and added 2-0-0.
;;;	07/07/12	R. Magalhães	Added version 2-1-0.
;;;	08/09/02	Sonia Pedrosa	Removed "v1-3-0". Added "v2-2-0" and "v2-3-0".
;;;					Updated odbc names.
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	09/01/13	J. P. Varandas	Removed the use of var '*new-odbc-names*' because now is always T
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;	09/06/09	F. Cantarinha	Added "v3-0-0" and removed "v2-2-0". 
;;;                                     (POA 15844.0)
;;;	10/02/23	F. Cantarinha	Added "dispatcher" and "web-server".
;;;					Added "v3-1-0".
;;;					(POA 16174.0)
;;;	10/07/27	F. Cantarinha	Removed "v2-0-0" and "v2-1-0".
;;;                                     Added "v3-1-1"
;;;                                     Updated database information. (POA 19370.0)
;;;	10/11/10	F. Cantarinha	Removed "v3-0-0" and added "v3-2-0" (OT.862.6)
;;;-----------------------------------------------------------------------------
(sc-define-system "CREWS-VR"
		  "CREWS"
		  "vr"
		  '("v2-3-0" "v3-1-0" "v3-1-1" "v3-2-0")
		  '(("vr009database" "DB029IORA1" "SISCOG 2.3.0")
		    ("vr009database" "DB029IORA2" "PRODUCTION 2.3.0")
		    ("vr009database" "DB0510GCRW" "SISCOG V3.1.0 V3.1.1 V3.2.0")
		    ("vr009database" "DB0511GCRW" "IMPORTED V3.1.0 V3.1.1 V3.2.0"))
		  '("data-manager" "scheduler" "roster"
		    "data-manager-st" "allocator" "scheduler-st"
		    "dispatcher" "web-server" "application-controller-db")
		  '((getenv "CREWS_VR_VERSION_DIR"))
		  "vr@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-BRISA"
;;;Description
;;;	System definition for CREWS-BRISA
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/09/01	Carlos Ribeiro	Update to version 1.1.0.
;;;					Set database user definitions.
;;;	04/10/22	Carlos Ribeiro	Update to version 2.0.0.
;;;					Added "work-recorder" application.
;;;	05/03/21	J. P. Varandas	Removed version "v1-0-0"
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	06/04/12	J. P. Varandas	Handles the new ODBC names
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	09/01/13	J. P. Varandas	Removed the use of var '*new-odbc-names*' because now is always T
;;;	09/02/04	J. P. Varandas	Commented
;;;-----------------------------------------------------------------------------
;;;(sc-define-system "CREWS-BRISA"
;;;		  "CREWS"
;;;		  "brisa"
;;;		  '("v2-0-0")
;;;		  '(("brisa009database" "DB018iCRW"))
;;;		  '("data-manager" "scheduler" "roster"
;;;		    "data-manager-st" "allocator" "scheduler-st"
;;;		    "work-recorder" "application-controller-db")
;;;		  '((getenv "CREWS_BRISA_VERSION_DIR"))
;;;		  "brisa@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CREWS-LUL"
;;;Description
;;;	System definition for CREWS_LUL
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/11/08	Sergio Pozzetti	Created
;;;	05/11/11	Carlos Ribeiro	Insert version "v0-0-1". Correct "Control User".
;;;					Insert "data-manager-st" and "scheduler-st"
;;;					applications.
;;;	06/04/12	J. P. Varandas	Handles the new ODBC names
;;;	08/03/20	P. Madeira	DB018iCRW -> DB029iCRW
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	09/01/13	J. P. Varandas	Removed the use of var '*new-odbc-names*' because now is always T
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;-----------------------------------------------------------------------------
(sc-define-system "CREWS-LUL"
		  "CREWS"
		  "lul"
		  '("v1-0-0" "v2-0-0")
		  '(("lul010database" "DB029iCRW")
		    ("lul011database" "DB0511gCRW"))
		  '("data-manager" "scheduler" "roster" 
		    "data-manager-st" "scheduler-st" 
		    "application-controller-db")
		  '((getenv "CREWS_LUL_VERSION_DIR"))
		  "lul@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;; Systems of product FLEET
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;"FLEET"
;;;Description
;;;	Defines the FLEET System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	Indicate all applications available on the product
;;;	09/04/22	J. P. Varandas	Add applications 'data-manager-st' and 'fplanner-st'
;;;-----------------------------------------------------------------------------
(sc-define-system "FLEET"
		  "FLEET"
		  nil
		  '("v1-0-0")
		  nil
		  '("data-manager" "fplanner" "data-manager-st" "fplanner-st" "application-controller-db")
		  '((getenv "FLEET_DIR"))
		  "fleet@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"FLEET-SISCOG"
;;;Description
;;;	Defines the FLEET-SISCOG System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;	09/04/22	J. P. Varandas	Add applications 'data-manager-st' and 'fplanner-st'
;;;-----------------------------------------------------------------------------
(sc-define-system "FLEET-SISCOG"
		  "FLEET"
		  "siscog"
		  '("v1-0-0")
		  '(("fleet011database" "DB018iCRW"))
		  '("data-manager" "fplanner" "data-manager-st" "fplanner-st" "application-controller-db")
		  '((getenv "FLEET_SISCOG_DIR"))
		  "fleet@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"FLEET-ML"
;;;Description
;;;	Defines the FLEET-ML System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(sc-define-system "FLEET-ML"
		  "FLEET"
		  "ml"
		  '("v1-0-0")
		  '(("fleet011database" "DB018iCRW"))
		  '("data-manager" "fplanner" "application-controller-db")
		  '((getenv "FLEET_ML_DIR"))
		  "ml@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;; Systems of product ONTIME
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;"ONTIME"
;;;Description
;;;	Defines the ONTIME System
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(sc-define-system "ONTIME"
		  "ONTIME"
		  nil
		  '("v1-0-0")
		  nil
		  nil
		  '((getenv "ONTIME_DIR"))
		  "ontime@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;; Systems of other products
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;"PMS"
;;;Description
;;;	System definition for PMS
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;-----------------------------------------------------------------------------
(sc-define-system "PMS"
		  "PMS"
		  nil
		  '("v3-2-0")
		  nil
		  nil
		  '((getenv "PMS_DIR"))
		  "raurelio@siscog.pt")


;;;-----------------------------------------------------------------------------
;;;"EMACS"
;;;Description
;;;	System definition for Emacs
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/10/12	A. Frazao	Updated send-to mail address
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;-----------------------------------------------------------------------------
(sc-define-system "Emacs"
		  "Emacs"
		  nil
		  '("v2-0-0")
		  nil
		  nil
		  '((getenv "SISCOG_EMACS_DIR"))
		  "siscog@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;"CRI"
;;;Description
;;;	System definition for CRI
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	08/11/03	A. Frazao	Created
;;;	08/12/26	MCoutinho	Updated email server: ALFAMA -> MOURARIA
;;;	09/02/04	J. P. Varandas	Added product attribute
;;;-----------------------------------------------------------------------------
(sc-define-system "CRI"
		  "CRI"
		  nil
		  nil
		  nil
		  nil
		  '("z:/siscog/bin/cri")
		  "siscog@mouraria.siscog.com")


;;;-----------------------------------------------------------------------------
;;;(SETF *SC-ALL-FILE-SUBSYSTEMS*)
;;;Description
;;;	Defines default applications per pathname.
;;;
;;;History
;;;	Date		Author		Description
;;;	03/09/19	A. Frazao	Added models and readers
;;;	07/07/02	A. Frazao	Added dispatcher
;;;	08/08/01	Fausto		Added web-server
;;;	08/11/03	A. Frazao	Updated systems
;;;	10/09/22	A. Frazao	Removed work-recorder
;;;-----------------------------------------------------------------------------
(setf *sc-all-file-subsystems* '(("utilities/" ("UTIL"))
				 ("traducao/" ("TRADUCAO"))
				 ("sike/" ("SIKE"))
				 ("search/" ("SEARCH"))
				 ("sg-odbc-aodbc-2/" ("SG-ODBC"))
				 ("sg-messaging/" ("SG-MESSAGING"))
				 ("debug-log-file/" ("DEBUG-LOG-FILE"))
				 ("generic-service/" ("GENERIC-SERVICE"))
				 ("help/" ("HELP"))
				 ("xml/" ("XML"))
				 ("wingraphics/" ("WINGRAPHICS"))
				 ("web/" ("WEB"))

				 ("models/" ("DATA-MANAGER" "SCHEDULER" "PARTITION-SERVER" "ROSTER" "DATA-MANAGER-ST"
					     "ALLOCATOR" "SCHEDULER-ST" "RECORDER" "WEB-SERVER" "DISPATCHER"))
				 
				 ("interface/db/short-term/" ("DATA-MANAGER-ST" "ALLOCATOR" "SCHEDULER-ST" "RECORDER" "WEB-SERVER" "DISPATCHER"))
				 ("interface/db/web-server/" ("WEB-SERVER"))
				 ("interface/" ("DATA-MANAGER" "SCHEDULER" "PARTITION-SERVER" "ROSTER" "DATA-MANAGER-ST"
						"ALLOCATOR" "SCHEDULER-ST" "RECORDER" "WEB-SERVER" "DISPATCHER"))
				 
				 ("data-manager/" ("DATA-MANAGER"))
				 ("data-manager-st/" ("DATA-MANAGER-ST"))
				 ("scheduler/dispatch-scheduler/" ("DISPATCHER"))
				 ("scheduler/duty-roster/" ("ROSTER"))
				 ("scheduler/task-scheduler/" ("SCHEDULER"))
				 ("scheduler/scheduler-st/" ("SCHEDULER-ST"))
				 ("scheduler/" ("SCHEDULER" "ROSTER" "SCHEDULER-ST" "DISPATCHER"))
				 ("task-abstractor/" ("SCHEDULER" "SCHEDULER-ST" "DISPATCHER"))
				 ("task-distributor/" ("SCHEDULER"))
				 ("pos-trip-gen/" ("DATA-MANAGER" "SCHEDULER" "SCHEDULER-ST" "DISPATCHER"))
				 ("task-gen/" ("DATA-MANAGER" "SCHEDULER" "SCHEDULER-ST" "DISPATCHER"))
				 ("task-seq/" ("SCHEDULER" "SCHEDULER-ST" "DISPATCHER"))
				 ("dispatcher/server/" ("DISPATCHER-SERVER"))
				 ("dispatcher/" ("DISPATCHER"))
				 ("multi-user/partition-server/clients/" ("DATA-MANAGER" "SCHEDULER" "DATA-MANAGER-ST" "SCHEDULER-ST"))
				 ("multi-user/partition-server/server/" ("PARTITION-SERVER"))
				 ("web-server/" ("WEB-SERVER"))
				 
				 ("readers/data-manager/" ("DATA-MANAGER" "DATA-MANAGER-ST"))
				 ("readers/scheduler/" ("SCHEDULER"))
				 ("readers/roster/" ("ROSTER"))
				 ("readers/data-manager-st/" ("DATA-MANAGER-ST"))
				 ("readers/allocator/" ("ALLOCATOR"))
				 ("readers/scheduler-st/" ("SCHEDULER-ST"))
				 ("readers/dispatcher/" ("DISPATCHER"))
				 ("readers/recorder/" ("RECORDER"))
				 ("readers/short-term/" ("DATA-MANAGER-ST" "ALLOCATOR" "SCHEDULER-ST"
							 "RECORDER" "WEB-SERVER" "DISPATCHER"))
				 ("readers/" ("DATA-MANAGER" "SCHEDULER" "ROSTER" "DATA-MANAGER-ST"
					      "ALLOCATOR" "SCHEDULER-ST" "RECORDER" "WEB-SERVER" "DISPATCHER"))
				 
				 ("middleware/data-manager/" ("DATA-MANAGER"))
				 ("middleware/scheduler/" ("SCHEDULER"))
				 ("middleware/roster/" ("ROSTER"))
				 ("middleware/scheduler-roster/" ("SCHEDULER" "ROSTER"))
				 ("middleware/data-manager-st/" ("DATA-MANAGER-ST"))
				 ("middleware/allocator/" ("ALLOCATOR"))
				 ("middleware/scheduler-st/" ("SCHEDULER-ST"))
				 ("middleware/dispatcher/" ("DISPATCHER"))
				 ("middleware/recorder/" ("RECORDER"))
				 ("middleware/web-server/" ("WEB-SERVER"))
				 ("middleware/short-term/" ("DATA-MANAGER-ST" "ALLOCATOR" "SCHEDULER-ST"
							    "RECORDER" "WEB-SERVER" "DISPATCHER"))
				 ("middleware/" ("DATA-MANAGER" "SCHEDULER" "ROSTER" "DATA-MANAGER-ST"
						 "ALLOCATOR" "SCHEDULER-ST" "RECORDER" "WEB-SERVER" "DISPATCHER"))
				 
				 ("win-maps/data-manager/" ("DATA-MANAGER" "DATA-MANAGER-ST"))
				 ("win-maps/scheduler/" ("SCHEDULER"))
				 ("win-maps/roster/" ("ROSTER"))
				 ("win-maps/scheduler-roster/" ("SCHEDULER" "ROSTER"))
				 ("win-maps/data-manager-st/" ("DATA-MANAGER-ST"))
				 ("win-maps/allocator/" ("ALLOCATOR"))
				 ("win-maps/scheduler-st/" ("SCHEDULER-ST"))
				 ("win-maps/recorder/" ("RECORDER"))
				 ("win-maps/web-server/" ("WEB-SERVER"))
				 ("win-maps/dispatcher/" ("DISPATCHER"))
				 ("win-maps/" ("DATA-MANAGER" "SCHEDULER" "ROSTER" "DATA-MANAGER-ST"
					       "ALLOCATOR" "SCHEDULER-ST" "DISPATCHER" "RECORDER" "WEB-SERVER"))
				 
				 ))

