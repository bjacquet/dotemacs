;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2008, SISCOG - Sistemas Cognitivos Lda.
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
;;;	
;;; History
;;;	Date		Author		Description
;;;	08/07/08	A. Frazao	Created
;;;					Changed definitions
;;;					  INSTALL-CREWS-IMAGE-SENTINEL-OLD
;;;					  INSTALL-CREWS-IMAGE-PROCESS-NEXT-ARG-OLD
;;;					  INSTALL-CREWS-DISTRIBUTION-PROCESS-SYSTEM-OLD
;;;					  INSTALL-CREWS-IMAGE-PROCESS-SYSTEM-OLD
;;;					  INSTALL-CREWS-IMAGE-PROCESS-FORM-OLD
;;;					  INSTALL-CREWS-IMAGE-CHECK-ARGS-OLD
;;;					  GET-CREWS-SYSTEMS-OLD
;;;					  PROCESS-FINAL-DISTRIBUTION-OLD
;;;					  CREWS-DISTRIBUTIONS-OLD
;;;					  CREWS-IMAGE-DISTRIBUTION-NAME-OLD
;;;					  CREWS-IMAGE-DISTRIBUTION-SPLASH-FROM-FILE-OLD
;;;	08/07/08	A. Frazao	Changed definitions
;;;					  INSTALL-CREWS-IMAGE-PROCESS-FORM-OLD
;;;	08/07/11	A. Frazao	Added definitions
;;;					  SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS-OLD
;;;					  *SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS-OLD*
;;;	08/07/31	A. Frazao	Changed definitions
;;;					  INSTALL-CREWS-IMAGE-PROCESS-NEXT-ARG-OLD
;;;					  INSTALL-CREWS-IMAGE-CHECK-ARGS-OLD
;;;	09/02/04	J. P. Varandas	Changed definitions
;;;					  PROCESS-FINAL-DISTRIBUTION-OLD
;;;					  INSTALL-CREWS-IMAGE-CHECK-ARGS-OLD
;;;					  GET-CREWS-SYSTEMS-OLD
;;;					  SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS-OLD
;;;	09/02/10	J. P. Varandas	Deleted definitions
;;;					  INSTALL-CREWS-IMAGE-SENTINEL-OLD
;;;					  INSTALL-CREWS-IMAGE-PROCESS-NEXT-ARG-OLD
;;;					  INSTALL-CREWS-IMAGE-CHECK-ARGS-OLD
;;;					Changed definitions
;;;					  CREWS-IMAGE-DISTRIBUTION-NAME-OLD
;;;					  INSTALL-CREWS-DISTRIBUTION-PROCESS-SYSTEM-OLD
;;;					  INSTALL-CREWS-IMAGE-PROCESS-SYSTEM-OLD
;;;					  INSTALL-CREWS-IMAGE-PROCESS-FORM-OLD
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  INSTALL-CREWS-DISTRIBUTION-PROCESS-SYSTEM-OLD
;;;	09/03/06	RAurelio	Changed definitions
;;;					  INSTALL-CREWS-DISTRIBUTION-PROCESS-SYSTEM-OLD
;;;	10/04/27	RAurelio	Changed definitions
;;;					  INSTALL-CREWS-DISTRIBUTION-PROCESS-SYSTEM-OLD
;;;-----------------------------------------------------------------------------



;;;-----------------------------------------------------------------------------
;;;*SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS-OLD*
;;;Description
;;;	Stores the menu items of the CREWS applications for each system.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	08/07/11	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar *sc-crews-company-systems-menu-items-old* nil)


;;;-----------------------------------------------------------------------------
;;;SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS-OLD
;;;Description
;;;	Returns the menu items of the CREWS applications for each system.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of menu items.
;;;		
;;;History
;;;	Date		Author		Description
;;;	08/07/11	A. Frazao	Created
;;;	09/02/04	J. P. Varandas	sc-crews-application-external-id -> sc-application-external-id
;;;					sc-system-crews-name -> sc-system-company-name
;;;-----------------------------------------------------------------------------
(defun sc-crews-company-systems-menu-items-old ()
  (unless *sc-crews-company-systems-menu-items-old*
    (dolist (system *sc-all-systems*)
      (when (sc-system-company-name system)
	(let ((items nil))
	  (dolist (sc-crews-appl (sc-system-applications system))
	    (push (sc-make-menu-item (sc-application-external-id sc-crews-appl)
				     (list (sc-system-company-name system) (sc-application-image-name sc-crews-appl)))
		  items))
	  (push (cons (upcase (sc-system-company-name system)) (reverse items)) *sc-crews-company-systems-menu-items-old*))))
    (setf *sc-crews-company-systems-menu-items-old* (reverse *sc-crews-company-systems-menu-items-old*)))
  *sc-crews-company-systems-menu-items-old*)


;;;-----------------------------------------------------------------------------
;;;CREWS-IMAGE-DISTRIBUTION-SPLASH-FROM-FILE-OLD
;;;Description
;;;	Return the file with the bitmap that is presented to the user when a 
;;;	distribution image starts.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \emph{string} with the name of the system what will be compiled.
;;;		
;;;		\arg{company} is a \emph{string} with the name of the company what will be compiled.
;;;		
;;;	\return-types
;;;		A \elem{pathname}
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	99/03/19	A. Frazao	Added nsb allocator
;;;	99/12/07	A. Frazao	Added ns
;;;	00/01/06	A. Frazao	Updated NSB for short-term
;;;	00/07/26	J. P. Varandas	Added nsb work-recorder
;;;	01/09/27	A. Vasconcelos	Updated for CREWS_ML and CREWS_DSB.
;;;	01/10/09	Fausto		Changed name of dsb files.
;;;	02/01/13	Fausto		Updated for CREWS_STOG.
;;;	02/01/31	Pedro Matos	Updated for CREWS_VR.
;;;	02/05/21	Fausto		Updated for CREWS_DSB.
;;;	02/06/27	J. P. Varandas	Updated for TPO adding bitmaps for 
;;;					RSR-CONVERTER-WIN and APPLICATION-CONTROLLER-DB-WIN
;;;	02/07/18	Carlos Ribeiro	Updated for CREWS_BRISA.
;;;	02/12/09	A. Frazao	Updated for CREWS_SISCOG
;;;	03/04/16	A. Frazao	Added dispatcher in CREWS_SISCOG
;;;	03/04/29	A. Vasconcelos	Added application-controller-db-win in CREWS_DSB
;;;	03/05/14	A. Frazao	Updated crews-siscog names
;;;	03/09/10	Dario		Updated for Recorder.
;;;	04/11/16	Carlos Ribeiro	Updated for CREWS_BRISA V2-0-0.
;;;	05/02/23	A. Vasconcelos	Updated for CREWS-DSB (Recorder).
;;;	05/03/21	J. P. Varandas	Updated for CREWS-NSB (TPO).
;;;	05/11/11	Carlos Ribeiro	Added LUL system.
;;;	07/02/16	R. Magalhães	Added vip-appl-ctrl.bmp.
;;;	07/06/08	A. Mesquita	Updated for CREWS-DSB (Dispatcher)
;;;	08/07/08	A. Frazao	Changed name. Deprecated.
;;;-----------------------------------------------------------------------------
(defun crews-image-distribution-splash-from-file-old (system company)
  (let ((var (format "CREWS_%s_VERSION_DIR" (upcase company))))
    (cond ((string= company "siscog")
	   (cond ((string= system "scheduler-win")
		  (make-env-file var "crews-siscog-sclt.bmp"))
		 ((string= system "scheduler-st-win")
		  (make-env-file var "crews-siscog-scst.bmp"))
		 ((string= system "roster-win")
		  (make-env-file var "crews-siscog-roster.bmp"))
		 ((string= system "data-manager-win")
		  (make-env-file var "crews-siscog-dmlt.bmp"))
		 ((string= system "data-manager-st-win")
		  (make-env-file var "crews-siscog-dmst.bmp"))
		 ((string= system "allocator-win")
		  (make-env-file var "crews-siscog-staff-all.bmp"))
		 ((string= system "work-recorder-win")
		  (make-env-file var "crews-siscog-work-recorder.bmp"))
		 ((string= system "dispatcher-win")
		  (make-env-file var "crews-siscog-dispatcher.bmp"))
		 ((string= system "application-controller-db-win")
		  (make-env-file var "crews-siscog-app-controller.bmp"))
		 ))
	  ((string= company "nsb")
	   (cond ((string= system "scheduler-win")
		  (make-env-file var "tpo-sclt.bmp"))
		 ((string= system "scheduler-st-win")
		  (make-env-file var "tpo-scst.bmp"))
		 ((string= system "roster-win")
		  (make-env-file var "tpo-roster.bmp"))
		 ((string= system "data-manager-win")
		  (make-env-file var "tpo-dmlt.bmp"))
		 ((string= system "data-manager-st-win")
		  (make-env-file var "tpo-dmst.bmp"))
		 ((string= system "allocator-win")
		  (make-env-file var "tpo-allocator.bmp"))
		 ((string= system "work-recorder-win")
		  (make-env-file var "tpo-wk-recorder.bmp"))
;;;		 ((string= system "rsr-converter-win")
;;;		  (make-env-file var ""))
		 ((string= system "application-controller-db-win")
		  (make-env-file var "tpo-app-controller.bmp"))
		 ))
	  ((string= company "wagn")
	   (cond ((string= system "scheduler-win")
		  (make-env-file var "wagn-sch.bmp"))
		 ((string= system "rsr-converter-win")
		  (make-env-file var "wagn-conv.bmp"))
		 ((string= system "data-manager-win")
		  (make-env-file var "wagn-dm.bmp"))))
	  ((string= company "cp")
	   (cond ((string= system "scheduler-win")
		  (make-env-file var "cp-sch.bmp"))
		 ((string= system "roster-win")
		  (make-env-file var "cp-roster.bmp"))
		 ((string= system "data-manager-win")
		  (make-env-file var "cp-dm.bmp"))))
	  ((string= company "ns")
	   (cond ((string= system "scheduler-win")
		  (make-env-file var "ns-sch.bmp"))
		 ((string= system "data-manager-win")
		  (make-env-file var "ns-dm.bmp"))))
	  ((string= company "ml")
	   (cond ((string= system "scheduler-win")
		  (make-env-file var "ml-sch.bmp"))
		 ((string= system "roster-win")
		  (make-env-file var "ml-roster.bmp"))
		 ((string= system "data-manager-win")
		  (make-env-file var "ml-dm.bmp"))))
	  ((string= company "stog")
	   (cond ((string= system "scheduler-win")
		  (make-env-file var "pds-sch.bmp"))
		 ((string= system "scheduler-st-win")
		  (make-env-file var "pds-sch-st.bmp"))
		 ((string= system "roster-win")
		  (make-env-file var "pds-roster.bmp"))
		 ((string= system "data-manager-win")
		  (make-env-file var "pds-dm.bmp"))
		 ((string= system "data-manager-st-win")
		  (make-env-file var "pds-dm-st.bmp"))
		 ((string= system "allocator-win")
		  (make-env-file var "pds-staff-st.bmp"))
		 ((string= system "work-recorder-win")
		  (make-env-file var "pds-wr-st.bmp"))
		 ((string= system "recorder-win")
		  (make-env-file var "pds-re.bmp"))))
	  ((string= company "dsb")
	   (cond ((string= system "scheduler-win")
		  (make-env-file var "pds-dsb-sch.bmp"))
		 ((string= system "scheduler-st-win")
		  (make-env-file var "pds-dsb-sch-st.bmp"))
		 ((string= system "roster-win")
		  (make-env-file var "pds-dsb-roster.bmp"))
		 ((string= system "data-manager-win")
		  (make-env-file var "pds-dsb-dm.bmp"))
		 ((string= system "data-manager-st-win")
		  (make-env-file var "pds-dsb-dm-st.bmp"))
		 ((string= system "allocator-win")
		  (make-env-file var "pds-dsb-staff-st.bmp"))
		 ((string= system "recorder-win")
		  (make-env-file var "pds-dsb-re.bmp"))
		 ((string= system "dispatcher-win")
		  (make-env-file var "pds-dsb-dispatcher.bmp"))
		 ((string= system "application-controller-db-win")
		  (make-env-file var "pds-dsb-appl-ctrl.bmp"))
		 ))
	  ((string= company "vr")
	   (cond ((string= system "scheduler-win")
		  (make-env-file var "vip-sch.bmp"))
		 ((string= system "scheduler-st-win")
		  (make-env-file var "vip-sch-st.bmp"))
		 ((string= system "roster-win")
		  (make-env-file var "vip-roster.bmp"))
		 ((string= system "data-manager-win")
		  (make-env-file var "vip-dm.bmp"))
		 ((string= system "data-manager-st-win")
		  (make-env-file var "vip-dm-st.bmp"))
		 ((string= system "allocator-win")
		  (make-env-file var "vip-staff-st.bmp"))
		 ((string= system "application-controller-db-win")
		  (make-env-file var "vip-appl-ctrl.bmp"))))
	  ((string= company "brisa")
	   (cond ((string= system "data-manager-win")
		  (make-env-file var "crews-brisa-v200-gestor-dados-lt.bmp"))
		 ((string= system "scheduler-win")
		  (make-env-file var "crews-brisa-v200-planeador-turnos-lt.bmp"))
		 ((string= system "roster-win")
		  (make-env-file var "crews-brisa-v200-planeador-escalas-lt.bmp"))
		 ((string= system "data-manager-st-win")
		  (make-env-file var "crews-brisa-v200-gestor-dados-st.bmp"))
		 ((string= system "allocator-win")
		  (make-env-file var "crews-brisa-v200-afectacao-pessoal-st.bmp"))
		 ((string= system "scheduler-st-win")
		  (make-env-file var "crews-brisa-v200-planeador-st.bmp"))
		 ((string= system "work-recorder-win")
		  (make-env-file var "crews-brisa-v200-registo-trabalho-st.bmp"))
		 ((string= system "application-controller-db-win")
		  (make-env-file var "crews-brisa-v200-controlador-aplicacoes.bmp"))
		 ))
	  ((string= company "lul")
	   (cond ((string= system "data-manager-win")
		  (make-env-file var "tss-v001-data-manager.bmp"))
		 ((string= system "scheduler-win")
		  (make-env-file var "tss-v001-duty-scheduler.bmp"))
		 ((string= system "roster-win")
		  (make-env-file var "tss-v001-roster-scheduler.bmp"))
		 ((string= system "data-manager-st-win")
		  (make-env-file var "tss-v001-short-term-data-manager.bmp"))
		 ((string= system "scheduler-st-win")
		  (make-env-file var "tss-v001-short-term-scheduler.bmp"))
		 ((string= system "application-controller-db-win")
		  (make-env-file var "tss-v001-application-controller-db.bmp"))
		 ))
	  )))


;;;-----------------------------------------------------------------------------
;;;CREWS-IMAGE-DISTRIBUTION-NAME-OLD
;;;Description
;;;	Returns the name of the distribution image for a given system.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{appl-image-name} is a \emph{string}, an application image name.
;;;		
;;;		\arg{crews-name} is a \emph{string}, a system crews name.
;;;		
;;;	\return-types
;;;		A \emph{string}, the real application image file name.
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	99/02/12	A. Frazao	Added application-controller-db-win
;;;	99/03/15	A. Frazao	Added allocator-win
;;;	99/07/06	Joao Filipe	Added roster-mt-win, scheduler-st-win,
;;;					data-manager-st-win
;;;	00/07/26	J. P. Varandas	Added work-recorder-win
;;;	03/04/16	A. Frazao	Added dispatcher-win
;;;	03/07/28	A. Frazao	Use matching
;;;	08/07/08	A. Frazao	Changed name. Deprecated.
;;;	09/02/10	J. P. Varandas	*install-crews-distribution-postfix* -> *install-distribution-postfix*
;;;-----------------------------------------------------------------------------
(defun crews-image-distribution-name-old (appl-image-name crews-name)
  (let ((name (if (string-match "-win$" appl-image-name)
		  (subseq appl-image-name 0 (match-beginning 0))
		  (format "%s-%s" crews-name appl-image-name))))
    (if *install-distribution-postfix*
	(format "%s%s" name *install-distribution-postfix*)
        name)))


;;;-----------------------------------------------------------------------------
;;;CREWS-DISTRIBUTIONS-OLD
;;;Description
;;;	Returns the distribution description
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{company} is a string
;;;		
;;;	\return-types
;;;		A list of lists
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/02/27	A. Frazao	Created
;;;	99/02/28	A. Frazao	Copy also DLLs to the server installation
;;;	99/02/28	A. Frazao	Copy also the bundle file
;;;	99/03/17	Patricia	Set distribution for WAGN
;;;	99/03/31	A. Vasconcelos	Set distribution for CP
;;;	99/07/26	A. Frazao	Added allocator in NSB
;;;	99/07/29	A. Frazao	Updated for NSB 1.0.4
;;;	99/09/13	P. Vieira	Set distribution for NS
;;;	99/09/13	P. Vieira	Added "msvcrtd.dll" to "univocal-path-server"
;;;                                     directory, in NS distribution
;;;	99/11/20	A. Frazao	Removed files.ebu
;;;	99/12/07	A. Frazao	Updated NS
;;;	00/01/06	A. Frazao	Updated NS and NSB
;;;	00/07/19	A. Vasconcelos	Updated CP
;;;	00/07/26	J. P. Varandas	Updated NSB
;;;	00/09/06	A. Frazao	Changed macro-caracters.cl to macro-characters.lisp
;;;					The installations are made in the bin directory
;;;					of the CREWS_X directory
;;;	01/06/19	Toni		Adapted for CREWS_WAGN 1.3.0.
;;;	01/07/04	J. P. Varandas	Updated NSB
;;;	01/09/27	A. Vasconcelos	Updated for CREWS_ML and CREWS_DSB.
;;;	01/10/09	Fausto		Canged DSB into PDS.
;;;	02/01/31	Pedro Matos	Updated for CREWS_VR.
;;;	02/03/13	Pedro Matos	Updated to VIP V0-1-0
;;;	02/05/21	Fausto		Added PDS-DSB.
;;;	02/06/07	A. Vasconcelos	Updated for CREWS_ML 1-0-0
;;;	02/06/07	J. P. Varandas	Changed version of NSB
;;;	02/06/27	J. P. Varandas	Add copy of the pack-problem.bat file for all companies
;;;	02/07/18	Carlos Ribeiro	Insert CREWS_BRISA 0-0-1.
;;;	02/07/22	Fausto		Changed version of PDS S-tog to v1-1-0.
;;;	02/11/01	A. Vasconcelos	Updated for PDS-DSB v1.0.0
;;;	02/12/09	A. Frazao	Updated for CREWS_SISCOG
;;;	02/12/17	A. Frazao	Corrected CREWS_SISCOG
;;;	03/01/15	A. Frazao	Added multi-user executables in siscog
;;;	03/04/16	A. Frazao	Added dispatcher in siscog
;;;	03/04/29	A. Vasconcelos	Updated for PDS-DSB v2.0.0
;;;	03/07/16	Pedro Matos	Updated to VIP V1-1-1
;;;	03/09/10	Dario		Updated for Recorder.
;;;	04/10/22	Carlos Ribeiro	Updated for CREWS-BRISA v2.0.0.
;;;	05/03/07	A. Frazao	Added recorder in CREWS SISCOG
;;;	05/03/21	J. P. Varandas	Updated for CREWS-NSB v2.3.0.
;;;					The 'Application Controller' is now a separated installation.
;;;	06/11/06	A. Frazao	Updated for CREWS-NS 3.8.0
;;;	06/12/12	RAurelio	Added dispatcher-server
;;;	07/01/11	Hugo Santos	Changed version of PDS S-tog to v1-5-0. (POA.10404.0)
;;;	07/01/22	Pedro Matos	Changed version of VR to v1-5-0. [POA.10249.0]
;;;	07/02/05	Ana Pacheco	Changed version of TPO to v2-4-0.
;;;	07/02/16	R. Magalhães	Changed version of VR to v2-0-0.
;;;	07/04/17	Hugo Santos	Changed version of PDS S-tog to v1-5-1. (POA.10404.0)
;;;	08/07/08	A. Frazao	Changed name. Deprecated.
;;;-----------------------------------------------------------------------------
(defun crews-distributions-old (company)
  (let* ((crews-dir   (getenv "CREWS_VERSION_DIR"))
	 (crews-x-dir (getenv (format "CREWS_%s_VERSION_DIR" (upcase company))))
	 (crews-x-top (up-directory crews-x-dir))
	 (distr-dir   (format "%s/distribution" crews-x-top))
	 (crews-bin   (format "%s/bin" (up-directory (getenv "CREWS_VERSION_DIR"))))
	 (data-dir    (format "%s/%s-data" crews-x-top company))
	 (patches-dir (format "%s/patches/crews-%s" crews-x-top company))
	 (dll-dir     (format "%s/dll" (getenv "CREWS_VERSION_DIR")))
	 (pack-dir    (format "%s/data-organization" (getenv "CREWS_VERSION_DIR"))))
    (cond ((string= company "siscog")
	   (list (list "crews-siscog-distribution"
		       (append (list (format "%s/%s" crews-dir "macro-characters.lisp")
				     (format "%s/%s" crews-bin "appl-controller.exe")
				     (format "%s/%s" crews-bin "applmgr-server.exe")
				     (format "%s/%s" crews-bin "launcher-server.exe")
				     (format "%s/%s" crews-bin "pcserver.exe"))
			       (files-with-name distr-dir "data-manager")
			       (files-with-name distr-dir "scheduler")
			       (files-with-name distr-dir "roster")
			       (files-with-name distr-dir "siscog-partition-server")
			       (files-with-name distr-dir "data-manager-st")
			       (files-with-name distr-dir "scheduler-st")
			       (files-with-name distr-dir "allocator")
			       (files-with-name distr-dir "recorder")
			       (files-with-name distr-dir "work-recorder")
			       (files-with-name distr-dir "dispatcher")
			       (files-with-name distr-dir "application-controller-db")
			       (files-with-type dll-dir "dll")
			       (files-with-type distr-dir "dll")
			       (files-with-name distr-dir "dispatcher-server")
)
		       nil)
		 ))
	  ((string= company "cp")
	   (let ((version "2-0-0"))
	     (list (list (format "escalas-server-%s/escalas-%s/bin" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp")
				       (format "%s/%s" crews-bin "appl-controller.exe")
				       (format "%s/%s" crews-bin "applmgr-server.exe")
				       (format "%s/%s" crews-bin "launcher-server.exe")
				       (format "%s/%s" crews-bin "pcserver.exe")
				       )
				 (files-with-name distr-dir "application-controller-db")
				 (files-with-name distr-dir "cp-partition-server")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll"))
			 nil)
		   (list (format "escalas-server-%s/escalas-%s/crews5-0/dll" version version)
			 (files-with-type dll-dir "dll") nil)
		   (list (format "escalas-server-%s/escalas-%s" version version)
			 nil (list data-dir))
		   (list (format "escalas-server-%s/escalas-%s/patches" version version)
			 nil (list patches-dir))
		   (list (format "escalas-client-manager-%s/escalas-%s/bin" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp")
				       (format "%s/%s" crews-bin "appl-controller.exe"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "roster")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "escalas-client-planner-%s/escalas-%s/bin" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "roster"))
			 nil)
		   (list "univocal-path-server"
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" dll-dir   "machine_win.dll")
			       (format "%s/%s" dll-dir   "msvcrtd.dll"))
			 nil)
		   (list "timetable_converter"
			 nil
			 '("g:/conversor-horarios/conversor"))
		   (list "management-reports"
			 nil
			 '("g:/relatorios-gestao/relatorios"))
		   )))
	  
	  ((string= company "wagn")
	   (let ((version "1-3-0")
		 (updates-dir (format "%s/updates" crews-x-top company)))
	     (list ;; UNIVOCAL PATH SERVER distribution
	      (list "univocal-path-server"
		    (list (format "%s/%s" crews-bin "upserver-server.exe")
			  (format "%s/%s" crews-bin "sc.exe")
			  (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			  (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			  (format "%s/%s" dll-dir   "machine_win.dll")
			  (format "%s/%s" dll-dir   "msvcrtd.dll"))
		    nil)
	      ;; SERVER distribution
	      (list (format "crews-wagn-server-%s/crews-wagn-%s/bin" version version)
		    (append (list (format "%s/%s" crews-dir "macro-characters.lisp")
				  (format "%s/%s" crews-bin "appl-controller.exe")
				  (format "%s/%s" crews-bin "applmgr-server.exe")
				  (format "%s/%s" crews-bin "launcher-server.exe")
				  (format "%s/%s" crews-bin "pcserver.exe"))
			    (files-with-name distr-dir "wagn-partition-server")
			    (files-with-type dll-dir   "dll")
			    (files-with-type distr-dir "dll"))
		    nil)
	      (list (format "crews-wagn-server-%s/crews-wagn-%s" version version)
		    nil (list data-dir))
	      ;;(list (format "crews-wagn-server-%s/crews-wagn-%s/patches" version version)
	      ;;nil (list patches-dir))
	      (list (format "crews-wagn-server-%s/crews-wagn-%s/updates" version version)
		    (append (files-with-type updates-dir "zip")
			    (files-with-type updates-dir "ZIP")
			    (files-with-type updates-dir "txt"))
		    nil)
	      ;; CLIENT MANAGER distribution
	      (list (format "crews-wagn-client-manager-%s/crews-wagn-%s/bin" version version)
		    (append (list (format "%s/%s" crews-dir "macro-characters.lisp")
				  (format "%s/%s" crews-bin "appl-controller.exe"))
			    (files-with-type pack-dir  "bat")
			    (files-with-name distr-dir "rsr-converter")
			    (files-with-name distr-dir "data-manager")
			    (files-with-name distr-dir "scheduler")
			    (files-with-type dll-dir   "dll")
			    (files-with-type distr-dir "dll"))
		    nil)
	      ;; CLIENT PLANNER distribution
	      (list (format "crews-wagn-client-planner-%s/crews-wagn-%s/bin" version version)
		    (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
			    (files-with-name distr-dir "scheduler")
			    (files-with-type dll-dir   "dll")
			    (files-with-type distr-dir "dll"))
		    nil)
	      )))
	  
	  ((string= company "ns")
	   (let ((version "v3-8-0"))
	     (list (list (format "crews-ns-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" dll-dir   "machine_win.dll")
			       (format "%s/%s" dll-dir   "msvcrtd.dll"))
			 nil)
		   (list (format "crews-ns-%s" version)
			 nil (list crews-x-dir))
		   (list (format "crews-ns-%s/ns-data" version)
			 nil (list data-dir))
		   (list (format "crews-ns-%s/patches" version)
			 nil (list patches-dir))
		   (list (format "crews-ns-%s/patches" version)
			 nil (list patches-x-dir))
		   (list (format "crews-ns-%s/bin" version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp")
				       (format "%s/%s" crews-bin "appl-controller.exe")
				       (format "%s/%s" crews-bin "applmgr-server.exe")
				       (format "%s/%s" crews-bin "launcher-server.exe")
				       (format "%s/%s" crews-bin "pcserver.exe"))
				 (files-with-name distr-dir "ns-partition-server")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "scheduler-dutch")
				 (files-with-type dll-dir "dll")
				 (files-with-type distr-dir "dll"))
			 nil)
		   )))
	  
	  ((string= company "nsb")
	   (let ((version "2-4-0"))
	     (list (list (format "tpo-%s/tpo-reference-%s" version version)
			 nil (list (format "%s/%s"  crews-x-top "nsb-data")))
		   
		   (list (format "tpo-%s/tpo-application-controller-%s" version version)
			 (append (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   
		   (list (format "tpo-%s/tpo-long-term-manager-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager"))
			 nil)
		   (list (format "tpo-%s/tpo-short-term-manager-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager-st"))
			 nil)
		   (list (format "tpo-%s/tpo-long-term-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir  "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "roster"))
			 nil)
		   
		   (list (format "tpo-%s/tpo-short-term-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler-st")
				 (files-with-name distr-dir "allocator")
				 (files-with-name distr-dir "work-recorder"))
			 nil)
		   
		   (list (format "tpo-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" dll-dir   "machine_win.dll"))
			 nil)
		   )))
	  ((string= company "ml")
	   (let ((version "1-0-0"))
	     (list (list (format "crews-ml-pilot-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" dll-dir   "machine_win.dll")
			       (format "%s/%s" dll-dir   "msvcrtd.dll"))
			 nil)
		   (list (format "crews-ml-pilot-%s/crews-ml-pilot-reference-%s" version version)
			 nil (list (format "%s/%s"  crews-x-top "ml-data")))
		   
		   ;;;(list (format "crews-ml-pilot-%s/crews-ml-pilot-server-%s/patches" version version)
			;;; nil (list patches-dir))
		   
		   (list (format "crews-ml-pilot-%s/crews-ml-pilot-server-%s/bin" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "roster")
				 (files-with-name distr-dir "application-controller-db")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll"))
			 nil)
		   )))
	  
	  ((string= company "stog")
	   (let ((version "1-5-1"))
	     (list (list (format "pds-%s/pds-reference-%s" version version)
			 nil (list (format "%s/%s"  crews-x-top "stog-data")))
		   (list (format "pds-%s/pds-long-term-manager-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "data-manager-eng")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "pds-%s/pds-short-term-manager-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager-st")
				 (files-with-name distr-dir "data-manager-st-eng")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "pds-%s/pds-long-term-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir  "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "scheduler-eng")
				 (files-with-name distr-dir "roster")
				 (files-with-name distr-dir "roster-eng"))
			 nil)
		   (list (format "pds-%s/pds-short-term-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler-st")
				 (files-with-name distr-dir "scheduler-st-eng")
				 (files-with-name distr-dir "allocator")
				 (files-with-name distr-dir "allocator-eng")
				 (files-with-name distr-dir "work-recorder")
				 (files-with-name distr-dir "work-recorder-eng")
				 (files-with-name distr-dir "recorder")
				 (files-with-name distr-dir "recorder-eng"))
			 nil)
		   (list (format "pds-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" dll-dir   "machine_win.dll"))
			 nil)
		   )))
	  
	  ((string= company "dsb")
	   (let ((version "2-0-0"))
	     (list (list (format "pds-dsb-%s/pds-dsb-reference-%s" version version)
			 nil (list (format "%s/%s"  crews-x-top "dsb-data")))
		   (list (format "pds-dsb-%s/pds-dsb-long-term-manager-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "data-manager-eng")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "pds-dsb-%s/pds-dsb-short-term-manager-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager-st")
				 (files-with-name distr-dir "data-manager-st-eng")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "pds-dsb-%s/pds-dsb-long-term-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir  "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "scheduler-eng")
				 (files-with-name distr-dir "roster")
				 (files-with-name distr-dir "roster-eng"))
			 nil)
		   (list (format "pds-dsb-%s/pds-dsb-short-term-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler-st")
				 (files-with-name distr-dir "scheduler-st-eng")
				 (files-with-name distr-dir "allocator")
				 (files-with-name distr-dir "allocator-eng")
				 (files-with-name distr-dir "work-recorder")
				 (files-with-name distr-dir "work-recorder-eng"))
			 nil)
		   (list (format "pds-dsb-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" dll-dir   "machine_win.dll"))
			 nil)
		   )))
	  
	  ((string= company "vr")
	   (let ((version "2-0-0"))
	     (list (list (format "vip-%s/vip-reference-%s" version version)
			 nil (list (format "%s/%s"  crews-x-top "vr-data")))
		   (list (format "vip-%s/vip-long-term-manager-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "vip-%s/vip-short-term-manager-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager-st")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "vip-%s/vip-duty-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir  "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "scheduler-eng"))
			 nil)
		   (list (format "vip-%s/vip-roster-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir  "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "roster")
				 (files-with-name distr-dir "roster-eng"))
			 nil)
		   (list (format "vip-%s/vip-short-term-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler-st")
				 (files-with-name distr-dir "scheduler-st-eng")
				 (files-with-name distr-dir "allocator")
				 (files-with-name distr-dir "allocator-eng"))
			 nil)
		   (list (format "vip-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" dll-dir   "machine_win.dll"))
			 nil)
		   )))
	  
	  ((string= company "brisa")
	   (let ((version "2-0-0"))
	     (list (list (format "brisa-%s/brisa-reference-%s" version version)
			 nil (list (format "%s/%s"  crews-x-top "brisa-data")))
		   (list (format "brisa-%s/brisa-long-term-manager-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "brisa-%s/brisa-short-term-manager-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager-st")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "brisa-%s/brisa-long-term-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir  "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
;				 (files-with-name distr-dir "scheduler-eng")
				 (files-with-name distr-dir "roster")
;				 (files-with-name distr-dir "roster-eng")
				 )
			 nil)
		   (list (format "brisa-%s/brisa-short-term-planner-%s" version version)
			 (append (list (format "%s/%s" crews-dir "macro-characters.lisp"))
				 (files-with-type dll-dir   "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler-st")
;				 (files-with-name distr-dir "scheduler-st-eng")
				 (files-with-name distr-dir "allocator")
;				 (files-with-name distr-dir "allocator-eng")
				 (files-with-name distr-dir "work-recorder")
;				 (files-with-name distr-dir "work-recorder-eng")
				 )
			 nil)
		   (list (format "brisa-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" dll-dir   "machine_win.dll"))
			 nil)
		   )))
	  
	  )))


;;;-----------------------------------------------------------------------------
;;;PROCESS-FINAL-DISTRIBUTION-OLD
;;;Description
;;;	Processes all files to locate in the distribution
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{company} is a \emph{string}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/02/27	A. Frazao	Created
;;;	00/09/06	A. Frazao	Updated location of install dir
;;;	08/07/08	A. Frazao	Changed name. Deprecated.
;;;	09/02/04	J. P. Varandas	crews-distributions -> crews-distributions-old
;;;-----------------------------------------------------------------------------
(defun process-final-distribution-old (company)
  (let ((distr-dir (format "%s/distribution" (up-directory (getenv (format "CREWS_%s_VERSION_DIR" (upcase company)))))))
    (dolist (installation (crews-distributions-old company))
      (let ((destination (format "%s/%s" distr-dir (car installation))))
	(sc-make-directory destination)
	(dolist (file (cadr installation))
	  (sc-copy-file file (format "%s/%s" destination (file-name-nondirectory file))))
	(dolist (dir (caddr installation))
	  (sc-copy-directory dir (format "%s/%s" destination (file-name-nondirectory dir))))))))


;;;-----------------------------------------------------------------------------
;;;GET-CREWS-SYSTEMS-OLD
;;;Description
;;;	Returns the systems that must be done to load an application.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{appl-image-name} is a \emph{string}, a crews application image name.
;;;		
;;;	\return-types
;;;		A list with strings to be supplied to load-crews
;;;		
;;;History
;;;	Date		Author		Description
;;;	98/12/16	A. Frazao	Generalized for more applications
;;;	98/12/22	A. Frazao	Added rsr-converter-win
;;;	99/02/12	A. Frazao	Added application-controller-db-win
;;;	99/03/15	A. Frazao	Added allocator-win
;;;	99/03/26	A. Frazao	Added rsr-converter-x
;;;	99/07/06	Joao Filipe	Added data-manager-st-win, scheduler-st-win,
;;;					roster-mt-win
;;;	00/07/26	J. P. Varandas	Added work-recorder-win
;;;	03/04/16	A. Frazao	Added dispatcher-win
;;;	03/07/28	A. Frazao	Use generic values
;;;	08/07/08	A. Frazao	Changed name. Deprecated.
;;;	09/02/04	J. P. Varandas	sc-crews-application-image-name -> sc-application-image-name
;;;					sc-find-crews-application -> sc-find-application
;;;-----------------------------------------------------------------------------
(defun get-crews-systems-old (appl-image-name)
  (if (sc-find-application appl-image-name :key 'sc-application-image-name)
      (if (string-match "-win$" system)
	  (list (format "-%s" (subseq appl-image-name 0 (match-beginning 0)))
		(format "-%s" appl-image-name))
	  (list (format "-%s" appl-image-name)))
      (error "Unknown application image name~a" appl-image-name)))


;;;-----------------------------------------------------------------------------
;;;INSTALL-CREWS-IMAGE-PROCESS-FORM-OLD
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{company} is a string
;;;		
;;;		\arg{system} is a string
;;;		
;;;		\arg{form} is a lisp form
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/27	A. Frazao	Created
;;;	99/09/01	A. Frazao	Add a sleep when launching from the Emacs
;;;	00/09/06	A. Frazao	Uses CREWS_X_VERSION_DIR
;;;	02/11/21	A. Frazao	Changed sleep to 5
;;;	08/07/08	A. Frazao	Changed name. Deprecated.
;;;	08/07/08	A. Frazao	Use 'install-crews-image-sentinel-old
;;;	09/02/10	J. P. Varandas	'install-crews-image-sentinel-old -> 'install-image-sentinel
;;;					*install-crews-with-command* -> *install-image-with-command*
;;;-----------------------------------------------------------------------------
(defun install-crews-image-process-form-old (company system form)
  (if *install-image-with-command*
      (let* ((old-buf (current-buffer))
	     (filename (format "%s-%s.cmd" company system))
	     (file (format "%s/%s" (up-directory (getenv (format "CREWS_~a_VERSION_DIR" (upcase company))) filename)))
	     (buf (get-buffer filename)))
	(when buf
	  (kill-buffer buf))
	(setq buf (find-file-noselect file))
	(set-buffer buf)
	(erase-buffer)
	(insert (format "%S" form) 10)
	(save-buffer buf)
	(kill-buffer buf)
	(set-buffer old-buf)
	(run-allegro-lisp-command nil (list "-e" (format "(load%S)" file))))
      (progn
	(allegro)
	(goto-allegro-lisp-buffer)
	(insert "(sleep 5)")
	(fi:inferior-lisp-newline)
	(insert (format "%S" form))
	(fi:inferior-lisp-newline)
	(set-process-sentinel (get-process "*Allegro CL*") 'install-image-sentinel)))
  )


;;;-----------------------------------------------------------------------------
;;;INSTALL-CREWS-IMAGE-PROCESS-SYSTEM-OLD
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a <>.
;;;		
;;;		\arg{compile-only} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	98/11/25	A. Frazao	Expanded for Lucid in Solaris Emacs
;;;	98/11/26	A. Frazao	Call crews-base-image-restart-function
;;;					in lucid
;;;	98/12/21	A. Frazao	Expand memory in Lucid
;;;	99/08/27	A. Frazao	Calls INSTALL-CREWS-IMAGE-PROCESS-FORM
;;;	00/09/06	A. Frazao	The installer is located in the
;;;					CREWS_X_VERSION_DIR
;;;	02/11/21	A. Frazao	Sleep for 5 before installation to allow
;;;					the allegro process to finish properly.
;;;	03/02/05	A. Frazao	
;;;	06/08/24	J. P. Varandas	Removed obsolete code for Solaris
;;;	08/07/08	A. Frazao	Changed name. Deprecated.
;;;	09/02/10	J. P. Varandas	*install-crews-image-company* -> *install-image-company*
;;;					*install-crews-image-no-design* -> *install-image-no-design*
;;;-----------------------------------------------------------------------------
(defun install-crews-image-process-system-old (system compile-only)
  (sleep-for 5)
  (let* ((crews-x-dir (getenv (format "CREWS_%s_VERSION_DIR" (upcase *install-image-company*))))
	 (install-dir (format "%s/bin" (up-directory crews-x-dir)))
	 (installer (format "%s/%s" crews-x-dir (format "installer-crews-%s.lisp" *install-image-company*)))
	 (keywords-dic-file (format "%s/%s" (up-directory crews-x-dir) "keywords.dic"))
	 (keywords-data-file (format "%s/%s" (up-directory crews-x-dir) "keywords.data"))
	 (siscog-dic-file (make-env-file "SISCOG_VERSION_DIR" "dictionaries/keywords.dic")))
    (let ((lambda `(let ((appl ,system))
		    ,(if compile-only
			 `(format t "~%Compiling ~a-~a~%" ,(upcase *install-image-company*) ,(upcase system))
			 `(format t "~%Installing ~a-~a~%" ,(upcase *install-image-company*) ,(upcase system)))
		    (load ,installer)
		    (load-crews-base)
		    (setf user::*crews-development-mode* ,(if *install-image-no-design* nil t))
		    (install-crews ',(get-crews-systems-old system)
		     :dir ,install-dir
		     :compile-only ,compile-only
		     :keywords-error-p nil
		     :keywords-file ,keywords-data-file
		     :dictionary-file ,keywords-dic-file
		     :no-exit t
		     :growth-limit 1800
		     :dynamic-free-segments 600
		     :reserved-free-segments 200)
		    (when (probe-file ,keywords-dic-file)
		      (format t "New keywords found. See ~a~%" ,siscog-dic-file)
		      (when (probe-file ,siscog-dic-file)
			(delete-file ,siscog-dic-file))
		      (sys::copy-file ,keywords-dic-file ,siscog-dic-file))
		    ,(cond (*windows-emacs* '(WIN:FATALEXIT 0))
			   (*linux-emacs*   '(excl::exit))))))
      (install-crews-image-process-form-old *install-image-company* system lambda))))

;;;-----------------------------------------------------------------------------
;;;INSTALL-CREWS-DISTRIBUTION-PROCESS-SYSTEM-OLD
;;;Description
;;;	Installs the image distributions
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \emph{keyword}
;;;		
;;;	\return-types
;;;		void
;;;
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	99/02/27	A. Frazao	Do not eval after distribution forms
;;;					Changed memory keyword arguments
;;;	99/08/27	A. Frazao	Calls INSTALL-CREWS-IMAGE-PROCESS-FORM
;;;	99/09/01	J. P. Varandas	Removed modules "mci", "ole", "ole-server", and "aclwin302"
;;;	99/11/20	A. Frazao	Added requires (:sock and :trace) to be
;;;					included in the distribution.
;;;	99/12/27	A. Frazao	Added requires (:loop) to be
;;;					included in the distribution.
;;;	00/01/06	A. Frazao	Include autoload warning
;;;					Sets the image date
;;;					Added require :fileutil, :inspect, :eli
;;;					and :emacs.
;;;	00/09/06	A. Frazao	The installer is located in the
;;;					CREWS_X_VERSION_DIR
;;;	01/11/08	A. Frazao	Change memory before loading
;;;	01/11/10	A. Frazao	Added load of rich-edit
;;;	02/06/07	J. P. Varandas	Added load of 'ole'
;;;	02/11/04	A. Frazao	Added changes to Allegro 6.2
;;;	02/11/21	A. Frazao	Sleep for 5 before installation to allow
;;;					the allegro process to finish properly.
;;;	06/08/14	J. P. Varandas	Added changes to Allegro 8.0
;;;	06/11/03	A. Frazao	Uses variable *sc-crews-acl-version* to identify the ACL version
;;;	06/11/07	A. Frazao	Correct setting icon for ACL 62
;;;	06/11/08	A. Frazao	Correct partition-server initialization function for ACL80
;;;	06/12/12	RAurelio	Added dispatcher-server stuff
;;;	07/04/16	A. Frazao	Uses crews.ico
;;;	08/07/08	A. Frazao	Changed name. Deprecated.
;;;	09/02/10	J. P. Varandas	*install-crews-image-company* -> *install-image-company*
;;;	09/02/10	J. P. Varandas	*sc-crews-acl-version* -> *sc-current-acl-version*
;;;	09/03/06	RAurelio	Prepare 'generate-application' to receive
;;;					custom heap values (only in ACL 8.0)
;;;	10/04/27	RAurelio	Added :streamc (support for binary streams) (POA 18591.0)
;;;-----------------------------------------------------------------------------
(defun install-crews-distribution-process-system-old (system)
  (sleep-for 5)
  (let* ((company *install-image-company*)
	 (crews-x-dir (getenv (format "CREWS_%s_VERSION_DIR" (upcase company))))
	 (installer (format "%s/installer-crews-%s.lisp" crews-x-dir company))
	 (distr-dir (format "%s/distribution/" (up-directory crews-x-dir)))
	 (icon-file (make-env-file "CREWS_VERSION_DIR" "crews.ico"))
	 (splash-from-file (crews-image-distribution-splash-from-file-old system company))
	 (path (format "%s/cg/ibls/runtime/" (getenv "ALLEGRO_CL_HOME")))
	 (path8 (format "%s/code/" (getenv "ALLEGRO_CL_HOME")))
	 (build-input (format "%sinput.txt" distr-dir))
	 (build-output (format "%soutput.txt" distr-dir))
	 (acl5-pre-load-files (mapcar '(lambda (mod)
					(format "%s%s.fasl" path mod))
				      '("cg" "drag-and-drop" "lisp-widget" "multi-picture-button" "common-control"
					"edit-in-place" "outline" "grid" "lisp-group-box" "header-control"
					"progress-indicator-control" "common-status-bar" "tab-control" "trackbar-control"
					"up-down-control" "dde" "carets" "hotspots" "menu-selection" "choose-list" 
					"directory-list" "color-dialog" "find-dialog" "font-dialog" "string-dialog" 
					"yes-no-list-dialog" "list-view-control" "rich-edit" "ole")
				      ))
	 
	 (acl6-pre-load-files (mapcar '(lambda (mod)
					(format "%s%s.fasl" path mod))
				      '("cg" "drag-and-drop" "lisp-widget" "multi-picture-button" "common-control"
					"edit-in-place" "outline" "grid" "group-box" "header-control"
					"progress-indicator-control" "common-status-bar" "tab-control" "trackbar-control"
					"up-down-control" "dde" "carets" "hotspots" "menu-selection" "choose-list" 
					"directory-list" "color-dialog" "find-dialog" "font-dialog" "string-dialog" 
					"yes-no-list-dialog" "list-view-control" "rich-edit" "ole")
				      ))
	 (acl8-pre-load-files (cons 'progn 
				    (mapcar '(lambda (mod)
					      (list 'require mod (format "%s%s.fasl" path8 (substitute 45 46 (subseq (symbol-name mod) 1)))))
					    '(:cg-dde-utils :cg.base :cg.bitmap-pane :cg.bitmap-pane.clipboard :cg.bitmap-stream :cg.button
					      :cg.caret :cg.check-box :cg.choice-list :cg.choose-printer :cg.clipboard :cg.clipboard-stack
					      :cg.clipboard.pixmap :cg.color-dialog :cg.combo-box :cg.common-control :cg.comtab :cg.cursor-pixmap
					      :cg.curve :cg.dialog-item :cg.directory-dialog :cg.directory-dialog-os :cg.drag-and-drop
					      :cg.drag-and-drop-image :cg.drawable :cg.drawable.clipboard :cg.dropping-outline :cg.edit-in-place
					      :cg.editable-text :cg.file-dialog :cg.fill-texture :cg.find-string-dialog :cg.font-dialog
					      :cg.gesture-emulation :cg.get-pixmap :cg.get-position :cg.graphics-context :cg.grid-widget 
					      :cg.grid-widget.drag-and-drop :cg.group-box :cg.header-control :cg.hotspot :cg.html-dialog
					      :cg.html-widget :cg.icon :cg.icon-pixmap :cg.ie :cg.item-list :cg.keyboard-shortcuts :cg.lamp
					      :cg.lettered-menu :cg.lisp-edit-pane :cg.lisp-text :cg.lisp-widget :cg.list-view :cg.mci :cg.menu
					      :cg.menu.tooltip :cg.message-dialog :cg.multi-line-editable-text :cg.multi-line-lisp-text
					      :cg.multi-picture-button :cg.multi-picture-button.drag-and-drop :cg.multi-picture-button.tooltip
					      :cg.ocx :cg.os-widget :cg.os-window :cg.outline :cg.outline.drag-and-drop :cg.outline.edit-in-place
					      :cg.palette :cg.paren-matching :cg.picture-widget :cg.picture-widget.palette :cg.pixmap
					      :cg.pixmap-widget :cg.pixmap.file-io :cg.pixmap.printing :cg.pixmap.rotate :cg.printing
					      :cg.progress-indicator :cg.project-window :cg.property :cg.radio-button :cg.rich-edit
					      :cg.rich-edit-pane :cg.rich-edit-pane.clipboard :cg.rich-edit-pane.printing :cg.sample-file-menu
					      :cg.scaling-stream :cg.scroll-bar :cg.scroll-bar-mixin :cg.selected-object :cg.shortcut-menu
					      :cg.static-text :cg.status-bar :cg.string-dialog :cg.tab-control :cg.template-string
					      :cg.text-edit-pane :cg.text-edit-pane.file-io :cg.text-edit-pane.mark :cg.text-or-combo
					      :cg.text-widget :cg.timer :cg.toggling-widget :cg.toolbar :cg.tooltip :cg.trackbar :cg.tray
					      :cg.up-down-control :cg.utility-dialog :cg.web-browser :cg.web-browser.dde :cg.wrap-string
					      :cg.yes-no-list :cg.yes-no-string :dde :streamc))))
	 (distribution (crews-image-distribution-name-old system company)))
    (cond (*windows-emacs*
	   (let* ((old-buf (current-buffer))
		  (filename (format "%s-%s.dist" company system))
		  (file (format "%s/%s" (up-directory crews-x-dir) filename))
		  (buf (get-buffer filename)))
	     (when buf
	       (kill-buffer buf))
	     (setq buf (find-file-noselect file))
	     (set-buffer buf)
	     (erase-buffer)
	     (insert "(in-package :user)" 10)
	     (insert "(require :trace)" 10)
	     (insert "(require :sock)" 10)
	     (insert "(require :loop)" 10)
	     (insert "(require :fileutil)" 10)
	     (insert "(require :inspect)" 10)
	     (insert "(require :eli)" 10)
	     (insert "(require :emacs)" 10)
	     (insert "#-allegro-v5.0.1 (require :mcombin)" 10)
	     (insert "#+allegro-v8.0 (require :srecord)" 10)
	     (insert "(load " 34 installer 34 ")" 10)
	     (insert "(load-crews-base)")
	     (insert "(change-allegro-memory-management 600 200 :resize t)")
	     (dolist (sys (get-crews-systems-old system))
	       (insert "(load-crews " 34 sys 34 ")" 10))
	     (insert "(setf *crews-image-date* (get-universal-time))" 10)
	     (cond ((string= system "partition-server")
		    (insert "#-allegro-v8.0 (common-graphics-user::define-project :name :crews :on-initialization 'user::partition-server-image-restart-function)" 10))
		   ((string= system "dispatcher-server")
		    (insert "#-allegro-v8.0 (common-graphics-user::define-project :name :crews :on-initialization 'user::dispatcher-server-image-restart-function)" 10))
		   (t (insert "#-allegro-v8.0 (common-graphics-user::define-project :name :crews :on-initialization 'user::crews-image-restart-function)" 10)))
	     (save-buffer buf)
	     (kill-buffer buf)
	     (set-buffer old-buf)
	     (let ((lambda (cond ((eq *sc-current-acl-version* :v8-0)
				  `(let ((program-file (pathname (format nil "~a~a.exe" ,distr-dir ,distribution))))
				    (when (probe-file program-file)
				      (delete-file program-file))
				    (apply #'generate-application
				     (append
				      (list
				       ,distribution
				       ,distr-dir
				       (list ,file)
				       :read-init-files nil 
				       :pre-load-form ',acl8-pre-load-files
				       :restart-init-function 'cg:do-default-restart 
				       :POST-LOAD-FORM
				       '(setf cg.base:.internal-session-startup-hook.
					 #'(lambda nil
					     (let* ((app (cg.base:app cg.base:*system*)))
					       (setf (cg:standalone-application app) :debuggable)
					       (setf (cg.base:initialization-function app)
						     ',(cond ((string= system "partition-server")
							      'user::partition-server-image-restart-function)
							     ((string= system "dispatcher-server")
							      'user::dispatcher-server-image-restart-function)
							     (t 'user::crews-image-restart-function)))
					       (setf (cg:kill-splash-screen-when-ready app) t)
					       (setf (cg.base:shared-libraries (cg.base:app cg.base:*system*)) nil)
					       (let* ((name '"COMMON-GRAPHICS-USER")
						      (package (and name (find-package name))))
						 (when package (setq *package* package))))))
				      
				       :runtime :dynamic
				       ;;:BUNDLE-FILE #P"D:\\Program Files\\acl80\\files.bu" 
				       :autoload-warning t
				       :allow-existing-directory t 
				       :include-ide nil 
				       :include-devel-env nil
				       :include-compiler t 
				       ;;:DISCARD-COMPILER NIL 
				       :include-tpl t
				       :include-debugger t 
				       :build-debug t 
				       :us-government nil
				       :runtime-bundle nil 
				       :discard-local-name-info t
				       :discard-source-file-info t 
				       :record-source-file-info nil
				       :load-source-file-info nil 
				       :discard-xref-info nil 
				       :record-xref-info nil 
				       :load-xref-info nil 
				       :destination-directory (pathname ,distr-dir)
				       :build-output (pathname ,build-output)
				       :splash-from-file ,(if splash-from-file
							      `(pathname ,splash-from-file))
				       :purify t 
				       :newspace 3000000 
				       :oldspace 30000000
				       :show-window :showna)
				      (list ,@(if (getenv "ACL_BUILD_LISP_HEAP_START")
					      `(:lisp-heap-start ,(getenv "ACL_BUILD_LISP_HEAP_START"))))
				      (list ,@(if (getenv "ACL_BUILD_LISP_HEAP_SIZE")
					      `(:lisp-heap-size ,(getenv "ACL_BUILD_LISP_HEAP_SIZE"))))
				      (list ,@(if (getenv "ACL_BUILD_C_HEAP_START")
					      `(:c-heap-start ,(getenv "ACL_BUILD_C_HEAP_START")))))
				     )
				    (when (probe-file ,icon-file)
				      (win::set-exe-icons-nt program-file "ACLICON" (pathname ,icon-file)))
				    (WIN:FATALEXIT 0)))
				 ((eq *sc-current-acl-version* :v6-2)
				  `(let ((program-file (pathname (format nil "~a~a.exe" ,distr-dir ,distribution))))
				    (when (probe-file program-file)
				      (delete-file program-file))
				     (excl::generate-application
				      ,distribution
				      ,distr-dir
				      (list ,file)
				      :pre-load-form '(without-package-locks (mapc 'load ',acl6-pre-load-files))
				      :restart-init-function 'cg::do-default-restart
				      :post-load-form '(cg::post-load-form :crews nil)
				      :include-ide nil
				      :include-compiler t 
				      :us-government nil 
				      :presto nil 
				      :discard-local-name-info t 
				      :discard-source-file-info t
				      ;; ACL 6.2
				      :verbose t
				      :build-input ,build-input
				      :build-output ,build-output
				      
				      :debug t
				      :build-debug t;;:interactive
				      :runtime :dynamic
				      
				      :include-devel-env nil 
				      :copy-shared-libraries t
				      :destination-directory (pathname ,distr-dir)
				      :allow-existing-directory t
				      :include-tpl t
				      :splash-from-file ,(if splash-from-file
							     `(pathname ,splash-from-file))
				      :purify t
				      :newspace 3000000 
				      :oldspace 30000000
				      ;; Use :normal if in the background
				      :show-window :showna
				      ;; Use the following for :autoload-warning to examine if more "requires" are
				      ;; necessary to include in the distribution
				      ;; (format nil "~aauto-loads-~a.txt" ,distr-dir ,distribution)
				      :autoload-warning nil
				      )
				     (when (probe-file ,icon-file)
				      (let ((temp-file (pathname (format nil "~a~a-temp.exe" ,distr-dir ,distribution))))
					(win::set-exe-icons program-file temp-file "ACLICON" (pathname ,icon-file))
					(delete-file program-file)
					(sys::copy-file temp-file program-file)
					(delete-file temp-file)))
				    (WIN:FATALEXIT 0)))
				 ((eq *sc-current-acl-version* :v5-0-1)
				  `(let ((program-file (pathname (format nil "~a~a.exe" ,distr-dir ,distribution))))
				    (when (probe-file program-file)
				      (delete-file program-file))
				     (excl::generate-application
				      ,distribution
				      ,distr-dir
				      (list ,file)
				      :pre-load-form '(without-package-locks (mapc 'load ',acl5-pre-load-files))
				      :restart-init-function 'cg::do-default-restart
				      :post-load-form '(setf (cg::initialization-function (cg::app cg::*system*))
							',(cond ((string= system "partition-server")
							   'user::partition-server-image-restart-function)
							  ((string= system "dispatcher-server")
							   'user::dispatcher-server-image-restart-function)
							  (t 'user::crews-image-restart-function))) ;;RA
				      :include-ide nil
				      :include-compiler t 
				      :us-government nil 
				      :presto nil 
				      :debug-on-error t
				      :discard-local-name-info t 
				      :discard-source-file-info t
				      :exit-after-image-build t
				      ;; ACL 5.0.1
				      :include-common-graphics nil
				      
				      :include-devel-env nil 
				      :copy-shared-libraries t
				      :destination-directory (pathname ,distr-dir)
				      :allow-existing-directory t
				      :include-tpl t
				      :splash-from-file ,(if splash-from-file
							     `(pathname ,splash-from-file))
				      :purify t
				      :newspace 3000000 
				      :oldspace 30000000
				      ;; Use nil if in the background
				      :show-window nil
				      ;; Use the following for :autoload-warning to examine if more "requires" are
				      ;; necessary to include in the distribution
				      ;; (format nil "~aauto-loads-~a.txt" ,distr-dir ,distribution)
				      :autoload-warning nil
				      )
				    (when (probe-file ,icon-file)
				      (let ((temp-file (pathname (format nil "~a~a-temp.exe" ,distr-dir ,distribution))))
					(win::set-exe-icons program-file temp-file "ACL5ICON" (pathname ,icon-file))
					(delete-file program-file)
					(sys::copy-file temp-file program-file)
					(delete-file temp-file)))
				    (WIN:FATALEXIT 0))))))
	       (install-crews-image-process-form-old company system lambda)
	       )))
	  )))
