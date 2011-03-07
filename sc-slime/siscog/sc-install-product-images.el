;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2009, SISCOG - Sistemas Cognitivos Lda.
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
;;;	09/02/10	J. P. Varandas	Created
;;;	09/05/29	F. Cantarinha	Changed definitions
;;;					  CREWS-DISTRIBUTIONS
;;;	09/10/26	A. Frazao	Changed definitions
;;;					  ONTIME-DISTRIBUTIONS
;;;					  FLEET-DISTRIBUTIONS
;;;					  CREWS-DISTRIBUTIONS
;;;	09/10/30	A. Frazao	Changed definitions
;;;					  CREWS-DISTRIBUTIONS
;;;	10/06/25	Ruben Vaz	Changed definitions
;;;					  CREWS-DISTRIBUTIONS
;;;	10/07/08	A. Frazao	Changed definitions
;;;					  CREWS-DISTRIBUTIONS
;;;	10/07/27	F. Cantarinha	Changed definitions
;;;					  CREWS-DISTRIBUTIONS
;;;	10/09/22	A. Frazao	Changed definitions
;;;					  CREWS-DISTRIBUTIONS
;;;	10/11/10	F. Cantarinha	Changed definitions
;;;					  CREWS-DISTRIBUTIONS
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;				PRODUCT CREWS
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;CREWS-DISTRIBUTIONS
;;;Description
;;;	Specifies the distribution description.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{company} is a \emph{string}.
;;;		
;;;	\return-types
;;;		A \emph{list} of \emph{list}s.
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
;;;	08/07/08	A. Frazao	New environment variables usage
;;;	08/08/01	Fausto		Added "web-server".
;;;	08/10/30	A. Frazao	Added "admin-rtd-service.exe" in "crews-siscog-distribution"
;;;	09/01/22	F. Cantarinha	Changed version of TPO from v2-4-0 to 
;;;					 v2-5-0. (POA 14968.0)
;;;	09/05/29	F. Cantarinha	Changed version of VR from v2-0-0 to 
;;;					v3-0-0. (POA 15844.0)
;;;	09/10/26	A. Frazao	Copy also DLLs from siscog dir
;;;	09/10/30	A. Frazao	Added webcontents in CREWS-SISCOG
;;;	10/06/25	Ruben Vaz	Changed version of PDS from v1-5-1 to
;;;					v2-1-0. (POA.17909.0.3.2)
;;;	10/07/08	A. Frazao	Updated for CREWS-NS
;;;	10/07/27	F. Cantarinha	Updated for CREWS-VR. (POA 19370.0)
;;;	10/09/22	A. Frazao	Removed work recorder
;;;	10/11/10	F. Cantarinha	Changed crews-vr version from 3-1-1
;;;					to 3-2-0. (OT.862.6)
;;;-----------------------------------------------------------------------------
(defun crews-distributions (company)
  (let* ((siscog-dir  (getenv "SISCOG_UTIL_DIR"))
	 (crews-dir   (getenv "CREWS_DIR"))
	 (crews-x-dir (getenv (format "CREWS_%s_DIR" (upcase company))))
	 (siscog-web-dirs  (sc-directories (format "%s/siscog-util/webcontent" siscog-dir)))
	 (crews-web-dirs   (sc-directories (format "%s/crews/webcontent" crews-dir)))
	 (crews-x-web-dirs (sc-directories (format "%s/crews-%s/webcontent" crews-x-dir company)))
	 (distr-dir   (format "%s/distribution" crews-x-dir))
	 (crews-bin   (format "%s/bin" crews-dir))
	 (data-dir    (format "%s/%s-data" crews-x-dir company))
	 (patches-dir (format "%s/patches/crews-%s" crews-x-dir company))
	 (siscog-dll-dir (format "%s/dll" siscog-dir))
	 (crews-dll-dir  (format "%s/dll" crews-dir))
	 (pack-dir    (format "%s/crews/data-organization" crews-dir)))
    (cond ((string= company "siscog")
	   (list (list "crews-siscog-distribution"
		       (append (list (format "%s/%s" crews-bin "admin-rtd-service.exe")
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
			       (files-with-name distr-dir "web-server")
			       (files-with-name distr-dir "dispatcher")
			       (files-with-name distr-dir "application-controller-db")
			       (files-with-type siscog-dll-dir "dll")
			       (files-with-type crews-dll-dir "dll")
			       (files-with-type distr-dir "dll")
			       (files-with-name distr-dir "dispatcher-server"))
		       nil)
		 (list "webcontent"
		       nil
		       (append siscog-web-dirs
			       crews-web-dirs
			       crews-x-web-dirs))
		 ))
	  ((string= company "cp")
	   (let ((version "2-0-0"))
	     (list (list (format "escalas-server-%s/escalas-%s/bin" version version)
			 (append (list (format "%s/%s" crews-bin "appl-controller.exe")
				       (format "%s/%s" crews-bin "applmgr-server.exe")
				       (format "%s/%s" crews-bin "launcher-server.exe")
				       (format "%s/%s" crews-bin "pcserver.exe")
				       )
				 (files-with-name distr-dir "application-controller-db")
				 (files-with-name distr-dir "cp-partition-server")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll"))
			 nil)
		   (list (format "escalas-server-%s/escalas-%s/crews5-0/dll" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll"))
			 nil)
		   (list (format "escalas-server-%s/escalas-%s" version version)
			 nil (list data-dir))
		   (list (format "escalas-server-%s/escalas-%s/patches" version version)
			 nil (list patches-dir))
		   (list (format "escalas-client-manager-%s/escalas-%s/bin" version version)
			 (append (list (format "%s/%s" crews-bin "appl-controller.exe"))
				 (files-with-type pack-dir  "bat")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "roster")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "escalas-client-planner-%s/escalas-%s/bin" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "roster"))
			 nil)
		   (list "univocal-path-server"
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" crews-dll-dir   "machine_win.dll")
			       (format "%s/%s" crews-dll-dir   "msvcrtd.dll"))
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
		 (updates-dir (format "%s/updates" crews-x-dir company)))
	     (list ;; UNIVOCAL PATH SERVER distribution
	      (list "univocal-path-server"
		    (list (format "%s/%s" crews-bin "upserver-server.exe")
			  (format "%s/%s" crews-bin "sc.exe")
			  (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			  (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			  (format "%s/%s" crews-dll-dir   "machine_win.dll")
			  (format "%s/%s" crews-dll-dir   "msvcrtd.dll"))
		    nil)
	      ;; SERVER distribution
	      (list (format "crews-wagn-server-%s/crews-wagn-%s/bin" version version)
		    (append (list (format "%s/%s" crews-bin "appl-controller.exe")
				  (format "%s/%s" crews-bin "applmgr-server.exe")
				  (format "%s/%s" crews-bin "launcher-server.exe")
				  (format "%s/%s" crews-bin "pcserver.exe"))
			    (files-with-name distr-dir "wagn-partition-server")
			    (files-with-type siscog-dll-dir "dll")
			    (files-with-type crews-dll-dir "dll")
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
		    (append (list (format "%s/%s" crews-bin "appl-controller.exe"))
			    (files-with-type pack-dir  "bat")
			    (files-with-name distr-dir "rsr-converter")
			    (files-with-name distr-dir "data-manager")
			    (files-with-name distr-dir "scheduler")
			    (files-with-type siscog-dll-dir "dll")
			    (files-with-type crews-dll-dir "dll")
			    (files-with-type distr-dir "dll"))
		    nil)
	      ;; CLIENT PLANNER distribution
	      (list (format "crews-wagn-client-planner-%s/crews-wagn-%s/bin" version version)
		    (append (files-with-name distr-dir "scheduler")
			    (files-with-type siscog-dll-dir "dll")
			    (files-with-type crews-dll-dir "dll")
			    (files-with-type distr-dir "dll"))
		    nil)
	      )))
	  
	  ((string= company "ns")
	   (let ((version "v5-0-0"))
	     (list (list (format "crews-ns-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" crews-dll-dir   "machine_win.dll")
			       (format "%s/%s" crews-dll-dir   "msvcrtd.dll"))
			 nil)
		   (list (format "crews-ns-%s" version)
			 nil (list (format "%s/crews-ns" crews-x-dir)))
		   (list (format "crews-ns-%s/ns-data" version)
			 nil (list data-dir))
		   (list (format "crews-ns-%s/patches" version)
			 nil (list patches-dir))
		   (list (format "crews-ns-%s/patches" version)
			 nil (list patches-x-dir))
		   (list (format "crews-ns-%s/bin" version)
			 (append (list (format "%s/%s" crews-bin "appl-controller.exe")
				       (format "%s/%s" crews-bin "applmgr-server.exe")
				       (format "%s/%s" crews-bin "launcher-server.exe")
				       (format "%s/%s" crews-bin "pcserver.exe"))
				 (files-with-name distr-dir "partition-server")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "scheduler-dutch")
				 (files-with-name distr-dir "data-manager-st")
				 (files-with-name distr-dir "dispatcher")
				 (files-with-name distr-dir "dispatcher-server")
				 (files-with-name distr-dir "application-controller-db")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll"))
			 nil)
		   (list (format "crews-ns-%s/webcontent" version)
			 nil
			 (append siscog-web-dirs
				 crews-web-dirs
				 crews-x-web-dirs))
		   )))
	  
	  ((string= company "nsb")
	   (let ((version "2-5-0"))
	     (list (list (format "tpo-%s/tpo-reference-%s" version version)
			 nil (list (format "%s/%s"  crews-x-dir "nsb-data")))
		   
		   (list (format "tpo-%s/tpo-application-controller-%s" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   
		   (list (format "tpo-%s/tpo-long-term-manager-%s" version version)
			 (append (files-with-type pack-dir  "bat")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager"))
			 nil)
		   (list (format "tpo-%s/tpo-short-term-manager-%s" version version)
			 (append (files-with-type pack-dir  "bat")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager-st"))
			 nil)
		   (list (format "tpo-%s/tpo-long-term-planner-%s" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "roster"))
			 nil)
		   
		   (list (format "tpo-%s/tpo-short-term-planner-%s" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler-st")
				 (files-with-name distr-dir "allocator"))
			 nil)
		   
		   (list (format "tpo-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" crews-dll-dir   "machine_win.dll"))
			 nil)
		   )))
	  ((string= company "ml")
	   (let ((version "1-0-0"))
	     (list (list (format "crews-ml-pilot-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" crews-dll-dir   "machine_win.dll")
			       (format "%s/%s" crews-dll-dir   "msvcrtd.dll"))
			 nil)
		   (list (format "crews-ml-pilot-%s/crews-ml-pilot-reference-%s" version version)
			 nil (list (format "%s/%s"  crews-x-dir "ml-data")))
		   
		   ;;;(list (format "crews-ml-pilot-%s/crews-ml-pilot-server-%s/patches" version version)
			;;; nil (list patches-dir))
		   
		   (list (format "crews-ml-pilot-%s/crews-ml-pilot-server-%s/bin" version version)
			 (append (files-with-type pack-dir  "bat")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "roster")
				 (files-with-name distr-dir "application-controller-db")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll"))
			 nil)
		   )))
	  
	  ((string= company "stog")
	   (let ((version "2-1-0"))
	     (list (list (format "pds-%s/pds-reference-%s" version version)
			 nil (list (format "%s/%s" crews-x-dir "stog-data")))
		   (list (format "pds-%s/pds-long-term-manager-%s" version version)
			 (append (files-with-type pack-dir  "bat")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "pds-%s/pds-short-term-manager-%s" version version)
			 (append (files-with-type pack-dir  "bat")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager-st")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "pds-%s/pds-long-term-planner-%s" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "roster"))
			 nil)
		   (list (format "pds-%s/pds-short-term-planner-%s" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (list (format "%s/%s" crews-bin "admin-rtd-service.exe"))
				 (files-with-type distr-dir "dispatcher")
				 (files-with-name distr-dir "scheduler-st")
				 (files-with-name distr-dir "allocator")
				 (files-with-name distr-dir "recorder")
				 (files-with-name distr-dir "dispatcher-server"))
			 nil)
		   (list (format "pds-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" crews-dll-dir "machine_win.dll"))
			 nil)
		   )))
	  
	  ((string= company "dsb")
	   (let ((version "2-0-0"))
	     (list (list (format "pds-dsb-%s/pds-dsb-reference-%s" version version)
			 nil (list (format "%s/%s"  crews-x-dir "dsb-data")))
		   (list (format "pds-dsb-%s/pds-dsb-long-term-manager-%s" version version)
			 (append (files-with-type pack-dir  "bat")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "data-manager-eng")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "pds-dsb-%s/pds-dsb-short-term-manager-%s" version version)
			 (append (files-with-type pack-dir  "bat")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager-st")
				 (files-with-name distr-dir "data-manager-st-eng")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "pds-dsb-%s/pds-dsb-long-term-planner-%s" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
				 (files-with-name distr-dir "scheduler-eng")
				 (files-with-name distr-dir "roster")
				 (files-with-name distr-dir "roster-eng"))
			 nil)
		   (list (format "pds-dsb-%s/pds-dsb-short-term-planner-%s" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler-st")
				 (files-with-name distr-dir "scheduler-st-eng")
				 (files-with-name distr-dir "allocator")
				 (files-with-name distr-dir "allocator-eng"))
			 nil)
		   (list (format "pds-dsb-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" crews-dll-dir   "machine_win.dll"))
			 nil)
		   )))
	  
	  ((string= company "vr")
	   (let ((version "3-2-0"))
	     (list
	      (list (format "vip-%s/vip-reference-%s" version version)
		    nil (list (format "%s/%s"  crews-x-dir "vr-data")))
	      (list (format "vip-%s/vip-long-term-manager-%s" version version)
		    (append (files-with-type pack-dir  "bat")
			    (files-with-type siscog-dll-dir "dll")
			    (files-with-type crews-dll-dir "dll")
			    (files-with-type distr-dir "dll")
			    (files-with-name distr-dir "data-manager")
			    (files-with-name distr-dir "application-controller-db"))
		    nil)	
	      (list (format "vip-%s/vip-short-term-manager-%s" version version)
		    (append (files-with-type pack-dir  "bat")
			    (files-with-type siscog-dll-dir "dll")
			    (files-with-type crews-dll-dir "dll")
			    (files-with-type distr-dir "dll")
			    (files-with-name distr-dir "data-manager-st")
			    (files-with-name distr-dir "application-controller-db"))
		    nil)
	      (list (format "vip-%s/vip-duty-planner-%s" version version)
		    (append (files-with-type siscog-dll-dir "dll")
			    (files-with-type crews-dll-dir "dll")
			    (files-with-type distr-dir "dll")
			    (files-with-name distr-dir "scheduler")
			    (files-with-name distr-dir "scheduler-eng"))
		    nil)
	      (list (format "vip-%s/vip-roster-planner-%s" version version)
		    (append (files-with-type siscog-dll-dir "dll")
			    (files-with-type crews-dll-dir "dll")
			    (files-with-type distr-dir "dll")
			    (files-with-name distr-dir "roster")
			    (files-with-name distr-dir "roster-eng"))
		    nil)
	      (list (format "vip-%s/vip-short-term-planner-%s" version version)
		    (append (files-with-type siscog-dll-dir "dll")
			    (files-with-type crews-dll-dir "dll")
			    (files-with-type distr-dir "dll")
			    (files-with-name distr-dir "scheduler-st")
			    (files-with-name distr-dir "scheduler-st-eng")
			    (files-with-name distr-dir "allocator")
			    (files-with-name distr-dir "allocator-eng"))
		    nil)
	      (list (format "vip-%s/vip-dispatcher-%s" version version)
		    (append (files-with-type siscog-dll-dir "dll")
			    (files-with-type crews-dll-dir "dll")
			    (files-with-type distr-dir "dll")
			    (files-with-name distr-dir "dispatcher")
			    (files-with-name distr-dir "dispatcher-eng"))
		    nil)
	      (list (format "vip-%s/univocal-path-server" version)
		    (list (format "%s/%s" crews-bin "upserver-server.exe")
			  (format "%s/%s" crews-bin "sc.exe")
			  (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			  (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			  (format "%s/%s" crews-dll-dir "machine_win.dll"))
		    nil)
	      (list (format "vip-%s/vip-rtd-server-%s" version version)
		    (append
		     (list (format "%s/%s" crews-bin "admin-rtd-service.exe"))
		     (files-with-name distr-dir "dispatcher-server")
		     (files-with-type siscog-dll-dir "dll")
		     (files-with-type crews-dll-dir "dll")
		     (files-with-type distr-dir "dll"))
		    nil)
	      (list (format "vip-%s/vip-web-server-%s" version version)
		    (append
		     (files-with-type siscog-dll-dir "dll")
		     (files-with-type crews-dll-dir "dll")
		     (files-with-type distr-dir "dll")
		     (files-with-name distr-dir "web-server")
		     (files-with-name distr-dir "web-server-eng")
		     )
		    nil)
	      ))
	   )
	  
	  ((string= company "brisa")
	   (let ((version "2-0-0"))
	     (list (list (format "brisa-%s/brisa-reference-%s" version version)
			 nil (list (format "%s/%s"  crews-x-dir "brisa-data")))
		   (list (format "brisa-%s/brisa-long-term-manager-%s" version version)
			 (append (files-with-type pack-dir  "bat")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "brisa-%s/brisa-short-term-manager-%s" version version)
			 (append (files-with-type pack-dir  "bat")
				 (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "data-manager-st")
				 (files-with-name distr-dir "application-controller-db"))
			 nil)
		   (list (format "brisa-%s/brisa-long-term-planner-%s" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler")
;				 (files-with-name distr-dir "scheduler-eng")
				 (files-with-name distr-dir "roster")
;				 (files-with-name distr-dir "roster-eng")
				 )
			 nil)
		   (list (format "brisa-%s/brisa-short-term-planner-%s" version version)
			 (append (files-with-type siscog-dll-dir "dll")
				 (files-with-type crews-dll-dir "dll")
				 (files-with-type distr-dir "dll")
				 (files-with-name distr-dir "scheduler-st")
;				 (files-with-name distr-dir "scheduler-st-eng")
				 (files-with-name distr-dir "allocator")
;				 (files-with-name distr-dir "allocator-eng")
				 )
			 nil)
		   (list (format "brisa-%s/univocal-path-server" version)
			 (list (format "%s/%s" crews-bin "upserver-server.exe")
			       (format "%s/%s" crews-bin "sc.exe")
			       (format "%s/%s" crews-bin "create-univocal-path-service.exe")
			       (format "%s/%s" crews-bin "delete-univocal-path-service.bat")
			       (format "%s/%s" crews-dll-dir   "machine_win.dll"))
			 nil)
		   )))
	  
	  )))


;;;-----------------------------------------------------------------------------
;;;INSTALL-CREWS-IMAGE
;;;Description
;;;	Processes (compile or install) CREWS applications (see function documentation)
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{str} is a \emph{string}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	98/11/26	A. Frazao	Interactive calling is now activated
;;;	08/07/08	A. Frazao	Removed deletion of keyword files
;;;					Allow old image creation procedure for
;;;					compatibility
;;;	08/07/08	A. Frazao	Corrected argument checking
;;;	08/07/31	A. Frazao	Doc updated
;;;	08/08/01	A. Frazao	Corrected documentation
;;;	09/02/10	J. P. Varandas	Use the functionality of 'install-images'
;;;-----------------------------------------------------------------------------
(defun install-crews-image (str)
  "Processes (compile or install) CREWS applications
STR is a string that represents the options is composed by a combination of the following options:

-co <company> - specifies the company for which the next systems should be are processed
                company is one of: ns cp nsb wagn
-dic          - specifies the behaviour when a new dictionary keyword is found while
                compiling code. If this option is not supplied an error is signaled in
                such cases. If this option is supplied, the system collect the keywords
                in two files below the directory given by CREWS_<X>_DIR. The files are
                keywords.dic, in a dictionary format, and keywords.data, with the
                identification of the source file where the keywords were found.
                Additionally, when the file keywords.dic is found it is loaded at the
                begginning. This way a keyword appears only once. To identify all the
                keywords not translated, these files should be deleted, all the code
                should be completelly recompiled using this option.
-c <system>   - specifies a system image to be compiled only (no image is created)
                for the current company (specified in the last -co option)
-i <system>   - specifies a system image to be created for the current company
                (specified in the last -co option)
-d <system>   - specifies a system distribution to be created for the current company
                (specified in the last -co option)
-ci  <system> - They are combinations of the previous arguments. They correspond to process
-cid <system>   sequencially the individual arguments for the specified system for
-id  <system>	the current company (specified in the last -co option)
		Example: -ci  <system> is the same as -c <system> -i <system>
-nd           - in the next processes the variable *CREWS-DEVELOPMENT-MODE* is set to T
-dp {<str>|-} - specifies a postfix to add to the image name:
                if the following parameter is '-', unsets the prefix;
                otherwise, uses the postfix in the next images.
-cmd          - Uses command mode to generate the distribution (not in Emacs)
-pu           - deletes all the binary files in the CREWS_VERSION_DIR, SISCOG_VERSION_DIR
                and the current company version directory (specified in the last -co option)

NOTE: All the arguments are processed sequencially.

Example:
  \"-co nsb -pu -c scheduler-win -i scheduler-win -co cp -pu -ci scheduler-win\"
   Does the following:
     Sets the current company to nsb
     Cleans all lisp compiled files (for CREWS and NSB)
     Compiles the scheduler-win code for NSB
     Creates an image for the scheduler-win for NSB
     Sets the current company to cp
     Cleans all lisp compiled files (for CREWS and CP)
     Compiles the scheduler-win code for CP and creates the corresponding image
"
  (install-images (format "-prod crews %s" str))
  )


;;;-----------------------------------------------------------------------------
;;;				PRODUCT FLEET
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;FLEET-DISTRIBUTIONS
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
;;;	09/02/10	J. P. Varandas	Created
;;;	09/10/26	A. Frazao	Copy also DLLs from siscog dir
;;;-----------------------------------------------------------------------------
(defun fleet-distributions (company)
  (let* ((siscog-dir     (getenv "SISCOG_UTIL_DIR"))
	 (fleet-dir      (getenv "FLEET_DIR"))
	 (fleet-x-dir    (getenv (format "FLEET_%s_DIR" (upcase company))))
	 (distr-dir      (format "%s/distribution" fleet-x-dir))
	 (siscog-dll-dir (format "%s/dll" siscog-dir))
	 (fleet-dll-dir  (format "%s/dll" fleet-dir)))
    (cond ((string= company "siscog")
	   (list (list "fleet-siscog-distribution"
		       (append (files-with-name distr-dir "data-manager")
			       (files-with-name distr-dir "fplanner")
			       (files-with-name distr-dir "application-controller-db")
			       (files-with-type siscog-dir "dll")
			       (files-with-type fleet-dll-dir "dll")
			       (files-with-type distr-dir "dll")
			       )
		       nil)
		 )))))


;;;-----------------------------------------------------------------------------
;;;INSTALL-FLEET-IMAGE
;;;Description
;;;	Processes (compile or install) FLEET applications (see function documentation)
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{str} is a \emph{string}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	98/11/26	A. Frazao	Interactive calling is now activated
;;;	08/07/08	A. Frazao	Removed deletion of keyword files
;;;					Allow old image creation procedure for
;;;					compatibility
;;;	08/07/08	A. Frazao	Corrected argument checking
;;;	08/07/31	A. Frazao	Doc updated
;;;	08/08/01	A. Frazao	Corrected documentation
;;;	09/02/10	J. P. Varandas	Use the functionality of 'install-images'
;;;-----------------------------------------------------------------------------
(defun install-fleet-image (str)
  "Processes (compile or install) FLEET applications
STR is a string that represents the options is composed by a combination of the following options:

-co <company> - specifies the company for which the next systems should be are processed
                company is one of: ns cp nsb wagn
-dic          - specifies the behaviour when a new dictionary keyword is found while
                compiling code. If this option is not supplied an error is signaled in
                such cases. If this option is supplied, the system collect the keywords
                in two files below the directory given by FLEET_<X>_DIR. The files are
                keywords.dic, in a dictionary format, and keywords.data, with the
                identification of the source file where the keywords were found.
                Additionally, when the file keywords.dic is found it is loaded at the
                begginning. This way a keyword appears only once. To identify all the
                keywords not translated, these files should be deleted, all the code
                should be completelly recompiled using this option.
-c <system>   - specifies a system image to be compiled only (no image is created)
                for the current company (specified in the last -co option)
-i <system>   - specifies a system image to be created for the current company
                (specified in the last -co option)
-d <system>   - specifies a system distribution to be created for the current company
                (specified in the last -co option)
-ci  <system> - They are combinations of the previous arguments. They correspond to process
-cid <system>   sequencially the individual arguments for the specified system for
-id  <system>	the current company (specified in the last -co option)
		Example: -ci  <system> is the same as -c <system> -i <system>
-nd           - in the next processes the variable *DEVELOPMENT-MODE* is set to T
-dp {<str>|-} - specifies a postfix to add to the image name:
                if the following parameter is '-', unsets the prefix;
                otherwise, uses the postfix in the next images.
-cmd          - Uses command mode to generate the distribution (not in Emacs)
-pu           - deletes all the binary files in the FLEET_VERSION_DIR, SISCOG_VERSION_DIR
                and the current company version directory (specified in the last -co option)

NOTE: All the arguments are processed sequencially.

Example:
  \"-co siscog -pu -c data-manager -i fplanner\"
   Does the following:
     Sets the current company to siscog
     Cleans all lisp compiled files (for FLEET and SISCOG)
     Compiles the data-manager code for SISCOG
     Creates an image for the fplanner for SISCOG
"
  (install-images (format "-prod fleet %s" str))
  )


;;;-----------------------------------------------------------------------------
;;;				PRODUCT ONTIME
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;ONTIME-DISTRIBUTIONS
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
;;;	09/02/10	J. P. Varandas	Created
;;;	09/10/26	A. Frazao	Copy also DLLs from siscog dir
;;;-----------------------------------------------------------------------------
(defun ontime-distributions (company)
  (let* ((siscog-dir   (getenv "SISCOG_UTIL_DIR"))
	 (ontime-dir   (getenv "ONTIME_DIR"))
	 (ontime-x-dir (getenv (format "ONTIME_%s_DIR" (upcase company))))
	 (distr-dir    (format "%s/distribution" ontime-x-dir))
	 (siscog-dll-dir (format "%s/dll" siscog-dir))
	 (ontime-dll-dir (format "%s/dll" ontime-dir)))
    (cond ((string= company "siscog")
	   (list (list "ontime-siscog-distribution"
		       (append (files-with-name distr-dir "data-manager")
			       ;;(files-with-name distr-dir "")
			       (files-with-name distr-dir "application-controller-db")
			       (files-with-type siscog-dll-dir "dll")
			       (files-with-type ontime-dll-dir "dll")
			       (files-with-type distr-dir "dll")
			       )
		       nil)
		 )))))


;;;-----------------------------------------------------------------------------
;;;INSTALL-ONTIME-IMAGE
;;;Description
;;;	Processes (compile or install) ONTIME applications (see function documentation)
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{str} is a \emph{string}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	98/11/26	A. Frazao	Interactive calling is now activated
;;;	08/07/08	A. Frazao	Removed deletion of keyword files
;;;					Allow old image creation procedure for
;;;					compatibility
;;;	08/07/08	A. Frazao	Corrected argument checking
;;;	08/07/31	A. Frazao	Doc updated
;;;	08/08/01	A. Frazao	Corrected documentation
;;;	09/02/10	J. P. Varandas	Use the functionality of 'install-images'
;;;-----------------------------------------------------------------------------
(defun install-ontime-image (str)
  "Processes (compile or install) ONTIME applications
STR is a string that represents the options is composed by a combination of the following options:

-co <company> - specifies the company for which the next systems should be are processed
                company is one of: ns cp nsb wagn
-dic          - specifies the behaviour when a new dictionary keyword is found while
                compiling code. If this option is not supplied an error is signaled in
                such cases. If this option is supplied, the system collect the keywords
                in two files below the directory given by ONTIME_<X>_DIR. The files are
                keywords.dic, in a dictionary format, and keywords.data, with the
                identification of the source file where the keywords were found.
                Additionally, when the file keywords.dic is found it is loaded at the
                begginning. This way a keyword appears only once. To identify all the
                keywords not translated, these files should be deleted, all the code
                should be completelly recompiled using this option.
-c <system>   - specifies a system image to be compiled only (no image is created)
                for the current company (specified in the last -co option)
-i <system>   - specifies a system image to be created for the current company
                (specified in the last -co option)
-d <system>   - specifies a system distribution to be created for the current company
                (specified in the last -co option)
-ci  <system> - They are combinations of the previous arguments. They correspond to process
-cid <system>   sequencially the individual arguments for the specified system for
-id  <system>	the current company (specified in the last -co option)
		Example: -ci  <system> is the same as -c <system> -i <system>
-nd           - in the next processes the variable *DEVELOPMENT-MODE* is set to T
-dp {<str>|-} - specifies a postfix to add to the image name:
                if the following parameter is '-', unsets the prefix;
                otherwise, uses the postfix in the next images.
-cmd          - Uses command mode to generate the distribution (not in Emacs)
-pu           - deletes all the binary files in the ONTIME_VERSION_DIR, SISCOG_VERSION_DIR
                and the current company version directory (specified in the last -co option)

NOTE: All the arguments are processed sequencially.

Example:
  \"-co siscog -pu -c data-manager\"
   Does the following:
     Sets the current company to siscog
     Cleans all lisp compiled files (for ONTIME and SISCOG)
     Compiles the data-manager code for SISCOG
     Creates an image for the fplanner for SISCOG
"
  (install-images (format "-prod ontime %s" str))
  )


