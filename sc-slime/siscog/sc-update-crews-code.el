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
;;;	
;;; History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	05/11/11	Carlos Ribeiro	Changed definitions
;;;					  (SETQ *MAKE-SISCOG-NAMES*)
;;;	06/08/24	J. P. Varandas	Changed definitions
;;;					  (SETQ *MAKE-SISCOG-NAMES*)
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  MAKE-OR-UPDATE-C-AND-CPP-CREWS-DATABASE
;;;					  MAKE-OR-UPDATE-C-AND-CPP-CREWS-MULTI-USER
;;;					  UPDATE-SISCOG-PATH
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;*MAKE-SISCOG-NAMES*
;;;Description
;;;	This variable contains all the paths bellow CREWS_DIR that are used by
;;;	MAKE-SISCOG and UPDATE-SISCOG
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar *make-siscog-names* nil)

;;;-----------------------------------------------------------------------------
;;;(SETQ *MAKE-SISCOG-NAMES*)
;;;Description
;;;	Default values for *MAKE-SISCOG-NAMES*
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	99/02/27	A. Frazao	Added new binaries
;;;	00/10/10	A. Frazao	Updated to the new structure.
;;;	01/09/27	A. Vasconcelos	Updated for CREWS_ML and CREWS_DSB.
;;;	02/01/13	Fausto		Updated for CREWS_STOG.
;;;	02/01/31	Pedro Matos	Updated for CREWS_VR
;;;	02/05/21	Fausto		Updated for CREWS_DSB.
;;;	02/07/18	Carlos Ribeiro	Updated for CREWS_BRISA.
;;;	02/12/09	A. Frazao	Updated for CREWS_SISCOG
;;;	05/11/11	Carlos Ribeiro	Updated for CREWS_LUL.
;;;	06/08/24	J. P. Varandas	Removed obsolete code for Solaris
;;;-----------------------------------------------------------------------------
(setq *make-siscog-names* '("crews-vdev/crews"
			    "crews-vdev/siscog-util"
				    
			    "crews-siscog-vdev/crews-siscog"
			    "crews-siscog-vdev/siscog-data/siscog"
				    
			    "crews-wagn-vdev/crews-wagn"
			    "crews-wagn-vdev/wagn-data/wagn"
				    
			    "crews-ns-vdev/crews-ns"
			    "crews-ns-vdev/ns-data/ns"
				    
			    "crews-cp-vdev/crews-cp"
			    "crews-cp-vdev/cp-data/cp"
				    
			    "crews-nsb-vdev/crews-nsb"
			    "crews-nsb-vdev/nsb-data/nsb"
				    
			    "crews-ml-vdev/crews-ml"
			    "crews-ml-vdev/ml-data/ml"
				    
			    "crews-stog-vdev/crews-stog"
			    "crews-stog-vdev/stog-data/stog"
				    
			    "crews-dsb-vdev/crews-dsb"
			    "crews-dsb-vdev/dsb-data/dsb"
				    
			    "crews-vr-vdev/crews-vr"
			    "crews-vr-vdev/vr-data/vr"

			    "crews-brisa-vdev/crews-brisa"
			    "crews-brisa-vdev/brisa-data/brisa"

			    "crews-lul-vdev/crews-lul"
			    "crews-lul-vdev/lul-data/lul"

			    "crews-vdev/bin/upserver-server.exe"
			    "crews-vdev/bin/sc.exe"
			    "crews-vdev/bin/create-univocal-path-service.exe"
			    "crews-vdev/bin/delete-univocal-path-service.bat"
			    "crews-vdev/bin/appl-controller.exe"
			    "crews-vdev/bin/applmgr-server.exe"
			    "crews-vdev/bin/launcher-server.exe"
			    "crews-vdev/bin/pcserver.exe"
			    ))

;;;-----------------------------------------------------------------------------
;;;MAKE-SISCOG
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	99/03/23	Toni		Added an optional second parameter to
;;;					receive the destination directory.
;;;					When the respective argument is nil the
;;;					destination directory is the value of
;;;					the environment variable 'CREWS_DIR'.
;;;-----------------------------------------------------------------------------
(defun make-siscog (source-directory &optional destination-directory)
  "Copies the complete files or directories in *MAKE-SISCOG-NAMES* from
SOURCE-DIRECTORY to the DESTINATION-DIRECTORY, or to the contents of the
environment variable CREWS_DIR if DESTINATION-DIRECTORY is nil.
If any reference already exists (file or directory) it will be deleted
before"
  (dolist (name *make-siscog-names*)
    (let ((dest (if destination-directory
		  (format "%s/%s" destination-directory name)
		  (make-env-file "CREWS_DIR" name)))
	  (orig (format "%s/%s" source-directory name)))
      (cond ((file-directory-p orig)
	     (cond ((file-directory-p dest)
		    (sc-delete-directory dest))
		   ((file-exists-p dest)
		    (delete-file dest)))
	     (sc-copy-directory orig dest))
	    ((file-exists-p orig)
	     (cond ((file-directory-p dest)
		    (sc-delete-directory dest))
		   ((file-exists-p dest)
		    (delete-file dest)))
	     (copy-file orig dest)))))
  )

;; ------------------------------------------------------------------------------
;;                             UPDATING FILES FROM SISCOG
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;UPDATE-SISCOG-FILE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun update-siscog-file (from to)
  (cond ((file-exists-p to)
	 (let ((from-time (elt (file-attributes from) 5))
	       (to-time (elt (file-attributes to) 5)))
	   (when (or (> (car from-time) (car to-time))
		     (and (= (car from-time) (car to-time))
			  (> (cadr from-time) (cadr to-time))))
	     (message "Updating %s" to)
	     (delete-file to)
	     (copy-file from to))))
	(t (message "Creating %s" to)
	   (copy-file from to))))

;;;-----------------------------------------------------------------------------
;;;UPDATE-SISCOG-PATH
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	99/02/26	A. Frazao	Calls SC-CREWS-FILE-P
;;;	99/03/23	Toni		Passed a second argument 'from' to the
;;;					call of 'sc-crews-file-p'.
;;;	09/02/10	J. P. Varandas	sc-crews-file-p -> sc-rplan-file-p
;;;-----------------------------------------------------------------------------
(defun update-siscog-path (from to)
  (cond ((file-directory-p from)
	 (cond ((file-directory-p to)
		(dolist (name (directory-files from))
		  (if (sc-rplan-file-p name from)
		      (let ((orig (format "%s/%s" from name))
			    (dest (format "%s/%s" to name)))
			(cond ((file-directory-p orig)
			       (update-siscog-path orig dest))
			      ((file-exists-p orig)
			       (update-siscog-file orig dest)))))))
	       (t (message "Creating %s" to)
		  (sc-copy-directory from to))))
	((file-exists-p from)
	 (update-siscog-file from to))))

;;;-----------------------------------------------------------------------------
;;;UPDATE-SISCOG
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	99/03/23	Toni		Added an optional second parameter to
;;;					receive the destination directory.
;;;					When the respective argument is nil the
;;;					destination directory is the value of
;;;					the environment variable 'CREWS_DIR'.
;;;-----------------------------------------------------------------------------
(defun update-siscog (source-directory &optional destination-directory)
  "Updates the complete files or directories in *MAKE-SISCOG-NAMES* from
SOURCE-DIRECTORY to the DESTINATION-DIRECTORY, or to the contents of the
environment variable CREWS_DIR if DESTINATION-DIRECTORY is nil.
Only the new files or those whose modification date in SOURCE-DIRECTORY is
prior the modification date in DESTINATION-DIRECTORY (or CREWS_DIR) are copied.
Directories are created automatically as necessary."
  (dolist (name *make-siscog-names*)
    (update-siscog-path (format "%s/%s" source-directory name)
			(if destination-directory
			  (format "%s/%s" destination-directory name)
			  (make-env-file "CREWS_DIR" name)))))


;; ------------------------------------------------------------------------------
;;           USED BELOW BY THE COPYING AND UPDATING OF C AND C++ CREWS
;;         MULTI-USER/DATABASE RELATED DIRECTORIES AND FILES FROM SISCOG
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;COPY-C-AND-CPP-CREWS-DLLS
;;;Description
;;;	Copies the non-siscog dlls on which siscog executables or dlls depend,
;;;	from the Official Siscog directory to a specified destination directory
;;;	(a supposed CREWS_DIR).
;;;	Parameters:
;;;	  source-directory
;;;	    The full path of the Official Siscog directory.
;;;	  destination-directory
;;;	    The full path of the destination directory for the copy or update.
;;;	Returns:
;;;	  t.
;;;History
;;;	Date		Author		Description
;;;	99/08/25	Toni		Created
;;;-----------------------------------------------------------------------------
(defun copy-c-and-cpp-crews-dlls (source-directory destination-directory)
  (let ((dlls-source-directory        (format "%s/crews5-0/dll" source-directory))
	;; DLLs are copied both to CREWS_VERSION_DIR/dll and to CREWS_BIN.
	(dlls-destination-directories (list (format "%s/crews5-0/dll" destination-directory)
					    (format "%s/bin" destination-directory)))
	(dlls-names '("mfc42.dll"
		      "mfc42d.dll"
		      "mfco42d.dll"
		      "msvcrt.dll"
		      "msvcrtd.dll"
		      "oncrpc.dll")))

    (dolist (dlls-destination-directory dlls-destination-directories)
      
      (when (not (file-directory-p dlls-destination-directory))
	(make-directory dlls-destination-directory t))

      (dolist (dll-name dlls-names)
	(copy-file (format "%s/%s" dlls-source-directory dll-name)
		   (format "%s/%s" dlls-destination-directory dll-name)))))
  
  t)

;; ------------------------------------------------------------------------------
;;      USED BELOW BY THE COPYING AND UPDATING OF C AND C++ CREWS MULTI-USER
;;                  RELATED DIRECTORIES AND FILES FROM SISCOG
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;*C-AND-CPP-CREWS-MULTI-USER-NAMES*
;;;Description
;;;	This variable contains all the paths bellow CREWS_DIR that are used by
;;;	MAKE-C-AND-CPP-CREWS-MULTI-USER and UPDATE-C-AND-CPP-CREWS-MULTI-USER.
;;;History
;;;	Date		Author		Description
;;;	99/03/23	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar *c-and-cpp-crews-multi-user-names* nil)

;;;-----------------------------------------------------------------------------
;;;SETQ *C-AND-CPP-CREWS-MULTI-USER-NAMES*
;;;Description
;;;	Default values for *C-AND-CPP-CREWS-MULTI-USER-NAMES*.
;;;History
;;;	Date		Author		Description
;;;	99/03/23	Toni		Created
;;;-----------------------------------------------------------------------------
(setq *c-and-cpp-crews-multi-user-names*
  '("crews5-0/application-controller"
    "crews5-0/application-manager"
    "crews5-0/interface"
    "crews5-0/launcher"
    "crews5-0/multi-user/partition-server/c"
    "crews5-0/multi-user/periods"
    "crews5-0/univocal-path-server"
    "crews5-0/win_services"
    "siscog-util/machine"
    ))

;;;-----------------------------------------------------------------------------
;;;DELETE-C-AND-CPP-CREWS-MULTI-USER-TRASH-FILES
;;;Description
;;;	Deletes C and C++ Crews Multi-User related files, that are trash, from a
;;;	specified directory (a supposed CREWS_DIR).
;;;	C and C++ Crews Multi-User trash files are files that:
;;;	  - are generated by the build process of C and C++ Crews Multi-User
;;;	    related applications and libraries;
;;;	  - are lost in the C and C++ Crews Multi-User directories but have no
;;;	    use (are just waiting for the judgement day to be terminated).
;;;History
;;;	Date		Author		Description
;;;	99/03/23	Toni		Created
;;;-----------------------------------------------------------------------------
(defun delete-c-and-cpp-crews-multi-user-trash-files (directory)

  (flet ((delete-file-if-exists (file-name)
	   (when (file-exists-p file-name)
	     (message "Deleting %s" file-name)
	     (delete-file file-name))))
  
    (let ((appl-mgr-directory (format "%s/%s" directory "crews5-0/application-manager")))
      (delete-file-if-exists (format "%s/%s" appl-mgr-directory "appl.h"))
      (delete-file-if-exists (format "%s/%s" appl-mgr-directory "applmgr.h"))
      (delete-file-if-exists (format "%s/%s" appl-mgr-directory "appl.c"))
      (delete-file-if-exists (format "%s/%s" appl-mgr-directory "appl_clnt.c"))
      (delete-file-if-exists (format "%s/%s" appl-mgr-directory "appl_svc.c"))
      (delete-file-if-exists (format "%s/%s" appl-mgr-directory "appl_xdr.c"))
      (delete-file-if-exists (format "%s/%s" appl-mgr-directory "applmgr_clnt.c"))
      (delete-file-if-exists (format "%s/%s" appl-mgr-directory "applmgr_svc.c"))
      (delete-file-if-exists (format "%s/%s" appl-mgr-directory "applmgr_xdr.c"))
      (delete-file-if-exists (format "%s/%s" appl-mgr-directory "appl.x")))
    
    (let ((launcher-directory (format "%s/%s" directory "crews5-0/launcher")))
      (delete-file-if-exists (format "%s/%s" launcher-directory "launcher.h"))
      (delete-file-if-exists (format "%s/%s" launcher-directory "launcher_clnt.c"))
      (delete-file-if-exists (format "%s/%s" launcher-directory "launcher_svc.c"))
      (delete-file-if-exists (format "%s/%s" launcher-directory "launcher_xdr.c")))
    
    (let ((pserver-directory (format "%s/%s" directory "crews5-0/multi-user/partition-server/c")))
      (delete-file-if-exists (format "%s/%s" pserver-directory "pserver.h"))
      (delete-file-if-exists (format "%s/%s" pserver-directory "pserver_clnt.c"))
      (delete-file-if-exists (format "%s/%s" pserver-directory "pserver_svc.c"))
      (delete-file-if-exists (format "%s/%s" pserver-directory "pserver_xdr.c")))

    (let ((pcserver-directory (format "%s/%s" directory "crews5-0/multi-user/periods")))
      (delete-file-if-exists (format "%s/%s" pcserver-directory "pcp.h"))
      (delete-file-if-exists (format "%s/%s" pcserver-directory "pcp_clnt.c"))
      (delete-file-if-exists (format "%s/%s" pcserver-directory "pcp_svc.c"))
      (delete-file-if-exists (format "%s/%s" pcserver-directory "pcp_xdr.c"))
      (delete-file-if-exists (format "%s/%s" pcserver-directory "pcclient")))
    
    (let ((upserver-directory (format "%s/%s" directory "crews5-0/univocal-path-server")))
      (delete-file-if-exists (format "%s/%s" upserver-directory "upserver.h"))
      (delete-file-if-exists (format "%s/%s" upserver-directory "upserver_clnt.c"))
      (delete-file-if-exists (format "%s/%s" upserver-directory "upserver_svc.c"))
      (delete-file-if-exists (format "%s/%s" upserver-directory "upserver_xdr.c")))

    (let ((machine-directory (format "%s/%s" directory "siscog-util/machine")))
      (delete-file-if-exists (format "%s/%s" machine-directory "rpc_strings.h"))
      (delete-file-if-exists (format "%s/%s" machine-directory "rpc_strings_xdr.c"))
      (delete-file-if-exists (format "%s/%s" machine-directory "c-utils.def")))

    ))
    
;;;-----------------------------------------------------------------------------
;;;MAKE-OR-UPDATE-C-AND-CPP-CREWS-MULTI-USER
;;;Description
;;;	Copies or updates the C and C++ Crews Multi-User related directories and
;;;	files from the Official Siscog directory to a specified destination
;;;	directory (a supposed CREWS_DIR).
;;;	Parameters:
;;;	  make
;;;	    t if it to copy, nil if it is to update.
;;;	  source-directory
;;;	    The full path of the Official Siscog directory.
;;;	  destination-directory
;;;	    The full path of the destination directory for the copy or update.
;;;	Returns:
;;;	  t.
;;;	Note:
;;;	  This definition re-binds, in an internal lexical environment, the
;;;	  symbols:
;;;	    - sc-crews-file-p
;;;	    - *make-siscog-names*
;;;History
;;;	Date		Author		Description
;;;	99/03/23	Toni		Created
;;;	99/08/25	Toni		Calls 'copy-c-and-cpp-crews-dlls'.
;;;	09/02/10	J. P. Varandas	sc-crews-file-p -> sc-rplan-file-p
;;;-----------------------------------------------------------------------------
(defun make-or-update-c-and-cpp-crews-multi-user (make source-directory destination-directory)
  
  (if make
    (message "Making C and C++ Crews Multi-User directories and files ...")
    (message "Updating C and C++ Crews Multi-User directories and files ..."))
  
  (flet ((sc-rplan-file-p (name &optional directory-name)
	   (sc-c-and-cpp-crews-multi-user-file-p name (and directory-name ""))))
    (let ((*make-siscog-names* *c-and-cpp-crews-multi-user-names*))
      (cond (make
	     (make-siscog source-directory destination-directory))
	    (t
	     (update-siscog source-directory destination-directory)))))
	     
  (copy-c-and-cpp-crews-dlls source-directory destination-directory)
  
  (delete-c-and-cpp-crews-multi-user-trash-files destination-directory)
  
  (if make
    (message "Finished making C and C++ Crews Multi-User directories and files.")
    (message "Finished updating C and C++ Crews Multi-User directories and files."))
  
  t)

;; ------------------------------------------------------------------------------
;; COPYING ALL THE C AND C++ CREWS MULTI-USER RELATED DIRECTORIES AND FILES FROM
;;                                   SISCOG
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;MAKE-C-AND-CPP-CREWS-MULTI-USER
;;;Description
;;;	See (below) the documentation string attached to the definition.
;;;History
;;;	Date		Author		Description
;;;	99/03/23	Toni		Created
;;;-----------------------------------------------------------------------------
(defun make-c-and-cpp-crews-multi-user (source-directory &optional destination-directory)
  "Copies the C and C++ Crews Multi-User related directories and files in
*C-AND-CPP-CREWS-MULTI-USER-NAMES* from the SOURCE-DIRECTORY to the
DESTINATION-DIRECTORY, or to the contents of the environment variable CREWS_DIR
if DESTINATION-DIRECTORY is nil.
If any reference already exists (file or directory) it will be deleted
before."
  
  (interactive (list (read-default-string "Source Directory" "Y:" nil)
		     (read-default-string "Destination Directory" (getenv "CREWS_DIR") nil)))

  (when (not destination-directory)
    (setq destination-directory (getenv "CREWS_DIR")))
  
  (make-or-update-c-and-cpp-crews-multi-user t source-directory destination-directory))  
  

;; ------------------------------------------------------------------------------
;;   UPDATING THE C AND C++ CREWS MULTI-USER RELATED DIRECTORIES AND FILES FROM
;;                                   SISCOG
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;UPDATE-C-AND-CPP-CREWS-MULTI-USER
;;;Description
;;;	See (below) the documentation string attached to the definition.
;;;History
;;;	Date		Author		Description
;;;	99/03/23	Toni		Created
;;;-----------------------------------------------------------------------------
(defun update-c-and-cpp-crews-multi-user (source-directory &optional destination-directory)
  "Updates the C and C++ Crews Multi-User related directories and files in
*C-AND-CPP-CREWS-MULTI-USER-NAMES* from the SOURCE-DIRECTORY to the
DESTINATION-DIRECTORY, or to the contents of the environment variable CREWS_DIR
if DESTINATION-DIRECTORY is nil.
Only the new files or those whose modification date in SOURCE-DIRECTORY is
prior the modification date in DESTINATION-DIRECTORY (or CREWS_DIR) are copied.
Directories are created automatically as necessary."
  
  (interactive (list (read-default-string "Source Directory" "Y:" nil)
		     (read-default-string "Destination Directory" (getenv "CREWS_DIR") nil)))

  (when (not destination-directory)
    (setq destination-directory (getenv "CREWS_DIR")))
  
  (make-or-update-c-and-cpp-crews-multi-user nil source-directory destination-directory))  
  

;; ------------------------------------------------------------------------------
;;      USED BELOW BY THE COPYING AND UPDATING OF C AND C++ CREWS DATABASE
;;                  RELATED DIRECTORIES AND FILES FROM SISCOG
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;*C-AND-CPP-CREWS-DATABASE-NAMES*
;;;Description
;;;	This variable contains all the paths bellow CREWS_DIR that are used by
;;;	MAKE-C-AND-CPP-CREWS-DATABASE and UPDATE-C-AND-CPP-CREWS-DATABASE.
;;;History
;;;	Date		Author		Description
;;;	99/08/25	Toni		Created
;;;-----------------------------------------------------------------------------
(defvar *c-and-cpp-crews-database-names* nil)

;;;-----------------------------------------------------------------------------
;;;SETQ *C-AND-CPP-CREWS-DATABASE-NAMES*
;;;Description
;;;	Default values for *C-AND-CPP-CREWS-DATABASE-NAMES*.
;;;History
;;;	Date		Author		Description
;;;	99/08/25	Toni		Created
;;;-----------------------------------------------------------------------------
(setq *c-and-cpp-crews-database-names*
  '("crews5-0/application-controller-db-cpp"
    "crews5-0/win_database"
    "siscog-util/odbc-cpp"
    "siscog-util/sg-odbc-cpp"
    ))

;;;-----------------------------------------------------------------------------
;;;DELETE-C-AND-CPP-CREWS-DATABASE-TRASH-FILES
;;;Description
;;;	Deletes C and C++ Crews Database related files, that are trash, from a
;;;	specified directory (a supposed CREWS_DIR).
;;;	C and C++ Crews Database trash files are files that:
;;;	  - are generated by the build process of C and C++ Crews Database
;;;	    related applications and libraries;
;;;	  - are lost in the C and C++ Crews Database directories but have no
;;;	    use (are just waiting for the judgement day to be terminated).
;;;History
;;;	Date		Author		Description
;;;	99/08/25	Toni		Created
;;;-----------------------------------------------------------------------------
(defun delete-c-and-cpp-crews-database-trash-files (directory)

  (flet ((delete-file-if-exists (file-name)
	   (when (file-exists-p file-name)
	     (message "Deleting %s" file-name)
	     (delete-file file-name))))

    ;; For now does nothing.
    
    ))
    
;;;-----------------------------------------------------------------------------
;;;MAKE-OR-UPDATE-C-AND-CPP-CREWS-DATABASE
;;;Description
;;;	Copies or updates the C and C++ Crews database related directories and
;;;	files from the Official Siscog directory to a specified destination
;;;	directory (a supposed CREWS_DIR).
;;;	Parameters:
;;;	  make
;;;	    t if it to copy, nil if it is to update.
;;;	  source-directory
;;;	    The full path of the Official Siscog directory.
;;;	  destination-directory
;;;	    The full path of the destination directory for the copy or update.
;;;	Returns:
;;;	  t.
;;;	Note:
;;;	  This definition re-binds, in an internal lexical environment, the
;;;	  symbols:
;;;	    - sc-crews-file-p
;;;	    - *make-siscog-names*
;;;History
;;;	Date		Author		Description
;;;	99/08/25	Toni		Created
;;;	09/02/10	J. P. Varandas	sc-crews-file-p -> sc-rplan-file-p
;;;-----------------------------------------------------------------------------
(defun make-or-update-c-and-cpp-crews-database (make source-directory destination-directory)
  
  (if make
    (message "Making C and C++ Crews Database directories and files ...")
    (message "Updating C and C++ Crews Database directories and files ..."))
  
  (flet ((sc-rplan-file-p (name &optional directory-name)
	   (sc-c-and-cpp-crews-database-file-p name (and directory-name ""))))
    (let ((*make-siscog-names* *c-and-cpp-crews-database-names*))
      (cond (make
	     (make-siscog source-directory destination-directory))
	    (t
	     (update-siscog source-directory destination-directory)))))
	     
  (copy-c-and-cpp-crews-dlls source-directory destination-directory)
  
  (delete-c-and-cpp-crews-database-trash-files destination-directory)
  
  (if make
    (message "Finished making C and C++ Crews Database directories and files.")
    (message "Finished updating C and C++ Crews Database directories and files."))
  
  t)

;; ------------------------------------------------------------------------------
;; COPYING ALL THE C AND C++ CREWS DATABASE RELATED DIRECTORIES AND FILES FROM
;;                                   SISCOG
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;MAKE-C-AND-CPP-CREWS-DATABASE
;;;Description
;;;	See (below) the documentation string attached to the definition.
;;;History
;;;	Date		Author		Description
;;;	99/08/25	Toni		Created
;;;-----------------------------------------------------------------------------
(defun make-c-and-cpp-crews-database (source-directory &optional destination-directory)
  "Copies the C and C++ Crews Database related directories and files in
*C-AND-CPP-CREWS-DATABASE-NAMES* from the SOURCE-DIRECTORY to the
DESTINATION-DIRECTORY, or to the contents of the environment variable CREWS_DIR
if DESTINATION-DIRECTORY is nil.
If any reference already exists (file or directory) it will be deleted
before."
  
  (interactive (list (read-default-string "Source Directory" "Y:" nil)
		     (read-default-string "Destination Directory" (getenv "CREWS_DIR") nil)))

  (when (not destination-directory)
    (setq destination-directory (getenv "CREWS_DIR")))
  
  (make-or-update-c-and-cpp-crews-database t source-directory destination-directory))  
  

;; ------------------------------------------------------------------------------
;;   UPDATING THE C AND C++ CREWS DATABASE RELATED DIRECTORIES AND FILES FROM
;;                                   SISCOG
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;UPDATE-C-AND-CPP-CREWS-DATABASE
;;;Description
;;;	See (below) the documentation string attached to the definition.
;;;History
;;;	Date		Author		Description
;;;	99/08/25	Toni		Created
;;;-----------------------------------------------------------------------------
(defun update-c-and-cpp-crews-database (source-directory &optional destination-directory)
  "Updates the C and C++ Crews Database related directories and files in
*C-AND-CPP-CREWS-DATABASE-NAMES* from the SOURCE-DIRECTORY to the
DESTINATION-DIRECTORY, or to the contents of the environment variable CREWS_DIR
if DESTINATION-DIRECTORY is nil.
Only the new files or those whose modification date in SOURCE-DIRECTORY is
prior the modification date in DESTINATION-DIRECTORY (or CREWS_DIR) are copied.
Directories are created automatically as necessary."
  
  (interactive (list (read-default-string "Source Directory" "Y:" nil)
		     (read-default-string "Destination Directory" (getenv "CREWS_DIR") nil)))

  (when (not destination-directory)
    (setq destination-directory (getenv "CREWS_DIR")))
  
  (make-or-update-c-and-cpp-crews-database nil source-directory destination-directory))  
  

