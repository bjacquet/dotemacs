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
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  SC-SELECT-APPL-VERSION-DIR
;;;					  SC-SET-PRODUCT
;;;					  SC-SETENV-PRODUCT-DIR
;;;					  SC-EXECUTE-RUN-APPLICATIONS
;;;					  SC-SET-ACL-VERSION
;;;	09/09/09	Rui Patrocinio	Changed definitions
;;;					  SC-RUN-ALL-APPLICATIONS
;;;					  SC-RUN-ALL-APPLICATIONS-NEXT
;;;					  SC-RUN-ALL-APPLICATIONS-SENTINEL
;;;					  *SC-APPLICATIONS-TO-RUN*
;;;					  SC-RUN-IMAGES-MENU-ITEMS
;;;	09/09/23	P. Filipe	Changed definitions
;;;                                       SC-ACL-VERSION-VALUES
;;;	09/09/23	P. Madeira	Changed definitions
;;;					  SC-SELECT-APPL-VERSION-DIR
;;;					  SC-RUN-IMAGE-WITH-MODE
;;;					  SC-RUN-ALL-APPLICATIONS
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;                             SET PRODUCTS CONFIGURATION
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;*OLD-PRODUCTS-CONFIGURATION*
;;;Description
;;;	Is a \emph{boolean} that states if the installation and execution of 
;;;	images is done in previous. 
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	08/07/08	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar *old-products-configuration* t)


;;;-----------------------------------------------------------------------------
;;;SC-SET-NEW-PRODUCTS
;;;Description
;;;	Activates new products environment
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
;;;	08/07/08	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-set-new-products ()
  (setq *old-products-configuration* nil))


;;;-----------------------------------------------------------------------------
;;;SC-UNSET-NEW-PRODUCTS
;;;Description
;;;	Deactivates new products environment
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
;;;	08/07/08	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-unset-new-products ()
  (setq *old-products-configuration* t))


;;;-----------------------------------------------------------------------------
;;;                             SET CURRENT PRODUCT
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;*SC-CURRENT-PRODUCT*
;;;Description
;;;	Indicates the product on where the user is working with
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *sc-current-product* nil)

;;;-----------------------------------------------------------------------------
;;;SC-SETENV-PRODUCT-DIR
;;;Description
;;;	Set the environment variables specifying the home directory of a product
;;;	and optionally a company.
;;;
;;;	These correspond to (old style) PRODUCT_VERSION_DIR or PRODUCT_COMPANY_VERSION_DIR
;;;	and (new style), PRODUCT_DIR and PRODUCT_COMPANY_DIR.
;;;	Note that the latter points to the parent directory of the former.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{topdir} is a \emph{string}, the top directory.
;;;		
;;;		\arg{product-name} is a \emph{string}, the name of a product.
;;;		
;;;		\arg{company-name} is a \emph{string}, the name of a company.
;;;		
;;;	\return-types
;;;		void
;;;
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	*sc-crews-top-dir* -> *sc-local-repository-dir*
;;;-----------------------------------------------------------------------------
(defun sc-setenv-product-dir (topdir product-name &optional company-name)
  (if company-name
      (let ((vdir (expand-file-name (format "%s-%s-vdev" (downcase product-name) (downcase company-name))
				    *sc-local-repository-dir*)))
	(setenv (format "%s_%s_VERSION_DIR" (upcase product-name) (upcase company-name))
		(expand-file-name (format "%s-%s" (downcase product-name) (downcase company-name)) vdir))
	(setenv (format "%s_%s_DIR" (upcase product-name) (upcase company-name)) vdir))
      (let ((vdir (expand-file-name (format "%s-vdev" (downcase product-name))
				    *sc-local-repository-dir*)))
	(setenv (format "%s_VERSION_DIR" (upcase product-name))
		(expand-file-name (downcase product-name) vdir))
	(setenv (format "%s_DIR" (upcase product-name)) vdir))))


;;;-----------------------------------------------------------------------------
;;;SC-SET-PRODUCT
;;;Description
;;;	Set the current product environment
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{product-name} is a \emph{string}, the name of a product. 
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	*sc-crews-top-dir* -> *sc-local-repository-dir*
;;;-----------------------------------------------------------------------------
(defun sc-set-product (product-name)
  (let ((product (sc-find-product (upcase product-name))))
    (assert product nil "Unknown product %s" product-name)
    (let ((name (sc-product-name product))
	  (external-id (sc-product-external-id product)))
      ;; Only CREWS uses the old products configuration
      (when (and *old-products-configuration*
		 (not (string-equal product-name "CREWS")))
	(sc-set-new-products))
      (setf *sc-current-product* product)
      ;; SISCOG_DIR
      (setenv "SISCOG_DIR" *sc-local-repository-dir*)
      ;; Product dir
      (sc-setenv-product-dir *sc-local-repository-dir* external-id)
      (setenv "SISCOG_VERSION_DIR" (expand-file-name "../siscog-util"
                                                     (getenv (format "%s_VERSION_DIR" (upcase product-name)))))
      (dolist (system (sc-product-systems product))
	(let ((company-name (sc-system-company-name system)))
	  (when company-name
	    (sc-setenv-product-dir *sc-local-repository-dir* external-id company-name))))
      )))


;; Only set the default product once startup is finished and the user's
;; settings are loaded.
(sc-set-product "crews")


;;;-----------------------------------------------------------------------------
;;;SC-CURRENT-PRODUCT-ENV-NAME
;;;Description
;;;	Create an environment variable name based on the name of the current
;;;	product and \arg{parts}.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{parts} is a \emph{list} of \emph{string}s.
;;;		
;;;	\return-types
;;;		A \emph{string}.
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun sc-current-product-env-name (sufix)
  (format "%s_%s" (sc-product-name *sc-current-product*) (upcase sufix)))


;;;-----------------------------------------------------------------------------
;;;                             SET LISP COMMAND MODE
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;*LISP-COMMAND-MODE*
;;;Description
;;;	A \emph{boolean}. If it is \emph{true}, start the lisp asynchronously in
;;;	the Operating System. Otherwise, starts the lisp within the Emacs.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar *lisp-command-mode* nil)

;;;-----------------------------------------------------------------------------
;;;SET-LISP-COMMAND-MODE
;;;Description
;;;	Sets the mode of starting a lisp application.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{value} is a \emph{boolean}. If it is \emph{true}, start the
;;;		lisp asynchronously in the Operating System. Otherwise, starts
;;;		the lisp within the Emacs.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun set-lisp-command-mode (value)
  (setf *lisp-command-mode* value))

;;;-----------------------------------------------------------------------------
;;;RUN-LISP-IMAGE
;;;Description
;;;	Runs a lisp image.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{image} is a \emph{string}, the pathname of the image file.
;;;		
;;;		\arg{args} is a list of \emph{string}, the arguments to use when
;;;		starting the application.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/27	A. Frazao	Added *lisp-command-mode*
;;;	03/07/28	A. Frazao	Removed Lucid switch
;;;-----------------------------------------------------------------------------
(defun run-lisp-image (image args)
  (if *lisp-command-mode*
      (run-allegro-lisp-command image args)
      (run-allegro-lisp-image image args)))


;;;-----------------------------------------------------------------------------
;;;                             ACL Versions
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SC-ACL-VERSION-VALUES
;;;Description
;;;	Returns the references to be used when setting a specific ACL version.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{version} is a \emph{keyword} that identifies the ACL version.
;;;		The possible values are currently :v5-0-1 or :v6-2.
;;;		
;;;	\return-types
;;;		A \emph{string}, the lisp program pathname.
;;;
;;;		A \emph{string}, the lisp image pathname.
;;;
;;;		A \emph{string}, the ACL top directory.
;;;
;;;		A \emph{string}, the directory where the DLLs are stored to set
;;;		the environment variable PATH
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	04/11/21	A. Frazao	By default ACL 62 files are in drive D
;;;	04/12/10	A. Frazao	Added 6th returned value
;;;	06/04/06	J. P. Varandas	Added ACL 80
;;;	06/10/23	A. Frazao	Changed ACL80 DLLs path
;;;	07/04/13	MCoutinho	Allegro path drive is now based on Windows installation.
;;;	07/12/28	J. P. Varandas	Added ACL 81 with the same DLLs of ACL 80
;;;	09/02/04	J. P. Varandas	Removed ACL 5.0.1 and ACL 6.2
;;;	09/02/10	J. P. Varandas	Removed dlls directory;
;;;					use "PROGRAMFILES" environment variable
;;;	09/09/23	P. Filipe	Added getenv to avoid re-define "SISCOG_DIR"
;;;-----------------------------------------------------------------------------
(defun sc-acl-version-values (version &rest keys)
  (flet ((in-program-files (path)
           (expand-file-name path (getenv "PROGRAMFILES"))))
    (case version
      (:v8-0
       (values (or (getenv "SISCOG_DIR") "z:/siscog")
	       (in-program-files "acl80/allegro-ansi.exe")
               (in-program-files "acl80/allegro-ansi.dxl")
               (in-program-files "acl80")))
      (:v8-1
       (values (or (getenv "SISCOG_DIR") "z:/siscog")
	       (in-program-files "acl81/allegro-ansi.exe")
               (in-program-files "acl81/allegro-ansi.dxl")
               (in-program-files "acl81")))
      (otherwise
       (error "Wrong ACL version: %s" version)))))



;;;-----------------------------------------------------------------------------
;;;NT-DIR-TO-XP-DIR
;;;Description
;;;	Transforms a pathname whose separator is "\" into a pathname whose separator is "\\" to be used in the XP registry
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{ntdir} is a \emph{string} that represents a pathname with separator = "/"
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	06/04/06	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun nt-dir-to-xp-dir (ntdir)
  (let* ((dirs (sc-string-to-list ntdir 47)) ;; 47 = "/"
	 (xpdir (car dirs)))
    (dolist (dir (cdr dirs))
      (setf xpdir (format "%s\\\\%s" xpdir dir)))
    xpdir))


;;;-----------------------------------------------------------------------------
;;;SC-SET-ACL-VERSION
;;;Description
;;;	Sets the parameters of the ACL version.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{version} is a \emph{keyword} that identifies the ACL version.
;;;		The possible values are currently :v5-0-1, :v6-2, :v8-0.
;;;
;;;		\arg{no-registry} is a \emph{boolean}. If it is \emph{true} does
;;;		not update the registry. By default it is \emph{nil}.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	04/12/10	A. Frazao	Added path-dll-dir
;;;	05/08/24	Joao Pavao	Corrected the format of the PATH env-var
;;;	06/04/06	J. P. Varandas	Specialised the creation of the registry file for Allegro 8-0
;;;	06/11/03	A. Frazao	Sets variable *sc-crews-acl-version*
;;;					Commented setting the path environment variable.
;;;	07/03/14	A. Frazao	Added argument no-registry
;;;	07/12/28	J. P. Varandas	Windows Vista will not run the registry 
;;;					and do not support versions of Allegro earlier then 8.0
;;;	08/03/25	P. Madeira	Added double-quotes around registry
;;;					entries with acl-dxl.
;;;	08/07/08	A. Frazao	New environment variables logic
;;;	09/02/10	J. P. Varandas	Generalised for multiple products
;;;	09/02/10	J. P. Varandas	*sc-crews-acl-version* -> *sc-current-acl-version*
;;;					*sc-crews-top-dir* -> *sc-local-repository-dir*
;;;-----------------------------------------------------------------------------
(defun sc-set-acl-version (version &optional no-registry)
  (multiple-value-bind (products-top-dir acl-exe acl-dxl acl-home)
      (sc-acl-version-values version)
    (setq allegro-common-lisp-image-name acl-exe)
    (setq allegro-common-lisp-image-file acl-dxl)
    (setenv "ALLEGRO_CL_HOME" acl-home)
    (setq *sc-current-acl-version* version)
    (setq *sc-local-repository-dir* products-top-dir)
    ;; Sets product environment variables
    (sc-set-product (sc-product-name *sc-current-product*))
    ;; Changes the windows registry
    (unless (or no-registry
                ;; Windows Vista do not allow to change the registry easily
                ;; Windows Vista does not support versions of Allegro earlier then 8.0
                (search "-nt6" (emacs-version)))
      (let* ((filename  (expand-file-name
                         (format "~/set-acl-%s.reg" (subseq (symbol-name version) 1))))
             (exe-path  (nt-dir-to-xp-dir acl-exe))
             (dxl-path  (nt-dir-to-xp-dir acl-dxl)))
        (with-temp-buffer
          (insert "REGEDIT4\n\n"
                  "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\.dxl]\n"
		  "@=\"ACL.ImageFile\"\n"
                  "\n"
                  "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\.lpr]\n"
                  "@=\"ACL.ProjectFile\"\n"
                  "\n"
		 "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\ACL.ImageFile\\shell\\open\\command]\n"
		 "@=\"" exe-path " -I \\\"%1\\\"\"\n"
                 "\n"
		 "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\ACL.ProjectFile\\shell\\open\\command]\n"
		 "@=\"" exe-path " -I \\\"" dxl-path "\\\" -project \\\"%1\\\"\"\n"
                 "\n"
		 "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Applications\\allegro-ansi.exe\\shell\\open\\command]\n"
		 "@=\"" exe-path " -I \\\"%1\\\"\"\n"
                 "\n")
          (write-file filename))
        (shell-command (format "regedit /s %s" filename))))))


;;;-----------------------------------------------------------------------------
;;;SC-SET-ACL-VERSION
;;;Description
;;;	Sets default ACL version
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	06/11/03	A. Frazao	Created
;;;	07/03/14	A. Frazao	Sets :v8-0 by default
;;;	09/02/04	J. P. Varandas	Appended to the 'sc-startup-hook'
;;;					Sets :v8-1 by default
;;;-----------------------------------------------------------------------------
(sc-set-acl-version :v8-1 :no-registry)


;; ------------------------------------------------------------------------------
;;                                   APPLICATIONS
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;ALLEGRO
;;;Description
;;;	Runs the ACL program.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{args} is a list of \emph{string}, the arguments for the lisp
;;;		program.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun allegro (&optional args)
  (interactive)
  (run-allegro-lisp-image nil args))


;;;-----------------------------------------------------------------------------
;;;SC-SELECT-APPL-VERSION-DIR
;;;Description
;;;	Selects the system version and sets the corresponding crews directory.
;;;
;;;	Uses the value of the variable systems to select the possible
;;;	version directories associated with \arg{sc-system-name}.
;;;
;;;	When a version is selected, assigns the values of the environment variables to the following values:
;;;	CREWS_DIR = <top dir>/<selected crews dir>
;;;	CREWS_BIN = <top dir>/<selected crews dir>/bin
;;;	CREWS_PATCHES_DIR = <top dir>/<selected crews dir>/patches   (only if \elem{*sc-crews-local-top-dir*} is defined)
;;;	where <top dir> is given by the variable \elem{*sc-crews-top-dir*} or \elem{*sc-crews-local-top-dir*} (if defined)
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{arg} is an emacs event argument.
;;;		
;;;		\arg{sc-system-name} is a \emph{string}, the name of the crews_x crews-name
;;;		
;;;	\return-types
;;;		A \emph{boolean}.
;;;		If a version is selected, either manually or automatically, returns \emph{t}.
;;;		Otherwise, if there are no versions associated with \arg{crews-name},
;;;		or if the user didn't select a version, returns \emph{nil}.
;;;	
;;;	\refs
;;;		*sc-local-repository-dir*
;;;		*sc-binary-repository-dir*
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	07/08/13	RAurelio	Added support for *sc-crews-local-top-dir*
;;;					Update documentation
;;;	08/07/08	A. Frazao	
;;;	09/02/10	J. P. Varandas	Generalised for multiple products
;;;	09/02/10	J. P. Varandas	*sc-crews-top-dir* -> *sc-local-repository-dir*
;;;					*sc-crews-local-top-dir* -> *sc-binary-repository-dir*
;;;	09/09/23	P. Madeira	`system-name' -> `sc-system-name'.
;;;					Correct env. var. setting function.
;;;-----------------------------------------------------------------------------
(defun sc-select-appl-version-dir (arg sc-system-name)
  (let ((system (sc-find-system sc-system-name :key 'sc-system-name)))
    (when system
      (let ((versions (sc-system-versions system)))
        (when versions
          (let ((version (sc-popup-menu arg "Select"
                                        (mapcar (lambda (version)
                                                  (sc-make-menu-item version version))
                                                (cons "vdev" versions)))))
            (when version
              (let ((top-dir      (sc-get-system-version-dir system version))
                    (product-name (sc-product-external-id (sc-system-product system)))
                    (local-top    (and (boundp '*sc-binary-repository-dir*) *sc-binary-repository-dir*)))
                (flet ((setenv-prod (name value)
			 (let ((*sc-current-product* product-name))
			   (setenv (sc-current-product-env-name name)
				   value))))
                  (setenv (sc-current-product-env-name "BIN")
			  (format "%s/%s/bin" (or local-top *sc-local-repository-dir*) top-dir))
                  (when local-top
                    (setenv (sc-current-product-env-name "PATCHES_DIR")
			    (format "%s/%s/patches" local-top top-dir)))))
              t)))))))


;;;-----------------------------------------------------------------------------
;;;SC-RUN-APPLICATION
;;;Description
;;;	Runs a crews application.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{company-name} is a \emph{string}, the crews name of the system.
;;;		
;;;		\arg{application-name} is a \emph{string}, the image name of the application.
;;;		
;;;		\arg{design-p} is a \emph{boolean}.
;;;		
;;;		\arg{multi-user-p} is a \emph{boolean}.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	07/05/19	RAurelio	Added special behaviour to Dispatcher Server
;;;					(in development mode).
;;;	08/07/08	A. Frazao	Allow new application names
;;;	09/02/10	J. P. Varandas	Renamed from "sc-run-crews-application";
;;;					Generalised for multiple products
;;;-----------------------------------------------------------------------------
(defun sc-run-application (company-name application-name design-p multi-user-p)
  (let* ((args nil)
	 (application (sc-find-application application-name))
	 (image-name (if *old-products-configuration*
			 (format "%s-%s" company-name (sc-application-image-name application))
			 application-name))
	 (image (format "%s/%s" (getenv (sc-current-product-env-name "BIN")) image-name)))
    (when multi-user-p
      (let ((pshosts (getenv "PARTITION_SERVER_HOSTS"))
	    (pchosts (getenv "PCSERVER_HOSTS")))
	(if (null pshosts)
	    (setq pshosts (getenv "COMPUTERNAME")))
	(if (null pchosts)
	    (setq pchosts (getenv "COMPUTERNAME")))
	(setenv "PARTITION_SERVER_HOSTS" (read-default-string "Enter PARTITION_SERVER_HOSTS" pshosts nil))
	(setenv "PCSERVER_HOSTS" (read-default-string "Enter PCSERVER_HOSTS" pchosts nil))
	(setq args (append '("-applmgr" "-multi-user") args))))
    (when (string= image-name "dispatcher-server")
      (let ((rtd_develop_mode_data (getenv "CREWS_RTD_DEVELOP_MODE_DATA")))
	(setenv "CREWS_RTD_DEVELOP_MODE_DATA" (read-default-string "Enter CREWS_RTD_DEVELOP_MODE_DATA" rtd_develop_mode_data nil))))
    (when design-p
      (setq args (cons "-design" args)))
    (run-lisp-image image args)))


;;;-----------------------------------------------------------------------------
;;;SC-RUN-IMAGE-WITH-MODE
;;;Description
;;;	Runs a crews application with a specified mode of operation.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{arg} is an Emacs event.
;;;		
;;;		\arg{mode} is a \emph{string} which can be one of the following:
;;;		"su", "mu", "db", "tdmgr-db".
;;;		
;;;		\arg{sc-system-name} is a \emph{string}, the crews name of the system.
;;;		
;;;		\arg{image-name} is a \emph{string}, the image name of the application.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	03/07/28	A. Frazao	Corrects writing the PATH variable
;;;	06/08/24	J. P. Varandas	On Emacs 21 and Allegro 6.2 it requests to load the file SCM.001
;;;	08/07/08	A. Frazao	Allow new application names
;;;	08/10/23	A. Frazao	crews-development-mode -> siscog-development-mode
;;;	99/88/77	Tom Weissmann	Renamed from "sc-run-crews-with-mode"
;;;					Generalised for multiple products. (POA 13404.0)
;;;	09/09/23	P. Madeira	`system-name' -> `sc-system-name'.
;;;					Corrected referenced variables.
;;;-----------------------------------------------------------------------------
(defun sc-run-image-with-mode (arg mode sc-system-name image-name)
  (flet ((ensure-envs (&rest envs)
           (mapc (lambda (env)
                   (assert (getenv env) nil "Environment variable %s not set." env))
                 envs))
         (setf-getenv (item)
           (destructuring-bind (env &optional value) (ensure-list item)
             (let ((env-value (getenv env)))
               (when (or env-value value)
                 `(setf (getenv ,env) ,(or value (getenv env)))))))
         (insert-lisp (form)
           (when form
             (insert (format "%S\n" form)))))
    ;;
    (when (sc-select-appl-version-dir arg sc-system-name)
      (let* ((system                   (sc-find-system sc-system-name))
             (product-name             (sc-product-external-id (sc-system-product system)))
             (filename                 (expand-file-name (format "~/%s-before.lisp" (downcase sc-system-name))))
             (before-file              (expand-file-name (format "~/%s-before.lisp" product-name)))
             (development-mode         t)
             (multi-user-mode          nil)
             (application-manager-mode nil)
             (database-mode            nil)
             (tdmgr-db-mode            nil)
             (tdmgr-selection-db-mode  nil))
        ;; Environment variables that always need to be set
        (ensure-envs "CREWS_NT_SERVICES_HOST"
                     (sc-current-product-env-name "DIR") ; eg CREWS_DIR
                     (sc-current-product-env-name "DATA_DIR") ; eg CREWS_DATA_DIR
                     )
        ;; Mode-specific settings
	(cond ((string-equal mode "mu")
	       (ensure-envs "APPLMGRPROG" "LAUNCHERPROG")
	       (setq multi-user-mode          t
		     application-manager-mode t))
	      ((string-equal mode "tdmgr-db")
	       (ensure-envs "APPLMGRPROG" "LAUNCHERPROG")
	       (setq multi-user-mode t
		     application-manager-mode t
		     tdmgr-db-mode t
		     tdmgr-selection-db-mode t))
	      ((string-equal mode "db")
	       (ensure-envs (sc-current-product-env-name "DB_USER")
			    (sc-current-product-env-name "DB_DATA_SOURCE"))
	       (setq database-mode t
		     tdmgr-selection-db-mode t)))
        
        ;; Write the lisp
        (with-temp-buffer
          (mapc 'insert-lisp
                `((in-package :user)
                  ;; Variables
                  ,@(cond ((string-equal product-name "crews")
			   `((setf *crews-development-mode*         ,development-mode)
			     (setf *siscog-development-mode*        ,development-mode)
			     (setf *crews-multi-user-mode*          ,multi-user-mode)
			     (setf *crews-application-manager-mode* ,application-manager-mode)
			     (setf *crews-database-mode*            ,database-mode)
			     (setf *crews-tdmgr-db-mode*            ,tdmgr-db-mode)
			     (setf *crews-tdmgr-selection-db-mode*  ,tdmgr-selection-db-mode)))
			  (t
			   `((setf *development.mode*         ,development-mode)
			     (setf *multi.user.mode*          ,multi-user-mode)
			     (setf *application.manager.mode* ,application-manager-mode)
			     (setf *database.mode*            ,database-mode)
			     (setf *tdmgr.db.mode*            ,tdmgr-db-mode)
			     (setf *tdmgr.selection.db.mode*  ,tdmgr-selection-db-mode))))
		  ;; Environment variables
		  ,@(mapcar 'setf-getenv
			    `(("PATH" ,(substitute ?/ ?\\ (getenv "PATH")))
			      ,(sc-current-product-env-name "BIN")
			      ,(sc-current-product-env-name "DIR")
			      ,(sc-current-product-env-name "DATA_DIR")
			      ,(sc-current-product-env-name "PATCHES_DIR")
			      "CREWS_NT_SERVICES_HOST"
			      ,@(cond ((or (string-equal mode "mu")
					   (string-equal mode "tdmgr-db"))
				       '("LAUNCHERPROG" "APPLMGRPROG" "PCSERVER_HOSTS" "PARTITION_SERVER_HOSTS"))
				      ((string-equal mode "db")
				       (list (sc-current-product-env-name "DB_USER")
					     (sc-current-product-env-name "DB_DATA_SOURCE"))))))))
          ;; Product "before" file
          (insert-lisp `(when (probe-file ,before-file)
			 (load ,before-file)))
          ;;
          ;; Write the buffer
          (write-file filename))
        ;;
        ;; Run it
	(sc-run-application (sc-system-company-name system) image-name t (string-equal mode "mu"))))))


;;;-----------------------------------------------------------------------------
;;;                             PRODUCT SETTINGS CHANGE
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SC-READ-FILE-NAME-INTERACTIVE
;;;Description
;;;	Reads a file name interactively.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{prompt} is a \emph{string} to use as the prompt to the user.
;;;		
;;;		\arg{default} is a \emph{string} to use as the default value.
;;;		
;;;	\return-types
;;;		A list with a \emph{string} or with \emph{nil}.
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	06/10/23	J. P. Varandas	Set 'use-dialog-box' temporarily to NIL
;;;-----------------------------------------------------------------------------
(defun sc-read-file-name-interactive (prompt default)
  (let ((use-dialog-box nil))
    (let ((result (read-file-name prompt default default nil)))
      (if result
	  (list (expand-file-name result))
	  (list nil)))))


;;;-----------------------------------------------------------------------------
;;;SC-SET-TOP-DIR
;;;Description
;;;	Sets the environment variable {product}_DIR where {product} is the currently
;;;	selected product.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a directory. If it is \emph{nil}, the variable is
;;;		unset. If not supplied, the user is asked to supply it.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "sc-set-crews-dir"
;;;					Generalised for multiple products. (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-set-top-dir (dir)
  (interactive (sc-read-file-name-interactive (format "%s: " (sc-current-product-env-name "DIR"))
					      (getenv (sc-current-product-env-name "DIR"))))
  (message "%s: %s" (sc-current-product-env-name "DIR") dir)
  (setenv (sc-current-product-env-name "DIR") dir))


;;;-----------------------------------------------------------------------------
;;;SC-SET-DATA-DIR
;;;Description
;;;	Sets the environment variable {product}_DATA_DIR, where {product} is the currently
;;;	selected product.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a directory. If it is \emph{nil}, the variable is
;;;		unset. If not supplied, the user is asked to supply it.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "sc-set-crews-data-dir"
;;;					Generalised for multiple products. (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-set-data-dir (dir)
  (interactive (sc-read-file-name-interactive (format "%s: " (sc-current-product-env-name "DATA_DIR"))
					      (getenv (sc-current-product-env-name "DATA_DIR"))))
  (message "%s: %s" (sc-current-product-env-name "DATA_DIR") dir)
  (setenv (sc-current-product-env-name "DATA_DIR") dir))


;;;-----------------------------------------------------------------------------
;;;SC-SET-PATCHES-DIR
;;;Description
;;;	Sets the environment variable {product}_PATCHES_DIR, where {product}
;;;	is the currently selected product.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a directory. If it is \emph{nil}, the variable is
;;;		unset. If not supplied, the user is asked to supply it.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "sc-set-crews-patches-dir"
;;;					Generalised for multiple products. (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-set-patches-dir (dir)
  (interactive (sc-read-file-name-interactive (format "%s: " (sc-current-product-env-name "PATCHES_DIR"))
					      (getenv (sc-current-product-env-name "PATCHES_DIR"))))
  (message "%s: %s" (sc-current-product-env-name "PATCHES_DIR") dir)
  (setenv (sc-current-product-env-name "PATCHES_DIR") dir))

;;;-----------------------------------------------------------------------------
;;;SC-SET-DB-USER
;;;Description
;;;	Sets the environment variables {product}_DB_USER and {product}_DB_DATA_SOURCE,
;;;	where {product} is the currently selected product.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{user} is a DB user name. If it is \emph{nil}, the variable
;;;		is unset. If not supplied, the user is asked to supply it.
;;;		
;;;		\arg{data-source} is a DB data source name. If not supplied, the
;;;		user is asked to supply it.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "sc-set-crews-db-user"
;;;					Generalised for multiple products. (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-set-db-user (user &optional data-source)
  (interactive (list (read-string (format "%s: " (sc-current-product-env-name "DB_USER"))
				  (getenv (sc-current-product-env-name "DB_USER")))))
  (setenv (sc-current-product-env-name "DB_USER") user)
  (if data-source
      (progn
	(setenv (sc-current-product-env-name "DB_DATA_SOURCE") data-source)
	(message "%s: %s %s: %s" (sc-current-product-env-name "DB_USER") user (sc-current-product-env-name "DB_DATA_SOURCE") data-source))
      (message "%s: %s" (sc-current-product-env-name "DB_USER") user)))


;;;-----------------------------------------------------------------------------
;;;SC-SET-DB-DATA-SOURCE
;;;Description
;;;	Sets the environment variable {product}_DB_DATA_SOURCE, where {product}
;;;	is the currently selected product.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{data-source} is a DB data source name. If it is \emph{nil},
;;;		the variable is unset. If not supplied, the user is asked to
;;;		supply it.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "sc-set-crews-db-data-source"
;;;					Generalised for multiple products. (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-set-db-data-source (data-source)
  (interactive (list (read-string (format "%s: " (sc-current-product-env-name "DB_DATA_SOURCE")) (getenv (sc-current-product-env-name "DB_DATA_SOURCE")))))
  (message "%s: %s" (sc-current-product-env-name "DB_DATA_SOURCE") data-source)
  (setenv (sc-current-product-env-name "DB_DATA_SOURCE") data-source))


;;;-----------------------------------------------------------------------------
;;;SC-SHOW-CURRENT-SETTINGS
;;;Description
;;;	Display the environment variables for the currently-selected product
;;;	in a temporary buffer.
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
;;;	08/07/08	A. Frazao	Added variables
;;;	99/88/77	Tom Weissmann	Renamed from "sc-show-crews-settings"
;;;					Generalised for multiple products
;;;					Use a speciased buffer instead of *scratch* (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-show-current-settings ()
  (interactive)
  (let* ((product   (if *sc-current-product*
                        (concat (sc-product-name *sc-current-product*) " ")
                        ""))
         (buffer    (get-buffer-create (format "* %sEnvironment Settings *"
                                               product)))
         (variables `("HOME"
                      "SISCOG_DIR"
                      "SISCOG_UTIL_DIR"
                      ,(sc-current-product-env-name "BIN")
                      ,(sc-current-product-env-name "DIR")
                      ,(sc-current-product-env-name "DATA_DIR")
                      ,(sc-current-product-env-name "PATCHES_DIR")
                      "CREWS_NT_SERVICES_HOST"
                      ,(sc-current-product-env-name "DB_USER")
                      ,(sc-current-product-env-name "DB_DATA_SOURCE")
                      "PCSERVER_HOSTS"
                      "PARTITION_SERVER_HOSTS"
                      "APPLMGRPROG"
                      "LAUNCHERPROG"
                      "PATH")))
  (with-current-buffer buffer 
    (erase-buffer)
    (save-excursion
      (save-excursion
        (insert "------------------------------------------------------------\n"
                "Current " product "Settings\n"
                "------------------------------------------------------------\n"
                "\n"))
      (center-line 3)
      (forward-line 4)
      (when *old-products-configuration*
        (insert "** Using OLD Product configuration **\n"))
      (dolist (variable variables)
        (insert variable ": " (or (getenv variable) "Unspecified") "\n"))))
  (display-buffer buffer)))



;;;-----------------------------------------------------------------------------
;;;                         Applications Menu
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;*SC-RUN-IMAGES-MENU-ITEMS*
;;;Description
;;;	Is a list of menu items for systems and CREWS applications.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "*sc-crews-menu-items*" (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defvar *sc-run-images-menu-items* nil)


;;;-----------------------------------------------------------------------------
;;;SC-SETTING-OPTIONS-MENU-ITEMS
;;;Description
;;;	Menu items for the Options section of the Applications menu.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{list} of menu items.
;;;
;;;History
;;;	Date		Author		Description
;;;	99/88/77	Tom Weissmann	Created (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-setting-options-menu-items ()
  (list (sc-make-menu-item "Show settings" '(sc-show-current-settings))
	""
	(sc-make-menu-item "Set Product Dir" '(call-interactively 'sc-set-top-dir))
	(sc-make-menu-item "Set Data Dir" '(call-interactively 'sc-set-data-dir))
	(sc-make-menu-item "Set Patches Dir" '(call-interactively 'sc-set-patches-dir))
	(sc-make-menu-item "Set DB User" '(call-interactively 'sc-set-db-user))
	(sc-make-menu-item "Set DB Source" '(call-interactively 'sc-set-db-data-source))
	""
        (sc-make-menu-item "Set ACL 8.1" '(sc-set-acl-version :v8-1))
	(sc-make-menu-item "Set ACL 8.0" '(sc-set-acl-version :v8-0))
	""
	(sc-make-menu-item "Set Command Mode"   '(set-lisp-command-mode t))
	(sc-make-menu-item "Unset Command Mode" '(set-lisp-command-mode nil))
        ""
        (sc-make-menu-item "Set NEW products" '(sc-set-new-products))
        (sc-make-menu-item "Set OLD products" '(sc-unset-new-products))))


;;;-----------------------------------------------------------------------------
;;;SC-SETTING-CUSTOM-OPTIONS-MENU-ITEMS
;;;Description
;;;	Returns a list of menu items to be added to the default systems and CREWS
;;;	applications menu items.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of menu items. By default returns \emph{nil}.
;;;		
;;;	\remarks
;;;		This function is intended to be re-defined for a custom user.
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "sc-crews-custom-options-menu-items" (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-setting-custom-options-menu-items ()
  nil)

;;;-----------------------------------------------------------------------------
;;;SC-MAKE-RUN-APPL-MENU-ITEM
;;;Description
;;;	Creates a menu item to run a CREWS application of a system.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{sc-system}.
;;;		
;;;		\arg{appl} is a \elem{sc-crews-application}.
;;;		
;;;		\arg{mode} is a \emph{string} which can be one of the following:
;;;		"su", "mu", "db", "tdmgr-db".
;;;		
;;;		\arg{id-p} is a \emph{boolean} stating if the menu item display
;;;		should present also the \arg{mode}.
;;;		
;;;	\return-types
;;;		A menu item
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	08/07/08	A. Frazao	Use sc-crews-application-name
;;;	99/88/77	Tom Weissmann	Renamed from "sc-make-run-crews-appl-menu-item" (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-make-run-appl-menu-item (system appl mode id-p)
  (sc-make-menu-item (if id-p
			 (format "%s (%s)" (sc-application-external-id appl) mode)
			 (sc-application-external-id appl))
		     `(sc-run-image-with-mode arg ,mode
		       ,(sc-system-name system)
		       ,(sc-application-name appl))))

;;;-----------------------------------------------------------------------------
;;;SC-APPL-MENU-ITEMS
;;;Description
;;;	Builds a list of menu items to run system applications.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{sc-system}.
;;;		
;;;	\return-types
;;;		A list of menu items
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/08/05	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "sc-crews-appl-menu-items"
;;;					Accessor names drop "crews-" (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-appl-menu-items (system)
  (let ((db-appl-menu-items nil)
	(su-appl-menu-items nil)
	(mu-appl-menu-items nil))
    ;; DB mode menu items
    (when (sc-system-db-users system)
      (dolist (appl (sc-system-applications system))
	(when (find "db" (sc-application-modes appl) :test 'string-equal)
	  (setf db-appl-menu-items (cons (sc-make-run-appl-menu-item system appl "db" nil)
					 db-appl-menu-items)))))
    (setf db-appl-menu-items (reverse db-appl-menu-items))
    
    ;; SU mode menu items
    (dolist (appl (sc-system-applications system))
      (when (find "su" (sc-application-modes appl) :test 'string-equal)
	(setf su-appl-menu-items (cons (sc-make-run-appl-menu-item system appl "su" db-appl-menu-items)
				       su-appl-menu-items))))
    (when su-appl-menu-items
      (setf su-appl-menu-items (reverse su-appl-menu-items))
      (when db-appl-menu-items
	(setf su-appl-menu-items (cons "" su-appl-menu-items))))
    
    ;; MU mode menu items
    (when (find "partition-server" (sc-system-applications system) :key 'sc-application-name :test 'string-equal)
      (dolist (appl (sc-system-applications system))
	(when (find "mu" (sc-application-modes appl) :test 'string-equal)
	  (setf mu-appl-menu-items (cons (sc-make-run-appl-menu-item system appl "mu" (or db-appl-menu-items su-appl-menu-items))
					 mu-appl-menu-items)))))
    (when mu-appl-menu-items
      (setf mu-appl-menu-items (reverse mu-appl-menu-items))
      (when (or db-appl-menu-items su-appl-menu-items)
	(setf mu-appl-menu-items (cons "" mu-appl-menu-items))))
    (append db-appl-menu-items su-appl-menu-items mu-appl-menu-items)))


;;;-----------------------------------------------------------------------------
;;;SC-RUN-OTHER-PROCESS
;;;Description
;;;	Presents a menu so that the user can select an application to run in
;;;	another process of the operating system (not connected with the Emacs).
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{arg} is an Emacs event
;;;		
;;;		\arg{system} is a \emph{string}, a system name.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/08/05	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "sc-run-crews-other-process"
;;;					Generalised for multiple products. (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defun sc-run-other-process (arg system)
  (interactive "e")
  (let ((choice (sc-popup-menu arg
			       (sc-product-name *sc-current-product*)
			       (sc-appl-menu-items (sc-find-system system)))))
    (when choice
      (set-lisp-command-mode t)
      (unwind-protect
	   (sc-execute-menu-item arg choice)
	(set-lisp-command-mode nil)))))


;;;-----------------------------------------------------------------------------
;;;SC-RUN-IMAGES-MENU-ITEMS
;;;Description
;;;	Returns the list of menu items for systems and CREWS applications.
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
;;;	03/07/28	A. Frazao	Created
;;;	03/07/29	A. Frazao	Added "Run All"
;;;	03/08/05	A. Frazao	Added "Run in other session"
;;;	04/10/29	A. Frazao	Added description in sc-system-crews-db-users
;;;	06/04/06	J. P. Varandas	Add option about Allegro 8.0
;;;	99/88/77	Tom Weissmann	Renamed from "sc-crews-menu-items"
;;;					Generalised for multiple products
;;;					Use mapping to construct menus. (POA 13404.0)
;;;	09/09/09	Rui Patrocinio	Uncommented Run All menu option
;;;-----------------------------------------------------------------------------
(defun sc-run-images-menu-items ()
  (labels ((separated (list)
             (and list (cons "" list)))
           ;;
           (system-other-items (system)
             (let ((sys-name (sc-system-name system)))
               (list (sc-make-menu-item "Run in Other Process"
                                        `(sc-run-other-process arg ,sys-name))
                    (sc-make-menu-item "Run All"
                                       `(sc-run-all-applications arg ,sys-name)))))
           ;;
           (system-option-items (system)
             (mapcar (lambda (db-config)
                       (destructuring-bind (user db &optional label) db-config
                         (sc-make-menu-item (format "Set %s at %s%s"
                                                    user
                                                    db
                                                    (if label (format " (%s)" label) ""))
                                            `(sc-set-db-user ,user ,db))))
                     (sc-system-db-users system)))
           ;;
           (system-items (system)
             (cons (upcase (sc-system-company-name system))
                (append (sc-appl-menu-items system)
                        (separated (system-option-items system)) 
                        (separated (system-other-items system))))))
    ;;
    (or *sc-run-images-menu-items*
        (let ((systems (remove-if-not 'sc-system-company-name
                                      (sc-product-systems *sc-current-product*))))
          (setq *sc-run-images-menu-items*
                `(;; Options
                  ("Options"
                   ,@(sc-setting-options-menu-items)
                   ,@(separated (sc-setting-custom-options-menu-items)))
                  ;; Systems
                  ,@(mapcar 'system-items systems)))))))


;;;-----------------------------------------------------------------------------
;;;*SC-APPLICATIONS-TO-RUN*
;;;Description
;;;	Is a list of \emph{string}, each being a lisp image pathname.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/29	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "*sc-crews-applications-to-run*" (POA 13404.0)
;;;	09/09/09	Rui Patrocinio	Uncommented
;;;-----------------------------------------------------------------------------
(defvar *sc-applications-to-run* nil)


;;;-----------------------------------------------------------------------------
;;;SC-RUN-ALL-APPLICATIONS-SENTINEL
;;;Description
;;;	Processes the sentinel associated with the Lisp process when running all applications.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{process} is a Emacs process
;;;		
;;;		\arg{status} is a status of the Emacs process
;;;		
;;;	\return-types
;;;		T
;;;History
;;;	Date		Author		Description
;;;	03/07/29	A. Frazao	Created
;;;	99/88/77	Tom Weissmann	Renamed from "sc-run-all-crews-applications-sentinel" (POA 13404.0)
;;;	09/09/09	Rui Patrocinio	Uncommented
;;;-----------------------------------------------------------------------------
(defun sc-run-all-applications-sentinel (process status)
 ;; Sentinel and filter for network connections.  The sentinel currently
 ;; does nothing, other than prevent the status change message when the
 ;; connection is closed.
 (set-buffer (process-buffer process))
 (goto-char (point-max))
 (insert
  (format
   "\n---------------------------------------------------------------------\n"))
 (sc-run-all-applications-next)
 t)


;;;-----------------------------------------------------------------------------
;;;SC-RUN-ALL-APPLICATIONS-NEXT
;;;Description
;;;	Runs the next image in *sc-applications-to-run*
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
;;;	03/07/29	A. Frazao	Created
;;;	07/07/26	A. Frazao	Moved exit to crews-x-before sc-run-all-crews-applications
;;;	99/88/77	Tom Weissmann	Renamed from "sc-run-all-crews-applications-next" (POA 13404.0)
;;;	09/09/09	Rui Patrocinio	Uncommented
;;;-----------------------------------------------------------------------------
(defun sc-run-all-applications-next ()
 (when *sc-applications-to-run*
   (let* ((image (pop *sc-applications-to-run*)))
     (run-lisp-image image nil)
     (set-process-sentinel (get-process "*Allegro CL*") 'sc-run-all-applications-sentinel))))


;;;-----------------------------------------------------------------------------
;;;SC-RUN-ALL-APPLICATIONS
;;;Description
;;;	Runs all images of system identified by \arg{crews-name}
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{arg} is an Emacs event.
;;;		
;;;		\arg{crews-name} is a \emph{string}, the CREWS name of the system.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/29	A. Frazao	Created
;;;	07/07/26	A. Frazao	Write crews-x-before to define advice so that it exist after loading patches
;;;	99/88/77	Tom Weissmann	Renamed from "sc-run-all-crews-applications". (POA 13404.0)
;;;	09/09/09	Rui Patrocinio	Uncommented; Corrected to support new products
;;;	09/09/23	P. Madeira	`system-name' -> `sc-system-name'
;;;-----------------------------------------------------------------------------
(defun sc-run-all-applications (arg sc-system-name)
 (when (sc-select-appl-version-dir arg sc-system-name)
   (let* ((sc-system (sc-find-system sc-system-name))
	   (company-name (sc-system-company-name sc-system))
	   (filename (format "%s-%s-before.lisp"
                            (sc-product-external-id *sc-current-product*)
                            company-name))
	   (file (format "%s/%s" (getenv "HOME") filename))
	   (old-buf (current-buffer))
	   (buf (get-buffer filename)))
     (when buf
	(kill-buffer buf))
     (setq buf (find-file-noselect file))
     (set-buffer buf)
     (erase-buffer)
     (insert "(in-package :user)" 10)
     (if *old-products-configuration*
	 (insert "(excl::advise-1 'load-crews-patches :around nil nil '(:do-it (sleep 5) (WIN:FATALEXIT 0)))" 10)
	 (insert "(excl::advise-1 'load-system-patches :around nil nil '(:do-it (sleep 5) (WIN:FATALEXIT 0)))" 10))
     (save-buffer buf)
     (kill-buffer buf)
     (set-buffer old-buf)
     (setf *sc-applications-to-run* nil)
     (dolist (appl (sc-system-applications sc-system))
       (let ((image (if *old-products-configuration*
			(format "%s/%s-%s"
				(getenv (sc-current-product-env-name "BIN"))
				company-name
				(sc-application-image-name appl))
			 (format "%s/%s"
				 (getenv (sc-current-product-env-name "BIN"))
				 (sc-application-name appl)))))
	  (when (file-exists-p (format "%s.dxl" image))
	    (setf *sc-applications-to-run* (cons image *sc-applications-to-run*)))))
     (setf *sc-applications-to-run* (reverse *sc-applications-to-run*))
     (sc-run-all-applications-next))))


;; ------------------------------------------------------------------------------
;;                                   SWITCH MENU
;; ------------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;*SC-RUN-APPLICATIONS-MENU-ITEMS*
;;;Description
;;;	Stores the menu items of the main utilities menu.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar *sc-run-applications-menu-items* nil)

;;;-----------------------------------------------------------------------------
;;;SC-RUN-APPLICATIONS-MENU-DEFINITIONS
;;;Description
;;;	Returns the main utilities menu definitions.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of menu definitions where a menu definition is a list with
;;;		the syntax
;;;		(<symbol> <title> <menu items>)
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-run-applications-menu-definitions ()
  (let ((items nil))
    (dolist (product *sc-all-products*)
      (when (sc-product-rplan? product)
	(setf items (cons (list (sc-product-external-id product) (sc-product-name product))
			  items))))
    (reverse items)))


;;;-----------------------------------------------------------------------------
;;;SC-SET-RUN-APPLICATIONS-MENU-ITEMS
;;;Description
;;;	Sets the main utilities menu items.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{key} is a \emph{symbol} that identifies a menu definition
;;;		in \elem{sc-run-applications-menu-definitions}
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-set-run-applications-menu-items (product)
  (let ((switch-items nil))
    (dolist (menu (sc-run-applications-menu-definitions))
      (if (string-equal product (first menu))
	  (progn
	    (sc-set-product product)
	    (setf *sc-run-images-menu-items* nil)
	    (setf *sc-run-applications-menu-items* (sc-run-images-menu-items)))
	  (push `(,(second menu) sc-switch-run-applications-menu ,(first menu))
		switch-items)))
    (setf *sc-run-applications-menu-items* (append *sc-run-applications-menu-items* (list (cons "Switch to" (reverse switch-items)))))))


;;;-----------------------------------------------------------------------------
;;;SC-SWITCH-RUN-APPLICATIONS-MENU
;;;Description
;;;	Switches the main utilities menu items to the menu definition identified
;;;	by \arg{key}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{key} is a \emph{symbol} that identifies a menu definition
;;;		in \elem{sc-run-applications-menu-definitions}
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-switch-run-applications-menu (key)
  ;; Only CREWS uses the old products configuration
  (when (and *old-products-configuration*
	     (not (string-equal key "CREWS")))
    (sc-set-new-products))
  (sc-set-run-applications-menu-items key)
  (sc-execute-run-applications last-input-event))


;;;-----------------------------------------------------------------------------
;;;SC-EXECUTE-RUN-APPLICATIONS
;;;Description
;;;	Popups the main utilities menu. Executes the selected option.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{arg} is an Emacs event.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	08/07/08	A. Frazao	New product configuration
;;;	99/88/77	Tom Weissmann	Renamed from "crews-applications-menu" (POA 13404.0)
;;;	09/02/10	J. P. Varandas	*sc-crews-acl-version* -> *sc-current-acl-version*
;;;-----------------------------------------------------------------------------
(defun sc-execute-run-applications (arg)
  (interactive "e")
  (let* ((title (format "%s %s (ACL %s)"
			(if *old-products-configuration*
			    "OLD"
			    "NEW")
			(sc-product-name *sc-current-product*)
			(subseq (symbol-name *sc-current-acl-version*) 2)))
	 (choice (sc-popup-menu arg title *sc-run-applications-menu-items* t)))
    (if choice
	(sc-execute-menu-item arg choice))))


;;;-----------------------------------------------------------------------------
;;;SC-SET-RUN-APPLICATIONS-MENU-ITEMS
;;;Description
;;;	Sets the default menu items for the main uitilities menu
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(sc-set-run-applications-menu-items "crews")

;;;-----------------------------------------------------------------------------
;;;[C-M-DOWN-MOUSE-1]
;;;Description
;;;	Assigns the keyboard and mouse keys for the main uitilities menu
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(global-set-key [M-down-mouse-1] 'sc-execute-run-applications)

