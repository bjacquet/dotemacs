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
;;;	defines basic structures and operations for the definition of systems
;;;	and applications.
;;;	
;;; History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	04/10/29	A. Frazao	Documented definitions
;;;					  SC-DEFINE-SYSTEM
;;;	05/10/12	A. Frazao	Changed definitions
;;;					  SC-DEFINE-SYSTEM
;;;					  DEFSTRUCT SC-SYSTEM
;;;					Deleted definitions
;;;					  SC-FILE-RESPONSIBLE-NAME
;;;					  SC-FILE-RESPONSIBLE-PATHNAME
;;;					  *SC-ALL-FILE-RESPONSIBLES*
;;;	06/11/03	A. Frazao	Added definitions
;;;					  *SC-CREWS-ACL-VERSION*
;;;	07/08/13	RAurelio	Added definitions
;;;					  *SC-CREWS-LOCAL-TOP-DIR*
;;;	09/02/04	J. P. Varandas	Added definitions
;;;					  SC-RPLAN-SYSTEM?
;;;					  SC-DEFINE-PRODUCT
;;;					  SC-FIND-PRODUCT
;;;					  DEFSTRUCT SC-PRODUCT
;;;					  *SC-ALL-PRODUCTS*
;;;					Changed definitions
;;;					  SC-GET-SYSTEM-VERSION-DIR
;;;					  DEFSTRUCT SC-SYSTEM
;;;					  SC-FIND-SYSTEM-COMPANY-NAME
;;;					  SC-ADD-SYSTEM-DB-USER
;;;					  SC-FIND-SYSTEM-CREWS-NAME
;;;					  SC-ADD-APPLICATION-MODE
;;;					  SC-DEFINE-APPLICATION
;;;					  SC-ADD-SYSTEM-APPLICATION
;;;					  SC-DEFINE-SYSTEM
;;;					  SC-FIND-APPLICATION
;;;					  SC-ADD-CREWS-APPLICATION-MODE
;;;					  DEFSTRUCT SC-APPLICATION
;;;					  SC-DEFINE-CREWS-APPLICATION
;;;					  SC-FIND-CREWS-APPLICATION
;;;					  *SC-ALL-APPLICATIONS*
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  *SC-CENTRAL-REPOSITORY-DIR*
;;;					  *SC-BINARY-REPOSITORY-DIR*
;;;					  *SC-LOCAL-REPOSITORY-DIR*
;;;					  *SC-CURRENT-ACL-VERSION*
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;*SC-CURRENT-ACL-VERSION*
;;;Description
;;;	A keyword that identifies the ACL version.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	06/11/03	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	*sc-crews-acl-version* -> *sc-current-acl-version*
;;;-----------------------------------------------------------------------------
(defvar *sc-current-acl-version* nil)

;;;-----------------------------------------------------------------------------
;;;*SC-LOCAL-REPOSITORY-DIR*
;;;Description
;;;	A string with the reference to the top directory under which the system
;;;	version directories are located.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\example
;;;		(setq *sc-local-repository-dir* "z:/siscog")
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	*sc-crews-top-dir* -> *sc-local-repository-dir*
;;;-----------------------------------------------------------------------------
(defvar *sc-local-repository-dir* nil)


;;;-----------------------------------------------------------------------------
;;;*SC-BINARY-REPOSITORY-DIR*
;;;Description
;;;	A string with the reference to the local top directory under which the system
;;;	binaries and patches are located (environment variables %<product>_BIN% and %<product>_PATCHES_DIR%).
;;;	This variable makes it possible to keep the user's source code under a backup
;;;	protected location and have the binaries and patches on a different location,
;;;	so that they are not unnecessary taken for backup.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	07/08/13	RAurelio	Created
;;;	09/02/10	J. P. Varandas	*sc-crews-local-top-dir* -> *sc-binary-repository-dir*
;;;-----------------------------------------------------------------------------
(defvar *sc-binary-repository-dir* nil)


;;;-----------------------------------------------------------------------------
;;;*SC-CENTRAL-REPOSITORY-DIR*
;;;Description
;;;	Is the directory of the SISCOG archive.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\example
;;;		(setq *sc-central-repository-dir* "x:")
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	*sc-siscog-top-dir* -> *sc-central-repository-dir*
;;;-----------------------------------------------------------------------------
(defvar *sc-central-repository-dir* nil)


;;;-----------------------------------------------------------------------------
;;;*SC-ALL-PRODUCTS*
;;;Description
;;;	Stores all instances of \elem{sc-product}
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *sc-all-products* nil)

;;;-----------------------------------------------------------------------------
;;;DEFSTRUCT SC-PRODUCT
;;;Description
;;;	Is the SC-PRODUCT definition
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\class-attrs
;;;		\emph{name} is a \emph{string} indicating the internal name of the product
;;;	
;;;		\emph{external-id} is a \emph{string} indicating how the product is known for the user
;;;	
;;;		\emph{systems} is a list of \elem{sc-system}s that make part of the product
;;;	
;;;		\emph{rplan?} is a \emph{boolean} indicating if the product is part of the resources planning suit
;;;	
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defstruct sc-product
  name
  external-id
  systems
  rplan?
  )

;;;-----------------------------------------------------------------------------
;;;SC-FIND-PRODUCT
;;;Description
;;;	Tries to find a \elem{sc-product} by the given \arg{name}
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string}
;;;		
;;;	\return-types
;;;		A \elem{sc-product} or NIL
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun sc-find-product (name)
  (find name *sc-all-products* :key 'sc-product-name :test 'string-equal))

;;;-----------------------------------------------------------------------------
;;;SC-DEFINE-PRODUCT
;;;Description
;;;	Creates an instance of \elem{sc-product}
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string} indicating the internal name of the product
;;;		
;;;		\arg{external-id} is a \emph{string} indicating how the product is known for the user
;;;		
;;;		\arg{rplan?} is a \emph{boolean} indicating if the product is part of the resources planning suit
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun sc-define-product (name external-id rplan?)
  (let ((prod (sc-find-product name)))
    (if prod
	(progn
	  (setf (sc-product-external-id prod) external-id)
	  (setf (sc-product-rplan? prod) rplan?))
	(progn
	  (setf prod (make-sc-product :name name
				      :external-id external-id
				      :rplan? rplan?))
	  (setf *sc-all-products* (append *sc-all-products* (list prod)))))))


;;;-----------------------------------------------------------------------------
;;;*SC-ALL-APPLICATIONS*
;;;Description
;;;	Is a list of \elem{sc-application}
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/04	J. P. Varandas	sc-all-crews-applications -> sc-all-applications
;;;-----------------------------------------------------------------------------
(defvar *sc-all-applications* nil)

;;;-----------------------------------------------------------------------------
;;;DEFSTRUCT SC-APPLICATION
;;;Description
;;;	Is the SC-APPLICATION definition
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\class-attrs
;;;		\emph{name} is a lowercase \emph{string} with the official name
;;;		of the application.
;;;
;;;		\emph{external-id} is a \emph{string} to be used in the menus.
;;;
;;;		\emph{image-name} is a \emph{string}, the application image name.
;;;
;;;		\emph{modes} is a list of \emph{string}, each representing a mode
;;;		of operation supported by the application and that can be one of:
;;;		\begin{itemize,bulleted}
;;;		\item "su" - single-user mode in file architecture.
;;;		\item "mu" - multi-user mode in file architecture.
;;;		\item "db" - database mode.
;;;		\item "tdmgr-db" - multi-user mode in file architecture mixed
;;;		      with database mode.
;;;		\end{itemize}
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/04	J. P. Varandas	sc-crews-application -> sc-application
;;;-----------------------------------------------------------------------------
(defstruct sc-application
  name
  external-id
  image-name
  modes)

;;;-----------------------------------------------------------------------------
;;;SC-FIND-APPLICATION
;;;Description
;;;	Searches for an Application.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string}.
;;;
;;;		\arg{key} is a keyword argument whose value is a symbol that
;;;		names a function that is applied to a \elem{sc-application}
;;;		and that should return a string which is compared with \arg{name}.
;;;		The default value is \elem{sc-application-name}.
;;;		
;;;	\return-types
;;;		A \elem{sc-application} with \arg{name} or \emph{nil} if
;;;		not found.
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/04	J. P. Varandas	sc-all-crews-applications -> sc-all-applications
;;;-----------------------------------------------------------------------------
(defun sc-find-application (name &rest keys-values)
  (let ((key (or (cadr (member :key keys-values))
		 'sc-application-name)))
    (find name *sc-all-applications* :key key :test 'string-equal)))

;;;-----------------------------------------------------------------------------
;;;SC-DEFINE-APPLICATION
;;;Description
;;;	Defines an Application. If there is already an application with 
;;;	\arg{name}, it is changed destructively.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a lowercase \emph{string} with the official name
;;;		of the application.
;;;		
;;;		\arg{external-id} is a \emph{string} to be used in the menus.
;;;		
;;;		\arg{image-name} is a \emph{string}, the application image name.
;;;		
;;;		\arg{modes} is a list of \emph{string}, each representing a mode
;;;		of operation supported by the application and that can be one of:
;;;		\begin{itemize,bulleted}
;;;		\item "su" - single-user mode in file architecture.
;;;		\item "mu" - multi-user mode in file architecture.
;;;		\item "db" - database mode.
;;;		\item "tdmgr-db" - multi-user mode in file architecture mixed
;;;		      with database mode.
;;;		\end{itemize}
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/04	J. P. Varandas	sc-all-crews-applications -> sc-all-applications
;;;-----------------------------------------------------------------------------
(defun sc-define-application (name external-id image-name modes)
  (let ((appl (sc-find-application name)))
    (if appl
	(progn
	  (setf (sc-application-external-id appl) external-id)
	  (setf (sc-application-image-name appl) image-name)
	  (setf (sc-application-modes appl) modes))
	(progn
	  (setf appl (make-sc-application :name name
						:external-id external-id
						:image-name image-name
						:modes modes))
	  (setf *sc-all-applications* (append *sc-all-applications* (list appl)))))))


;;;-----------------------------------------------------------------------------
;;;SC-ADD-APPLICATION-MODE
;;;Description
;;;	Adds a new mode of operation for the application with \arg{name}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a lowercase \emph{string} with the official name
;;;		of the application.
;;;		
;;;		\arg{mode} is a \emph{string} which can be one of: "su", "mu",
;;;		"db" or "tdmgr-db"
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/04	J. P. Varandas	sc-crews-application -> sc-application
;;;-----------------------------------------------------------------------------
(defun sc-add-application-mode (name mode)
  (let ((appl (sc-find-application name)))
    (if appl
	(if (find mode (sc-application-modes appl) :test 'string-equal)
	    (format "Application '%s' already with mode '%s'" name mode)
	    (setf (sc-application-modes appl) (append (sc-application-modes appl) (list mode))))
	(message (format "Application '%s' not found" name)))))


;;;-----------------------------------------------------------------------------
;;;*SC-ALL-SYSTEMS*
;;;Description
;;;	Is a list of \elem{sc-system}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar *sc-all-systems* nil)


;;;-----------------------------------------------------------------------------
;;;DEFSTRUCT SC-SYSTEM
;;;Description
;;;	Is the SC-SYSTEM definition
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\class-attrs
;;;		\emph{name} is a uppercase \emph{string} with the official name
;;;		of the application.
;;;		
;;;		\emph{company-name} is a lowercase \emph{string} used to create
;;;		applications references, e.g., <product>-<company-name>.
;;;		
;;;		\emph{versions} is a list of \emph{string}, the names of the
;;;		released versions and which are under maintenance. The format
;;;		of a version must be the following: v<n>-<n>-<n>. This is used
;;;		also determine the version application directories as, for example,
;;;		crews-x-v1-0-0.
;;;		
;;;		\emph{db-users} is a list of database identifiers. A database
;;;		identifier is a list with two \emph{string} with the syntax:
;;;		(<database control user> <database name>).
;;;		
;;;		\emph{applications} is a list of \elem{sc-application}.
;;;		
;;;		\emph{src-version-dirs} is a list of reference directory locations
;;;		where the source code is stored. The location can be a:
;;;		\begin{itemize,bulleted}
;;;		\item A \emph{string}, an absolute pathname
;;;		\item A \emph{list} that, when it is evaluated, returns an absolute
;;;		      pathname
;;;		\end{itemize}
;;;		
;;;		\emph{send-to} is a \emph{string} with the mail address to whom
;;;		any modification should be sent to.
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	05/10/12	A. Frazao	Removed responsible
;;;	09/02/04	J. P. Varandas	Added slot 'product'
;;;					crews-name -> company-name (POA 13404.0)
;;;-----------------------------------------------------------------------------
(defstruct sc-system
  name
  product
  company-name
  versions
  db-users
  applications
  src-version-dirs
  send-to)


;;;-----------------------------------------------------------------------------
;;;SC-FIND-SYSTEM
;;;Description
;;;	Searches for a system by name.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string}.
;;;
;;;		\arg{key} is a keyword argument whose value is a symbol that
;;;		names a function that is applied to a \elem{sc-system} and that
;;;		should return a string which is compared with \arg{name}. The
;;;		default value is \elem{sc-system-name}.
;;;		
;;;	\return-types
;;;		A \elem{sc-system} whose value returned by \arg{key} is equal to
;;;		\arg{name}, or \emph{nil} if not found.
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-find-system (name &rest keys-values)
  (let ((key (or (cadr (member :key keys-values))
		 'sc-system-name)))
    (find name *sc-all-systems* :key key :test 'string-equal)))


;;;-----------------------------------------------------------------------------
;;;SC-FIND-SYSTEM-COMPANY-NAME
;;;Description
;;;	Searches for a system by the CREWS name.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{company-name} is a \emph{string}.
;;;		
;;;	\return-types
;;;		A \elem{sc-system} with \arg{company-name} or \emph{nil} if not found.
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/04	J. P. Varandas	sc-system-crews-name -> sc-system-company-name
;;;-----------------------------------------------------------------------------
(defun sc-find-system-company-name (company-name)
  (find company-name *sc-all-systems* :key 'sc-system-company-name :test 'string-equal))


;;;-----------------------------------------------------------------------------
;;;SC-DEFINE-SYSTEM
;;;Description
;;;	Defines a system. If there is already a system with \arg{name}, it is
;;;	changed destructively.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is an uppercase \emph{string} with the official name
;;;		of the application.
;;;		
;;;		\arg{company-name} is a lowercase \emph{string} used to create
;;;		applications references, e.g., <product>-<company-name>.
;;;		
;;;		\arg{versions} is a list of \emph{string}, the names of the
;;;		released versions and which are under maintenance. The format
;;;		of a version must be the following: v<n>-<n>-<n>. This is used
;;;		also determine the version application directories as, for example,
;;;		crews-x-v1-0-0.
;;;		
;;;		\arg{db-users} is a list of database identifiers. A database
;;;		identifier is a list with two \emph{string} with the syntax:
;;;		(<database control user> <database name> [<description>]).
;;;		
;;;		\arg{application-names} is a list of \emph{string}, each being a
;;;		valid name of a \elem{sc-application}.
;;;		
;;;		\arg{src-version-dirs} is a list of reference directory locations
;;;		where the source code is stored. The location can be a:
;;;		\begin{itemize,bulleted}
;;;		\item A \emph{string}, an absolute pathname
;;;		\item A \emph{list} that, when it is evaluated, returns an absolute
;;;		      pathname
;;;		\end{itemize}
;;;		
;;;		\arg{send-to} is a \emph{string} with the mail address to whom
;;;		any modification should be sent to.
;;;		
;;;		\arg{responsible} is a \emph{string} with the mail address of
;;;		the system manager.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	04/10/29	A. Frazao	Doc updated
;;;	05/10/12	A. Frazao	Removed responsible
;;;	09/02/04	J. P. Varandas	sc-find-crews-application -> sc-find-application
;;;-----------------------------------------------------------------------------
(defun sc-define-system (name product-name company-name versions db-users application-names src-version-dirs send-to)
  (let ((system       (sc-find-system name))
	(product      (sc-find-product product-name))
	(applications nil))
    (dolist (name application-names)
      (let ((application (sc-find-application name)))
	(if application
	    (push application applications)
	    (message (format "Application '%s' not found" name)))))
    (if product
	(if system
	    (progn
	      (setf (sc-system-company-name system) company-name)
	      (setf (sc-system-versions system) versions)
	      (setf (sc-system-db-users system) db-users)
	      (setf (sc-system-applications system) (reverse applications))
	      (setf (sc-system-src-version-dirs system) src-version-dirs)
	      (setf (sc-system-send-to system) send-to))
	    (progn
	      (setf system (make-sc-system :name name
					   :product product
					   :company-name company-name
					   :versions versions
					   :db-users db-users
					   :applications (reverse applications)
					   :src-version-dirs src-version-dirs
					   :send-to send-to))
	      (setf (sc-product-systems product)
		    (append (sc-product-systems product) (list system)))
	      (setf *sc-all-systems*
		    (append *sc-all-systems* (list system)))))
	(message (format "Product '%s' not found" product-name)))))


;;;-----------------------------------------------------------------------------
;;;SC-ADD-SYSTEM-APPLICATION
;;;Description
;;;	Adds an application to a system.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string}, the name of a system.
;;;		
;;;		\arg{application-name} is a \emph{string}, a valid name of a
;;;		\elem{sc-application}.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/04	J. P. Varandas	sc-find-crews-application -> sc-find-application
;;;-----------------------------------------------------------------------------
(defun sc-add-system-application (name application-name)
  (let ((system (sc-find-system name)))
    (if system
	(let ((application (sc-find-application application-name)))
	  (cond ((null application)
		 (message (format "Application '%s' not found" application-name)))
		((find application (sc-system-applications system))
		 (message (format "Application '%s' already in system '%s'" application-name name)))
		(t (setf (sc-system-applications system) (append (sc-system-applications system) (list application))))))
	(message (format "System '%s' not found" name)))))


;;;-----------------------------------------------------------------------------
;;;SC-ADD-SYSTEM-DB-USER
;;;Description
;;;	Adds a database identifier to a system.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string}, the name of a system.
;;;		
;;;		\arg{db-user} is a database identifier. A database identifier is
;;;		a list with two \emph{string} with the syntax:
;;;		(<database control user> <database name>).
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/04	J. P. Varandas	sc-system-crews-db-users -> sc-system-db-users
;;;-----------------------------------------------------------------------------
(defun sc-add-system-db-user (name db-user)
  (let ((system (sc-find-system name)))
    (if system
	(if (find db-user (sc-system-db-users system) :test 'equal)
	    (format "System '%s' already with DB user '%s'" name db-user)
	    (setf (sc-system-db-users system) (append (sc-system-db-users system) (list db-user))))
	(message (format "System '%s' not found" name)))))


;;;-----------------------------------------------------------------------------
;;;SC-ADD-SYSTEM-VERSION
;;;Description
;;;	Adds a version to a system.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string}, the name of a system.
;;;		
;;;		\arg{version} is a \emph{string}. The format of a version must
;;;		be the following: v<n>-<n>-<n>.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-add-system-version (name version)
  (let ((system (sc-find-system name)))
    (if system
	(if (find version (sc-system-versions system) :test 'string-equal)
	    (format "Version '%s' already in system '%s'" version name)
	    (setf (sc-system-versions system) (append (sc-system-versions system) (list version))))
	(message (format "System '%s' not found" name)))))

;;;-----------------------------------------------------------------------------
;;;SC-RPLAN-SYSTEM?
;;;Description
;;;	Checks if the given system belongs to a rplan product
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{sc-system}
;;;		
;;;	\return-types
;;;		A \elem{boolean}
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun sc-rplan-system? (system)
  (sc-product-rplan? (sc-system-product system)))

;;;-----------------------------------------------------------------------------
;;;SC-GET-SYSTEM-VERSION-DIR
;;;Description
;;;	Builds the system version directory to be added to *sc-local-repository-dir*.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{company-name} is a \emph{string}, the system name.
;;;		
;;;		\arg{version} is a \emph{string}, the version name.
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;		
;;;	\example
;;;		If the value of *sc-local-repository-dir* is "z:/siscog" and the version
;;;		directory is "crews-vdev", the full path of the version directory
;;;		is "z:/siscog/crews-vdev".
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/04	J. P. Varandas	crews-name -> company-name
;;;-----------------------------------------------------------------------------
(defun sc-get-system-version-dir (system version)
  (format "%s-%s-%s" (sc-product-external-id (sc-system-product system)) (sc-system-company-name system) version))


;;;-----------------------------------------------------------------------------
;;;*SC-ALL-FILE-SUBSYSTEMS*
;;;Description
;;;	Is a list that maps directories to CREWS sub-systems. Each element is a
;;;	list with the syntax: (<pathname> <sub-systems>).
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\example
;;;		(("win-maps/allocator/" ("ALLOCATOR-WIN")))
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar *sc-all-file-subsystems* nil)

;;;-----------------------------------------------------------------------------
;;;SC-FILE-SUBSYSTEMS-PATHNAME
;;;Description
;;;	Returns the pathname of a directory to CREWS sub-systems mapping.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{file-subsystems} is a \emph{list}.
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-file-subsystems-pathname (file-subsystems)
  (first file-subsystems))

;;;-----------------------------------------------------------------------------
;;;SC-FILE-SUBSYSTEMS-NAMES
;;;Description
;;;	Returns the sub-systems names of a directory to CREWS sub-systems mapping.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{file-subsystems} is a \emph{list}.
;;;		
;;;	\return-types
;;;		A list of \emph{string}
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-file-subsystems-names (file-subsystems)
  (second file-subsystems))
