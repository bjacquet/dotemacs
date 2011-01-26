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
;;;					Changed definitions
;;;					  INSTALL-IMAGE-SENTINEL
;;;					  INSTALL-DISTRIBUTION-PROCESS-SYSTEM
;;;					  INSTALL-IMAGE-PROCESS-FORM
;;;					  PROCESS-FINAL-DISTRIBUTION
;;;	10/03/02	Rui Patrocinio	Changed definitions
;;;					  INSTALL-IMAGE-PROCESS-SYSTEM
;;;	82/06/20	P. Madeira	Changed definitions
;;;					  INSTALL-IMAGE-PROCESS-FORM
;;;					  INSTALL-IMAGE-SENTINEL
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;(SETQ MAX-LISP-EVAL-DEPTH)
;;;Description
;;;	Enlarges the Emacs Lisp maximum evaluation depth. Required by \elem{recreate-all-images}.
;;;	
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(setq max-lisp-eval-depth 1000)

;;;-----------------------------------------------------------------------------
;;;INSTALL-IMAGE variables
;;;Description
;;;	Variables that support the functionality to install images
;;;	
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *install-image-args* nil)
(defvar *install-image-product* nil)
(defvar *install-image-company* nil)
(defvar *install-image-no-design* nil)
(defvar *install-image-with-command* nil)
(defvar *install-distribution-postfix* nil)
(defvar *install-image-collect-keywords* nil)


;;;-----------------------------------------------------------------------------
;;;APPLICATION-IMAGE-P
;;;Description
;;;	Checks if \arg{system} corresponds to a valid application image.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \emph{string}.
;;;		
;;;	\return-types
;;;		A \emph{boolean}
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun application-image-p (system)
  (if (string-match "-win$" system)
      (setf system (subseq system 0 (match-beginning 0))))
  (sc-find-application system))


;;;-----------------------------------------------------------------------------
;;;INSTALL-IMAGE-RESET-VARS
;;;Description
;;;	Resets installation process parameters.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{args} is a list of arguments for the installation procedure.
;;;		
;;;	\return-types
;;;		void
;;;
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun install-image-reset-vars (args)
  (setq *install-image-args* args)
  (setq *install-image-product* nil)
  (setq *install-image-company* nil)
  (setq *install-image-no-design* nil)
  (setq *install-image-with-command* nil)
  (setq *install-distribution-postfix* nil)
  (setq *install-image-collect-keywords* nil))

(defun delete-file-if (file)
  (if (file-exists-p file)
      (delete-file file)))

(defun make-env-file (env name)
  (format "%s/%s" (getenv env) name))

;;;-----------------------------------------------------------------------------
;;;UP-DIRECTORY
;;;Description
;;;	Transform the given directory (\arg{dir}) into its parent directory
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a \emph{string} representing a pathname
;;;		
;;;	\return-types
;;;		A \emph{string} representing a pathname
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/09/06	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun up-directory (dir)
  (substring dir 0 (position 47 dir :from-end t)))


;;;-----------------------------------------------------------------------------
;;;PURGE-COMPILED-FILES
;;;Description
;;;	Deletes all files with a given extension (\arg{type}) in the given 
;;;	directories
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{type} is a \emph{string} representing a file extension
;;;		
;;;		\arg{directories} is a list of \emph{pathname}s
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun purge-compiled-files (type directories)
  (dolist (directory directories)
    (let ((names (directory-files directory)))
      (dolist (name names)
	(unless (member name '("." ".."))
	  (let ((path (format "%s/%s" directory name)))
	    (cond ((file-directory-p path)
		   (purge-compiled-files type (list path)))
		  ((string-match type name)
		   (delete-file path)))))))))


;;;-----------------------------------------------------------------------------
;;;PROCESS-FINAL-DISTRIBUTION
;;;Description
;;;	Processes all files to locate in the distribution
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{product} is a \emph{string}
;;;		
;;;		\arg{company} is a \emph{string}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	99/02/27	A. Frazao	Created
;;;	00/09/06	A. Frazao	Updated location of install dir
;;;	08/07/08	A. Frazao	New environment variables usage
;;;	09/02/10	J. P. Varandas	Receive the \arg{product}
;;;-----------------------------------------------------------------------------
(defun process-final-distribution (product company)
  (let ((distr-dir (format "%s/distribution" (getenv (format "%s_%s_DIR" (upcase product) (upcase company))))))
    (dolist (installation (product-distributions product company))
      (let ((destination (format "%s/%s" distr-dir (car installation))))
	(sc-make-directory destination)
	(dolist (file (cadr installation))
	  (sc-copy-file file (format "%s/%s" destination (file-name-nondirectory file))))
	(dolist (dir (caddr installation))
	  (sc-copy-directory dir (format "%s/%s" destination (file-name-nondirectory dir))))))))


;;;-----------------------------------------------------------------------------
;;;PRODUCT-DISTRIBUTIONS
;;;Description
;;;	Call the appropriated function to perform the distribution of the 
;;;	production images.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{product} is a \emph{string}
;;;		
;;;		\arg{company} is a \emph{string}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/10	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun product-distributions (product company)
  (cond ((string-equal product "crews")
	 (crews-distributions company))
	((string-equal product "fleet")
	 (fleet-distributions company))
	((string-equal product "ontime")
	 (ontime-distributions company))))


;;;-----------------------------------------------------------------------------
;;;INSTALL-IMAGE-PROCESS-FORM
;;;Description
;;;	Processes a form in Allegro to create images.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{company} is a \emph{string}
;;;		
;;;		\arg{system} is a \emph{string}
;;;		
;;;		\arg{form} is a lisp form
;;;		
;;;	\return-types
;;;		void
;;;
;;;History
;;;	Date		Author		Description
;;;	99/08/27	A. Frazao	Created
;;;	99/09/01	A. Frazao	Add a sleep when launching from the Emacs
;;;	00/09/06	A. Frazao	Uses CREWS_X_VERSION_DIR
;;;	02/11/21	A. Frazao	Changed sleep to 5
;;;	08/07/08	A. Frazao	New environment variables usage
;;;	09/02/10	J. P. Varandas	Receive the \arg{product}
;;;					Generalise for any product
;;;	82/06/20	P. Madeira	Use new functions defined in sc-allegro.el
;;;-----------------------------------------------------------------------------
(defun install-image-process-form (product company system form)
  (if *install-image-with-command*
      (let* ((old-buf (current-buffer))
	     (filename (format "%s-%s.cmd" company system))
	     (file (format "%s/%s" (getenv (format "~a_~a_DIR" (upcase product) (upcase company))) filename))
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
	(repl-newline)
	(insert (format "%S" form))
	(repl-newline)
	(set-process-sentinel (lisp-process) 'install-image-sentinel)))
  )


;;;-----------------------------------------------------------------------------
;;;INSTALL-IMAGE-PROCESS-SYSTEM
;;;Description
;;;	Creates an development image of the given \elem{product}
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{company} is a \emph{string}
;;;		
;;;		\arg{system} is a \emph{string}
;;;		
;;;		\arg{system} is a \emph{string}
;;;		
;;;		\arg{compile-only} is a \emph{boolean}
;;;		
;;;	\return-types
;;;		void
;;;
;;;	\implem-notes
;;;		ELI depends on the fact that (fi::temporary-directory) in emacs and
;;;		(sys:temporary-directory) in allegro return the same value.
;;;		sys:*temporary-directory* is set to 'nil' so that (sys:temporary-directory)
;;;		works correctly, returning the same as (fi::temporary-directory);
;;;		otherwise (sys:temporary-directory) returns the sys:*temporary-directory*
;;;		that was cached when creating the image.
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
;;;	08/07/08	A. Frazao	New environment variables usage and installation procedure
;;;	08/07/31	A. Frazao	Adds collection of translation keywords
;;;	09/02/10	J. P. Varandas	Receive the \arg{product} and \arg{company}
;;;					Generalise for any product
;;;					install-crews-image -> install-images
;;;	10/03/02	Rui Patrocinio	Cleanup sys:*temporary-directory* (POA 16535.0)
;;;-----------------------------------------------------------------------------
(defun install-image-process-system (product company system compile-only)
  (sleep-for 5)
  (let* ((company-dir (getenv (format "%s_%s_DIR" (upcase product) (upcase company))))
	 (install-system (intern (format ":%s" system)))
	 (load-fn (if compile-only
		      `(,(intern (format "load-%s-%s" product company)) :application ,install-system)
		      `(,(intern (format "install-%s-%s-image" product company)) ,install-system)))
	 (installer (format "%s/%s-%s/%s"
			    company-dir
			    product
			    company
			    (format "installer-%s-%s.lisp" product company))))
    (when *install-image-collect-keywords*
      (let ((keywords-file (format "%s/%s" company-dir "keywords.data"))
	    (dictionary-file (format "%s/%s" company-dir "keywords.dic")))
	(setf load-fn (append load-fn
			      `(:keywords-error-p nil :keywords-file ,keywords-file :dictionary-file ,dictionary-file)))))
    (let ((lambda `(let ((appl ,system))
		    (load ,installer)
		    ;; cleanup ELI used variable
		    (setf sys:*temporary-directory* nil)
		    ,(if compile-only
			 `(progn
			   (format t "~%Compiling ~a-~a~%" ,(upcase company) ,(upcase system))
			   ,load-fn)
			 `(progn
			   (format t "~%Installing ~a-~a~%" ,(upcase company) ,(upcase system))
			   ,load-fn))
		    ,(cond (*windows-emacs* '(WIN:FATALEXIT 0))
			   (*linux-emacs*   '(excl::exit))))))
      (install-image-process-form product company system lambda))))



;;;-----------------------------------------------------------------------------
;;;INSTALL-DISTRIBUTION-PROCESS-SYSTEM
;;;Description
;;;	Installs the image distributions
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{company} is a \emph{string}
;;;		
;;;		\arg{system} is a \emph{string}
;;;		
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
;;;	08/07/08	A. Frazao	New environment variables usage and installation procedure
;;;	09/02/10	J. P. Varandas	Receive the \arg{product} and \arg{company}
;;;					Generalise for any product
;;;-----------------------------------------------------------------------------
(defun install-distribution-process-system (product company system)
  (sleep-for 5)
  (let* ((company-dir (getenv (format "%s_%s_DIR" (upcase product) (upcase company))))
	 (install-system (intern (format ":%s" system)))
	 (installer (format "%s/%s-%s/%s"
			    company-dir
			    product
			    company
			    (format "installer-%s-%s.lisp" product company)))
	 (load-fn `(,(intern (format "install-%s-%s-distribution" product company)) ,install-system)))
    (let ((lambda `(progn
		    (load ,installer)
		    ,load-fn)))
      (install-image-process-form product company system lambda))))


;;;-----------------------------------------------------------------------------
;;;INSTALL-IMAGE-CHECK-ARGS
;;;Description
;;;	Checks the arguments to build a product system's application images.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{args} is a list of \emph{string}.
;;;		
;;;	\return-types
;;;		A \emph{boolean}, \emph{true} if the arguments are correct or
;;;		\emph{nil} otherwise.
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Added command arguments "-cid" "-id" "-d" "-cd" "-dp" "-cmd"
;;;	99/02/27	A. Frazao	Added command arguments "-f" "-fd"
;;;	01/09/27	A. Vasconcelos	Updated for CREWS_ML and CREWS_DSB.
;;;	02/01/13	Fausto		Updated for CREWS_STOG.
;;;	02/01/31	Pedro Matos	Updated for CREWS_VR.
;;;	02/05/21	Fausto		Updated for CREWS_DSB.
;;;	02/07/18	Carlos Ribeiro	Updated for CREWS_BRISA.
;;;	02/12/09	A. Frazao	Updated for CREWS_SISCOG
;;;	03/07/28	A. Frazao	Use crews name
;;;	08/07/08	A. Frazao	Use 'sc-crews-application-name
;;;	08/07/31	A. Frazao	Added commend argument "-dic"
;;;	09/02/04	J. P. Varandas	sc-find-crews-application -> sc-find-application
;;;					sc-crews-application-name -> sc-application-name
;;;					sc-system-crews-name -> sc-system-company-name
;;;	09/02/10	J. P. Varandas	Added commend argument "-prod"
;;;					Checks if the product was defined.
;;;-----------------------------------------------------------------------------
(defun install-image-check-args (args)
  (let ((product-name-p nil)
	(company-name-p nil))
    (do ()
	((null args))
      (let ((arg (pop args)))
	(cond ((string= arg "-prod")
	       (let ((product-name (upcase (pop args))))
		 (cond ((null product-name)
			(error "Product name not supplied after -prod"))
		       ((not (sc-find-product product-name))
			(error (format "Product name %s not found in product definitions" product-name)))
		       (t (setq product-name-p t)))))
	      ((string= arg "-co")
	       (if (null product-name-p)
		   (error "Must supply product name (-prod) before %s" arg)
		   (let ((company-name (pop args)))
		     (cond ((null company-name)
			    (error "Company name not supplied after -co"))
			   ((not (sc-find-system company-name :key 'sc-system-company-name))
			    (error (format "Company name %s not found in systems definitions" company-name)))
			   (t (setq company-name-p t))))))
	      ((string= arg "-pu")
	       (cond ((null product-name-p)
		      (error "Must supply product name (-prod) before %s" arg))
		     ((null company-name-p)
		      (error "Must supply company name (-co) before %s" arg))))
	      ((string= arg "-nd"))
	      ((string= arg "-dic"))
	      ((string= arg "-f")
	       (pop args))
	      ((string= arg "-dp")
	       (pop args))
	      ((string= arg "-cmd"))
	      ((string= arg "-fd")
	       (cond ((null product-name-p)
		      (error "Must supply product name (-prod) before %s" arg))
		     ((null company-name-p)
		      (error "Must supply company name (-co) before %s" arg))))
	      ((member arg '("-i" "-c" "-ci" "-cid" "-id" "-d" "-cd"))
	       (let ((appl-image-name (pop args)))
		 (cond ((null product-name-p)
			(error "Must supply product name (-prod) before %s" arg))
		       ((null company-name-p)
			(error "Must supply company name (-co) before %s" arg))
		       ((null appl-image-name)
			(error "Application image name not supplied after %s" arg))
		       ((if *old-products-configuration*
			    (not (sc-find-application appl-image-name :key 'sc-application-image-name))
			    (not (sc-find-application appl-image-name :key 'sc-application-name)))
			(error "Unknown application image name %s" appl-image-name)))))
	      (t (error "Unknown argument: %s" arg)))))
    t))

;;;-----------------------------------------------------------------------------
;;;INSTALL-IMAGE-PROCESS-NEXT-ARG
;;;Description
;;;	Processes the next argument when processing images.
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
;;;	99/01/12	A. Frazao	Added command arguments "-cid" "-id" "-cd" "-d" "-dp" "-cmd"
;;;	99/02/27	A. Frazao	Added command arguments "-f" "-fd"
;;;	08/07/08	A. Frazao	New environment variables usage
;;;	08/07/31	A. Frazao	Added command argument "-dic"
;;;	09/02/10	J. P. Varandas	Added commend argument "-prod"
;;;					Generalised for any product
;;;					Also handles the '*old-products-configuration*'
;;;					install-crews-image -> install-images
;;;-----------------------------------------------------------------------------
(defun install-image-process-next-arg ()
  (when *install-image-args*
    (let ((arg (pop *install-image-args*)))
      (cond ((string= arg "-pu")
	     ;; Clean compiled files
	     (if *install-image-company*
		 (if *old-products-configuration*
		     (let ((dirs (list (getenv (format "CREWS_%s_VERSION_DIR" (upcase *install-image-company*)))
				       (getenv "CREWS_VERSION_DIR")
				       (getenv "SISCOG_VERSION_DIR"))))
		       (purge-compiled-files "\\.sbin$" dirs)
		       (purge-compiled-files "\\.fasl$" dirs)
		       (install-image-process-next-arg))
		     
		     (let ((dirs (list (format "%s/%s-%s"
					       (getenv (format "%s_%s_DIR" (upcase *install-image-product*) (upcase *install-image-company*)))
					       *install-image-product*
					       *install-image-company*)
				       (format "%s/%s"
					       (getenv (format "%s_DIR" (upcase *install-image-product*)))
					       *install-image-product*)
				       (format "%s/siscog-util" (getenv "SISCOG_UTIL_DIR")))))
		       (purge-compiled-files "\\.fasl$" dirs)
		       (install-image-process-next-arg)))
		 (error "No current company")))
	    ((string= arg "-nd")
	     ;; Process without design mode
	     (setf *install-image-no-design* t)
	     (install-image-process-next-arg))
	    ((string= arg "-dic")
	     (setf *install-image-collect-keywords* t)
	     (install-image-process-next-arg))
	    ((string= arg "-f")
	     ;; Process function
	     (funcall (car (read-from-string (pop *install-image-args*))))
	     (install-image-process-next-arg))
	    ((string= arg "-dp")
	     ;; Set the image name postfix
	     (let ((postfix (pop *install-image-args*)))
	       (if (string= postfix "-")
		   (setf *install-distribution-postfix* nil)
		   (setf *install-distribution-postfix* postfix)))
	     (install-image-process-next-arg))
	    ((string= arg "-fd")
	     ;; Finalize distribution
	     (if *old-products-configuration*
		 (process-final-distribution-old *install-image-company*)
		 (process-final-distribution *install-image-product* *install-image-company*))
	     (install-image-process-next-arg))
	    ((string= arg "-cmd")
	     ;; Use COMMAND mode
	     (setf *install-with-command* t)
	     (install-image-process-next-arg))
	    ((string= arg "-prod")
	     ;; Set the product name
	     (setf *install-image-product* (pop *install-image-args*))
	     (unless (string-equal (sc-product-external-id *sc-current-product*) *install-image-product*)
	       (sc-set-product *install-image-product*))
	     (install-image-process-next-arg))
	    ((string= arg "-co")
	     ;; Set the company name
	     (setf *install-image-company* (pop *install-image-args*))
	     (install-image-process-next-arg))
	    ((member arg '("-ci" "-cid" "-id" "-cd"))
	     ;; Process multiple args
	     (let ((system (car *install-image-args*)))
	       (push (format "-%s" (subseq arg 2)) *install-image-args*)
	       (push system *install-image-args*)
	       (push (subseq arg 0 2) *install-image-args*)
	       (install-image-process-next-arg)))
	    ((string= arg "-c")
	     ;; Compile
	     (if *old-products-configuration*
		 (install-crews-image-process-system-old (pop *install-image-args*) t)
		 (install-image-process-system *install-image-product* *install-image-company* (pop *install-image-args*) t)))
	    ((string= arg "-i")
	     ;; Create development image
	     (if *old-products-configuration*
		 (install-crews-image-process-system-old (pop *install-image-args*) nil)
		 (install-image-process-system *install-image-product* *install-image-company* (pop *install-image-args*) nil)))
	    ((string= arg "-d")
	     ;; Make distribution image
	     (if *old-products-configuration*
		 (install-crews-distribution-process-system-old (pop *install-image-args*))
		 (install-distribution-process-system *install-image-product* *install-image-company* (pop *install-image-args*))))
	    ))))


;;;-----------------------------------------------------------------------------
;;;INSTALL-IMAGE-SENTINEL
;;;Description
;;;	Sentinel and filter for network connections. The sentinel currently 
;;;	does nothing, other than prevent the status change message when the 
;;;	connection is closed.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{process} is a \emph{process}.
;;;		
;;;		\arg{status} is a \emph{string}.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	98/11/25	A. Frazao	Updated for Lucid in Solaris Emacs
;;;	09/02/10	J. P. Varandas	Changed name 'install-crews-image-sentinel' -> 'install-image-sentinel'
;;;	82/06/20	P. Madeira	Don't write dash line
;;;-----------------------------------------------------------------------------
(defun install-image-sentinel (process status)
  (install-image-process-next-arg)
  t)


;;;-----------------------------------------------------------------------------
;;;INSTALL-IMAGE
;;;Description
;;;	See function documentation
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
;;;	09/02/10	J. P. Varandas	install-crews-image-reset-vars -> install-image-reset-vars
;;;					install-crews-image-check-args -> install-image-check-args
;;;					install-crews-image-process-next-arg -> install-image-process-next-arg
;;;-----------------------------------------------------------------------------
(defun install-images (str)
  "Processes (compile or install) applications of a PRODUCT
STR is a string that represents the options is composed by a combination of the following options:

-prod <product> - specifies the product for which the next systems should be processed
		  product is one of: crews or fleet
-co <company>   - specifies the company for which the next systems should be are processed
                  company is one of: ns cp nsb wagn
-dic            - specifies the behaviour when a new dictionary keyword is found while
                  compiling code. If this option is not supplied an error is signaled in
                  such cases. If this option is supplied, the system collect the keywords
                  in two files below the directory given by CREWS_<X>_DIR. The files are
                  keywords.dic, in a dictionary format, and keywords.data, with the
                  identification of the source file where the keywords were found.
                  Additionally, when the file keywords.dic is found it is loaded at the
                  begginning. This way a keyword appears only once. To identify all the
                  keywords not translated, these files should be deleted, all the code
                  should be completelly recompiled using this option.
-c <system>     - specifies a system image to be compiled only (no image is created)
                  for the current company (specified in the last -co option)
-i <system>     - specifies a system image to be created for the current company
                  (specified in the last -co option)
-d <system>     - specifies a system distribution to be created for the current company
                  (specified in the last -co option)
-ci  <system>   - They are combinations of the previous arguments. They correspond to process
-cid <system>     sequencially the individual arguments for the specified system for
-id  <system>	  the current company (specified in the last -co option)
		  Example: -ci  <system> is the same as -c <system> -i <system>
-nd             - in the next processes the variable *CREWS-DEVELOPMENT-MODE* is set to T
-dp {<str>|-}   - specifies a postfix to add to the image name:
                  if the following parameter is '-', unsets the prefix;
                  otherwise, uses the postfix in the next images.
-cmd            - Uses command mode to generate the distribution (not in Emacs)
-pu             - deletes all the binary files in the CREWS_VERSION_DIR, SISCOG_VERSION_DIR
                  and the current company version directory (specified in the last -co option)

NOTE: All the arguments are processed sequencially.

Example:
  \"-prod crews -co nsb -pu -c scheduler-win -i scheduler-win -co cp -pu -ci scheduler-win\"
   Does the following:
     Sets the current company to nsb
     Cleans all lisp compiled files (for CREWS and NSB)
     Compiles the scheduler-win code for NSB
     Creates an image for the scheduler-win for NSB
     Sets the current company to cp
     Cleans all lisp compiled files (for CREWS and CP)
     Compiles the scheduler-win code for CP and creates the corresponding image
"
  (interactive (list (read-default-string "Install Images" (or str "") nil)))
  (let ((args (sc-string-to-list str)))
    (when (install-image-check-args args)
      (install-image-reset-vars args)
      (install-image-process-next-arg))))
