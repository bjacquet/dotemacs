;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2004, SISCOG - Sistemas Cognitivos Lda.
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
;;;	Functionality for copying the application's patches.
;;;	
;;; History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;					Added definitions
;;;					  *CREWS-X-PATCHES-DEST-DIR*
;;;					  *CREWS-X-PATCHES-SRC-DIR*
;;;					  *CREWS-PATCHES-DEST-DIR*
;;;					  *CREWS-PATCHES-SRC-DIR*
;;;					  COPY-VERSIONS
;;;					  COPY-RELEVANT-FILES
;;;					  OTHER-SOURCE-FILE-NAMES
;;;					  OTHER-FILES-TO-COPY
;;;					  TIME-SECONDS
;;;					  MAKE-FILE-PATHNAME
;;;					  PATCHES-TO-COPY
;;;					  PATCH-VERSIONS-TO-COPY
;;;					  ENCODE-PATCH-NAME
;;;					  DECODE-PATCH-NAME
;;;					  PATCHES-LAST-VERSIONS
;;;					  PATCH-FILE-NAMES
;;;					  OTHER-FILE-NAMES
;;;					  *OTHER-FILES-TO-COPY*
;;;	25/04/74	Rui Mestre	Added definitions
;;;					  COPY-VERSIONS
;;;	74/10/11	Célia Henriques	Changed definitions
;;;					  COPY-VERSIONS
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;*OTHER-FILES-TO-COPY*
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defvar *other-files-to-copy* nil)
(setq *other-files-to-copy* (list "patches.*\\.data$"))

;;;-----------------------------------------------------------------------------
;;;OTHER-FILE-NAMES
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun other-file-names (dir)
  (let ((names nil))
    (dolist (regexp *other-files-to-copy*)
      (setf names (nconc names (directory-files dir nil regexp))))
    names))

;;;-----------------------------------------------------------------------------
;;;PATCH-FILE-NAMES
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun patch-file-names (dir)
  (directory-files dir nil "^.*\.lisp$"))


;;;-----------------------------------------------------------------------------
;;;PATCHES-LAST-VERSIONS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun patches-last-versions (dir)
  (let ((names (patch-file-names dir))
	(versions nil)
	(patch-data nil)
	(aux-version nil))
    (dolist (name names)
      (setq patch-data (decode-patch-name name))
      (setq aux-version (find (first patch-data) versions :key 'first :test 'equal))
      (if aux-version 
	  (when (> (second patch-data) (second aux-version))
	    (setf (second aux-version) (second patch-data)))
	  (push patch-data versions)))
    versions))



;;;-----------------------------------------------------------------------------
;;;DECODE-PATCH-NAME
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun decode-patch-name (name)
  (let ((pos nil)
	(version nil))
    (setq pos (string-match "\.lisp" name))
    (when pos 
      (setq name (substring name 0 pos)))

    (setq pos (string-match "[0123456789][0123456789][0123456789][0123456789]$" name))
    
    (if pos 
	(list name 0)
	(progn 
    
	  (setq pos (string-match "-[0123456789]+$" name))
	  
	  (if pos
	      (progn
		(setq version (substring name (1+ pos) (length name)))
		(setq name (substring name 0 pos))
		(list name (read version))
		)
	      (list name 0))))))


;;;-----------------------------------------------------------------------------
;;;ENCODE-PATCH-NAME
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{version} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun encode-patch-name (version)
  (concat 
   (if (= (second version) 0)
       (first version)
       (concat (first version) "-" (format "%d" (second version))))
   ".lisp"))

;;;-----------------------------------------------------------------------------
;;;PATCH-VERSIONS-TO-COPY
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{src-dir} is a <>.
;;;		
;;;		\arg{dest-dir} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun patch-versions-to-copy (src-dir dest-dir)
  (let ((src-versions (patches-last-versions src-dir))
	(dest-versions (patches-last-versions dest-dir))
	(versions-to-copy nil)
	(aux-version nil))
    (dolist (src-version src-versions)
      (setq aux-version (find (first src-version) dest-versions :key 'first :test 'equal))
      (when (or
	     (not aux-version)
	     (< (second aux-version) (second src-version)))
	(push src-version versions-to-copy)))
  versions-to-copy))
    
;;;-----------------------------------------------------------------------------
;;;PATCHES-TO-COPY
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{src-dir} is a <>.
;;;		
;;;		\arg{dest-dir} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun patches-to-copy (src-dir dest-dir)
  (mapcar 'encode-patch-name (patch-versions-to-copy src-dir dest-dir)))


;;;-----------------------------------------------------------------------------
;;;MAKE-FILE-PATHNAME
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a <>.
;;;		
;;;		\arg{name} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun make-file-pathname (dir name)
  (concat (file-name-as-directory dir)
	  name))

;;;-----------------------------------------------------------------------------
;;;TIME-SECONDS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{stamp} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun time-seconds (stamp)
  (+ (* (first stamp) (expt 2 16))
     (second stamp)))

;;;-----------------------------------------------------------------------------
;;;OTHER-FILES-TO-COPY
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{src-dir} is a <>.
;;;		
;;;		\arg{dest-dir} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun other-files-to-copy (src-dir dest-dir)
  (let ((src-files (other-file-names src-dir))
	(dest-files (other-file-names dest-dir))
	(dest-file nil)
	(to-copy nil))
    
    (dolist (src-file src-files)
      (if (find src-file dest-files :test 'equal)
	  (let ((src-mod (sixth (file-attributes (make-file-pathname src-dir src-file))))
		(dest-mod (sixth (file-attributes (make-file-pathname dest-dir src-file)))))
	    (when (> (time-seconds src-mod) (time-seconds dest-mod))
	      (push src-file to-copy)))
	  (push src-file to-copy)))
    to-copy))
    

;;;-----------------------------------------------------------------------------
;;;OTHER-SOURCE-FILE-NAMES
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
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun other-source-file-names ()
  (let ((names nil))
    (dolist (regexp *other-files-to-copy*)
      (setf names (nconc names (directory-files *src-dir* nil regexp))))
    names))


;;;-----------------------------------------------------------------------------
;;;COPY-RELEVANT-FILES
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{src-dir} is a <>.
;;;		
;;;		\arg{dest-dir} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defun copy-relevant-files (src-dir dest-dir)
  (interactive "DSource: \nDDestination: ")
  (if (file-directory-p src-dir)
      (progn
	(unless (file-directory-p dest-dir)
	  (message (format "Creating destination directory: %s" dest-dir))
	  (make-directory dest-dir t))
	(let ((patches (patches-to-copy src-dir dest-dir))
	      (other-files (other-files-to-copy src-dir dest-dir))
	      src-path
	      dest-path)
	  (dolist (file (append patches other-files))
	    (setq src-file (make-file-pathname src-dir file))
	    (setq dest-file (make-file-pathname dest-dir file))
	    (message (format "Copying %s to %s" src-file dest-file))
	    (copy-file src-file dest-file t))
	  (message (format "Copied %d files from %s to %s" (length (append patches other-files)) src-dir dest-dir))))
      (message (format "Source directory doesn't exist: %s" src-dir))))



(defun data-set-files-to-copy (src-dir dest-dir src-file-spec dest-file-spec)
  (let* ((base-src-files (directory-files src-dir nil src-file-spec))
	 (base-dest-files (directory-files dest-dir nil (or dest-file-spec src-file-spec)))
	 (dest-file nil)
	 (to-copy nil))
    (do* ((src-files base-src-files (rest src-files))
	  (dest-files base-dest-files (rest dest-files))
	  (src-file (first src-files) (first src-files))
	  (dest-file (first dest-files) (first dest-files)))
	((null src-files))
      (if (or (find src-file base-dest-files :test 'equal)
	      (and dest-file-spec dest-file))
	  (let ((src-mod (sixth (file-attributes (make-file-pathname src-dir src-file))))
		(dest-mod (sixth (file-attributes (make-file-pathname dest-dir (if (and  dest-file-spec dest-file (not (equal src-file dest-file)))
										   dest-file
										   src-file))))))
	    (when (> (time-seconds src-mod) (time-seconds dest-mod))
	      (push src-file to-copy)))
	  (push src-file to-copy)))
    to-copy))


(defun copy-dataset-relevant-files (base-src-dir base-dest-dir)
  (interactive "DSource: \nDDestination: ")
  (do* ((src-dirs *copy-dataset-src-dirs* (rest src-dirs))
	(dest-dirs *copy-dataset-dest-dirs* (rest dest-dirs))
	(src-file-specs *copy-dataset-file-spec* (rest src-file-specs))
	(dest-file-specs *copy-dataset-dest-file-names* (rest dest-file-specs))
	(src (first src-dirs) (first src-dirs))
	(dest (first dest-dirs) (first dest-dirs))
	(src-file-spec (first src-file-specs) (first src-file-specs))
	(dest-file-spec (first dest-file-specs) (first dest-file-specs)))
      ((not (and src-dirs dest-dirs src-file-specs dest-file-specs)))
    (let ((src-dir (concat base-src-dir "/" src))
	  (dest-dir (concat base-dest-dir "/" dest)))
      (if (file-directory-p src-dir)
	  (progn
	    (unless (file-directory-p dest-dir)
	      (message (format "Creating destination directory: %s" dest-dir))
	      (make-directory dest-dir t))
	    (let ((files (data-set-files-to-copy src-dir dest-dir src-file-spec dest-file-spec))
		  src-path
		  dest-path)
	      (dolist (file files)
		(setq src-file (make-file-pathname src-dir file))
		(setq dest-file (make-file-pathname dest-dir (if dest-file-spec dest-file-spec file)))
		(message (format "Copying %s to %s" src-file dest-file))
		(copy-file src-file dest-file t))
	      (message (format "Copied %d files from %s to %s" (length files) src-dir dest-dir))))
	  (message (format "Source directory doesn't exist: %s" src-dir))))))


(defun copy-own-patches (src-dir dest-dir src-file-spec)
  (interactive "DSource: \nDDestination: ")
  (if (file-directory-p src-dir)
      (progn
	(unless (file-directory-p dest-dir)
	  (message (format "Creating destination directory: %s" dest-dir))
	  (make-directory dest-dir t))
	    (let ((files (data-set-files-to-copy src-dir dest-dir src-file-spec nil))
		  src-path
		  dest-path)
	      (dolist (file files)
		(setq src-file (make-file-pathname src-dir file))
		(setq dest-file (make-file-pathname dest-dir file))
		(message (format "Copying %s to %s" src-file dest-file))
		(copy-file src-file dest-file t))
	      (message (format "Copied %d files from %s to %s" (length files) src-dir dest-dir))))
      (message (format "Source directory doesn't exist: %s" src-dir))))


(defvar *copy-dataset-src-dirs* nil)
(setq *copy-dataset-src-dirs* (list "" "" "" "labour-rules" "dictionaries"))

(defvar *copy-dataset-dest-dirs* nil)
(setq *copy-dataset-dest-dirs* (list "" "personnel-1" "guards-1-1" "labour-rules" "dictionaries"))

(defvar *copy-dataset-file-spec* nil)
(setq *copy-dataset-file-spec* (list "^.*\.lisp$" "addit-knowledge.data" "addit-knowledge-guards.data"  "^.*\.lisp$" "^.*\.dic$"))

(defvar *copy-dataset-dest-file-names* nil)
(setq *copy-dataset-dest-file-names* (list nil nil "addit-knowledge.data" nil nil))


(defvar *company* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; USAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;-----------------------------------------------------------------------------
;;;*CREWS-PATCHES-SRC-DIR*
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defvar *crews-patches-src-dir* nil)
(setq *crews-patches-src-dir* "")

;;;-----------------------------------------------------------------------------
;;;*CREWS-PATCHES-DEST-DIR*
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defvar *crews-patches-dest-dir* nil)
(setq *crews-patches-dest-dir* "")

;;;-----------------------------------------------------------------------------
;;;*CREWS-X-PATCHES-SRC-DIR*
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defvar *crews-x-patches-src-dir* nil)
(setq *crews-x-patches-src-dir* "")

;;;-----------------------------------------------------------------------------
;;;*CREWS-X-PATCHES-DEST-DIR*
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	79/11/22	amateus 	Created
;;;-----------------------------------------------------------------------------
(defvar *crews-x-patches-dest-dir* nil)
(setq *crews-x-patches-dest-dir* "")


(defvar *crews-x-data-dir* nil)

(defvar *dataset* nil)

;;;-----------------------------------------------------------------------------
;;;  Prepare to actualise siscog-util-patches
;;;-----------------------------------------------------------------------------
(defvar *siscog-util-patches-src-dir*)
(setq *siscog-util-patches-src-dir* "")


(defvar *siscog-util-patches-dest-dir*)
(setq  *siscog-util-patches-dest-dir* "")
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;COPY-VERSIONS
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
;;;	79/11/22	amateus 	Created
;;;	25/04/74	Rui Mestre	Created
;;;	74/10/11	Célia Henriques	Added siscog-util
;;;-----------------------------------------------------------------------------
(defun copy-versions ()
  (interactive)
  (copy-relevant-files *product-patches-src-dir* *product-patches-dest-dir*)
  (copy-relevant-files *product-x-patches-src-dir* *product-x-patches-dest-dir*)
  (copy-relevant-files *siscog-util-patches-src-dir* *siscog-util-patches-dest-dir*)
;;;  (when (and *dataset* *crews-x-data-dir*)
;;;    (copy-dataset-relevant-files *crews-x-data-dir* (concat (getenv "CREWS_DATA_DIR") "/" *dataset*))
;;;    (copy-dataset-relevant-files (concat (getenv "CREWS_DIR") "/" *company* "-data/" *company*) (concat (getenv "CREWS_DATA_DIR") "/" *dataset*)))
;;;  (when *company*
;;;    (copy-own-patches *default-mod-dir* *crews-patches-dest-dir* "\\patch-crews-0")
;;;    (copy-own-patches *default-mod-dir* *crews-x-patches-dest-dir* (concat "\\patch-crews-" *company*)))
  (switch-to-buffer-other-window (get-buffer "*Messages*")))




