;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 1900, SISCOG - Sistemas Cognitivos Lda.
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
;;; Description
;;;	Implements a functionality to extract the files that were changed since
;;;	a certain date.
;;;	Function: update-crews-files-from-date
;;;	Args:
;;;		from-dir
;;;			A directory from where the new or modified files are collected.
;;;			From this directory, it collects the changes of files and sub-directories
;;;			defined in the variable *make-siscog-names*
;;;		to-dir
;;;			A directory where the new or modified files are copied to
;;;		date
;;;			The date from which the new or modified files are selected
;;;			The date has the format yyyy/mm/dd
;;;	Example:
;;;		(update-crews-files-from-date "x:" "z:/update-crews" 20000201)
;;; History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;					Added definitions
;;;					  UPDATE-CREWS-FILES-FROM-DATE
;;;					  COLLECT-CREWS-FILES-TO-UPDATE-FOR-DATE
;;;					  CREWS-FILE-CHANGED-AFTER-DATE-P
;;;	00/10/10	A. Frazao	Changed definitions
;;;					  COLLECT-CREWS-FILES-TO-UPDATE-FOR-DATE
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  COLLECT-CREWS-FILES-TO-UPDATE-FOR-DATE
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;CREWS-FILE-CHANGED-AFTER-DATE-P
;;;Description
;;;	Returns TRUE if the file was modified after DATE
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun crews-file-changed-after-date-p (file date)
  (sc-date< date (sc-date-from-timeval (elt (file-attributes file) 5))))

;;;-----------------------------------------------------------------------------
;;;COLLECT-CREWS-FILES-TO-UPDATE-FOR-DATE
;;;Description
;;;	Collects all the files to be copied.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{from} is a pathname string.
;;;		
;;;		\arg{to} is a pathname string.
;;;		
;;;		\arg{date} is a date, with the format yyyymmdd.
;;;		
;;;	\return-types
;;;		A list with the syntax ({(<origin> <destination>)}*)
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;	00/10/10	A. Frazao	Create the directory when copying isolated
;;;					files.
;;;	09/02/10	J. P. Varandas	sc-crews-file-p -> sc-rplan-file-p
;;;-----------------------------------------------------------------------------
(defun collect-crews-files-to-update-for-date (from to date)
  (let ((files nil))
    (cond ((file-directory-p from)
	   (dolist (name (directory-files from))
	     (if (sc-rplan-file-p name from)
		 (let ((orig (format "%s/%s" from name))
		       (dest (format "%s/%s" to name)))
		   (if (file-directory-p orig)
		       (let ((new-files (collect-crews-files-to-update-for-date orig dest date)))
			 (when (and new-files
				    (not (file-directory-p dest)))
			   (sc-make-directory dest))
			 (setf files (append new-files files)))
		       (when (crews-file-changed-after-date-p orig date)
			 (push (list orig dest) files)))))))
	  ((file-exists-p from)
	   (when (crews-file-changed-after-date-p from date)
	     (let ((dest (file-name-directory to)))
	       (if (not (file-directory-p dest))
		   (sc-make-directory (file-name-directory to))))
	     (push (list from to) files))))
    files))

;;;-----------------------------------------------------------------------------
;;;UPDATE-CREWS-FILES-FROM-DATE
;;;Description
;;;	Copies all the files in source-directory, that are new or modified after
;;;	date, into destination-directory
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun update-crews-files-from-date (source-directory destination-directory date)
  (when (file-directory-p destination-directory)
    (sc-delete-directory destination-directory))
  (sc-make-directory destination-directory)
  (let ((buf (current-buffer))
	(output (get-buffer "log.txt"))
	(current-date (sc-current-date)))
    (when output
      (kill-buffer output))
    (setq output (find-file-noselect (format "%s/log.txt" destination-directory)))
    (set-buffer output)
    (erase-buffer)
    (insert "update-crews-files-from-date" 10)
    (insert (format "  From directory: %s" source-directory) 10)
    (insert (format "  To directory  : %s" destination-directory) 10)
    (insert (format "  From Date     : %d/%02d/%02d" (sc-date-year date) (sc-date-month date) (sc-date-day date)) 10)
    (insert (format "  Current Date  : %d/%02d/%02d"
		    (sc-date-year current-date) 
		    (sc-date-month current-date)
		    (sc-date-day current-date))
	    10 10)
    (dolist (name *make-siscog-names*)
      (let ((files (collect-crews-files-to-update-for-date (format "%s/%s" source-directory name)
							   (format "%s/%s" destination-directory name)
							   date)))
	(dolist (file files)
	  (insert (format "%s" (car file)) 10)
	  (sc-copy-file (car file) (cadr file)))))
    (save-buffer output)
    (set-buffer buf)
    ))
