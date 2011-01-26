;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2002, SISCOG - Sistemas Cognitivos Lda.
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
;;;	This file contains the definitions for searching patterns in files.
;;;
;;;	See the documentation of the following definitions for usage:
;;;		*grepfc-top-dir*
;;;		*grepfc-names*
;;;		grepfc-get-crews-dirs
;;;		grepfc
;;;		grepfc-goto-line
;;;
;;;	The following are examples of specialisations that may be done by the user
;;;	in a local initialisation file. They were extracted from the definitions
;;;	documentation.
;;;		(setq *grepfc-top-dir* "z:/siscog")
;;;		(setq *grepfc-names* '("crews" "cp" "ns" "nsb" "wagn"))
;;;		(global-set-key [f2] 'grepfc-goto-line)
;;;
;;;	Examples of usage (extracted from the documentation of grepfc
;;;		(grepfc '("rsr.related.seq") '("lisp" "cl") (grepfc-get-crews-dirs '("crews")) nil "z:/siscog/grepfc.txt")
;;;		(grepfc '("(defmethod do.tasks") '("lisp" "cl") (grepfc-get-crews-dirs) nil "z:/siscog/grepfc.txt")
;;;		(grepfc '("ddm.add.dday") '("lisp") '("z:/siscog/crews-nsb-v1-2-0/patches") nil "z:/siscog/grepfc.txt")
;;;		(grepfc '("Remove UDPT mark") '("lisp" "cl") '("z:/siscog/crews-ns-vdev/crews-ns/win-maps"))
;;; History
;;;	Date		Author		Description
;;;	02/02/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;GREPFC-FILE
;;;Description
;;;	Looks for a pattern in a file.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{pattern-regexp} is a regexp that represents the pattern to be
;;;		searched.
;;;		
;;;		\arg{file} is a file namestring, an absolute pathname.
;;;		
;;;		\arg{comments} is a \emph{boolean}. \emph{true} if comments should
;;;		be considered or \emph{nil} otherwise.
;;;		
;;;		\arg{out-buffer} is an emacs buffer where the output is written.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\remarks
;;;		When a line matches the pattern, it writes the name of the file
;;;		and the line in \arg{out-buffer}.
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/02/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun grepfc-file (pattern-regexp file comments out-buffer)
  (let* ((old-buffer (get-file-buffer file))
	 (buffer (or old-buffer
		     (find-file-noselect file))))
    (set-buffer buffer)
    (save-excursion
      (let ((end nil))
	(beginning-of-buffer)
	(while (not end)
	  (if (search-forward-regexp pattern-regexp nil t)
	      (progn
		(beginning-of-line)
		(if (or comments
			(not (looking-at "[ \t]*;")))
		    (let ((start (point)))
		      (end-of-line)
		      (let ((str (buffer-substring start (point))))
			(set-buffer out-buffer)
			(insert file ": " str 10)
			(if (buffer-file-name out-buffer)
			    (save-buffer))
			(set-buffer buffer))))
		(forward-line))
	      (setf end t)))))
    (unless old-buffer
      (kill-buffer buffer))))

;;;-----------------------------------------------------------------------------
;;;GREPFC-DIRECTORY
;;;Description
;;;	Looks for a pattern in a directory. It searches recursivelly in the
;;;	sub-directories.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{pattern-regexp} is a regexp that represents the pattern to be
;;;		searched.
;;;		
;;;		\arg{type-regexps} is a list of regexps to filter the files where
;;;		search should be done (see \elem{directory-files}).
;;;		
;;;		\arg{directory} is a directory namestring, an absolute pathname.
;;;		
;;;		\arg{comments} is a \emph{boolean}. \emph{true} if comments should
;;;		be considered or \emph{nil} otherwise.
;;;		
;;;		\arg{out-buffer} is an emacs buffer where the output is written.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/02/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun grepfc-directory (pattern-regexp type-regexps directory comments out-buffer)
  (dolist (type-regexp type-regexps)
    (dolist (file (directory-files directory t type-regexp))
      (if (not (file-directory-p file))
	  (grepfc-file pattern-regexp file comments out-buffer))))
  (dolist (dir (directory-files directory t "^[^\.]"))
    (if (file-directory-p dir)
	(grepfc-directory pattern-regexp type-regexps dir comments out-buffer))))

;;;-----------------------------------------------------------------------------
;;;*GREPFC-TOP-DIR*
;;;Description
;;;	The top directory where to locate the crews directories in \elem{grepfc-get-crews-dirs}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\example
;;;		(setq *grepfc-top-dir* "z:/siscog")
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/02/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar *grepfc-top-dir* nil)

;;;-----------------------------------------------------------------------------
;;;*GREPFC-NAMES*
;;;Description
;;;	The names of the systems for which the directories should be considered
;;;	by default in \elem{grepfc-get-crews-dirs}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\example
;;;		(setq *grepfc-names* '("crews" "cp" "ns" "nsb" "wagn"))
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/02/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar *grepfc-names* nil)

;;;-----------------------------------------------------------------------------
;;;GREPFC-GET-CREWS-DIRS
;;;Description
;;;	Returns a list of directories.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{crews-names} is a list of strings that represent a company
;;;		name, e.g., "nsb". To obtain the directory where CREWS code is
;;;		located, use "crews". If the value is \emph{nil}, it uses the value
;;;		of the variable \elem{*grepfc-names*}. It is optional and the default
;;;		value is \emph{nil}.
;;;		
;;;	\return-types
;;;		A list of directories namestrings (absolute pathnames) that correspond
;;;		to \arg{crews-names}.
;;;		
;;;	\example
;;;		(grepfc-get-crews-dirs '("crews")) -> ("z:/siscog/crews-vdev")
;;;
;;;		(grepfc-get-crews-dirs '("crews" "cp")) -> ("z:/siscog/crews-vdev" "z:/siscog/crews-cp-vdev/crews-cp" "z:/siscog/crews-cp-vdev/cp-data/cp")
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/02/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun grepfc-get-crews-dirs (&optional crews-names)
  (let ((dirs nil))
    ;; If CREWS-NAMES is not supplied, use all.
    (if (null crews-names)
	(setq crews-names *grepfc-names*))
    ;; Iterate all the names and collect the directory references for each.
    (dolist (name crews-names)
      (if (equal name "crews")
	  ;; If it is "crews" collect the crews-vdev
	  (setf dirs (cons (format "%s/crews-vdev" *grepfc-top-dir*) dirs))
	  ;; Otherwise, collect the specific crews-x and data directories
	  (progn
	    (setf dirs (cons (format "%s/crews-%s-vdev/crews-%s" *grepfc-top-dir* name name) dirs))
	    (setf dirs (cons (format "%s/crews-%s-vdev/%s-data/%s" *grepfc-top-dir* name name name) dirs)))))
    ;; Return the directories in the order as supplied by the argumments
    (reverse dirs)))

;;;-----------------------------------------------------------------------------
;;;GREPFC
;;;Description
;;;	Looks for a set of patterns in the files of specified types in of a set
;;;	of directories. It searches recursivelly in the sub-directories.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{patterns} is a list of strings to be searched.
;;;		
;;;		\arg{file-types} is a list of file extensions that should be searched.
;;;		
;;;		\arg{directories} is a list of directories namestrings (absolute pathnames)
;;;		where search should be done.
;;;		
;;;		\arg{comments} is a \emph{boolean}. \emph{true} if comments should
;;;		be considered or \emph{nil} otherwise. It is optional and the default
;;;		value is \emph{nil}.
;;;		
;;;		\arg{output-file} is a file namestring, an absolute pathname, where
;;;		the outputs are written. If the value is \emph{nil}, the outputs are
;;;		written in a buffer named \emph{*grepfc*}. It is optional and the
;;;		default value is \emph{nil}. 
;;;		
;;;	\return-types
;;;		void
;;;		
;;;	\example
;;;		(grepfc '("rsr.related.seq") '("lisp" "cl") (grepfc-get-crews-dirs '("crews")) nil "z:/siscog/grepfc.txt")
;;;		
;;;		(grepfc '("(defmethod do.tasks") '("lisp" "cl") (grepfc-get-crews-dirs) nil "z:/siscog/grepfc.txt")
;;;		
;;;		(grepfc '("ddm.add.dday") '("lisp") '("z:/siscog/crews-nsb-v1-2-0/patches") nil "z:/siscog/grepfc.txt")
;;;		
;;;		(grepfc '("Remove UDPT mark") '("lisp" "cl") '("z:/siscog/crews-ns-vdev/crews-ns/win-maps") nil "z:/siscog/grepfc.txt")
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/02/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun grepfc (patterns file-types directories &optional comments output-file)
  (let ((type-regexps nil)
	(pattern-regexp (format "%s" (car patterns)))
	(out-buffer (if output-file
			(find-file-noselect output-file)
			(get-buffer-create "*grepfc*")))
	(old-buffer (current-buffer)))
    (set-buffer out-buffer)
    (end-of-buffer)
    (save-excursion
      (insert "-------------------------------------------------------------------------------" 10)
      (insert "(grepfc " (format "'%S '%S '%S %S" patterns file-types directories comments) ")" 10)
      (insert "-------------------------------------------------------------------------------" 10)
      (dolist (pattern (cdr patterns))
	(setq pattern-regexp (format "%s\\|%s" pattern-regexp pattern)))
      (dolist (type file-types)
	(setq type-regexps (cons (format "\.%s$" type) type-regexps)))
      (insert "(grepfc " (format "%S" pattern-regexp) ")" 10)
      (let ((old-font-lock-maximum-size font-lock-maximum-size))
	(setf font-lock-maximum-size 0)
	(unwind-protect
	    (dolist (dir directories)
	      (if (file-directory-p dir)
		  (grepfc-directory pattern-regexp type-regexps dir comments out-buffer)
		  (insert dir " is not a directory" 10)))
	  (setf font-lock-maximum-size old-font-lock-maximum-size)))
      (set-buffer old-buffer)
      (if (not (equal out-buffer old-buffer))
	  (switch-to-buffer-other-window out-buffer))
      )))

;;;-----------------------------------------------------------------------------
;;;GREPFC-GOTO-LINE
;;;Description
;;;	Looks for the file reference that appears in the beggining of the current
;;;	line and opens it in a buffer (if not yet open). Then, searches for the
;;;	line string that follows the file reference.
;;;
;;;	It can be assigned to a keyboard key
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
;;;	\example
;;;		Assignment of a key so that when the user points to a line of the
;;;		grepfc output and presses it, it locates the place where the line is.
;;;		
;;;		(global-set-key [f2] 'grepfc-goto-line)
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/02/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun grepfc-goto-line ()
  (interactive)
  (let ((start nil)
	(file nil)
	(pattern nil))
    (save-excursion
      (beginning-of-line)
      (setq start (point))
      (while (not (= (char-after (point)) 32))
	(forward-char))
      (forward-char -1)
      (setq file (buffer-substring start (point)))
      (forward-char 2)
      (setq start (point))
      (end-of-line)
      (setq pattern (buffer-substring start (point))))
    (switch-to-buffer-other-window (find-file-noselect file))
    (beginning-of-buffer)
    (search-forward pattern)
    (beginning-of-line)))
