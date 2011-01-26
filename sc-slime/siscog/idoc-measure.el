;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 1999, SISCOG - Sistemas Cognitivos Lda.
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
;;;                           1700 LISBOA
;;;                             PORTUGAL
;;;
;;;-----------------------------------------------------------------------------
;;; Description
;;;	Gives a set of functions to ease the measure of some Internal Documentation
;;;    statistical results: measure.system, measure-dir.
;;;
;;; History
;;;	Date		Author		Description
;;;     99/11/16        cpla            Created.
;;;	00/06/01	cpla    	Added measure of siscog-util.
;;;	00/11/03	cpla    	Removed "work-recorder" from %ignore-modules-crews%.
;;;	02/06/07	J. P. Varandas	Added definitions
;;;					  MEASURE-CREWS-X-MAPS
;;;					  MEASURE-CREWS-X
;;;					  MEASURE-WINGRAPH
;;;					  MEASURE-SISCOG-UTIL
;;;					  MEASURE-WIN-MAPS
;;;					  PRINT-STAT
;;;					  GET.MOD.DATE
;;;					  PRINT-COUNTERS
;;;					  COLLECT-ELEM-WITHOUT-FULLY-DESC
;;;					  COLLECT-ELEM-WITH-UNKNOWN-CONST
;;;					  COLLECT-ELEM-WITHOUT-DESC
;;;					  COLLECT-ELEM-WITHOUT-HEADER
;;;					  RESET-COUNTERS
;;;					  *ELEMS-WITHOUT-FULLY-DESC*
;;;					  *ELEMS-WITH-UNKNOWN-CONST*
;;;					  *ELEMS-WITHOUT-DESC*
;;;					  *ELEMS-WITHOUT-HEADER*
;;;					  %STAT-PRETTY-PRINT%
;;;					  %CONTROL-BY-ELEMENT%
;;;					  %CONTROL-BY-FILE%
;;;					  SHOW-RESULT-BUFFER
;;;					  FIND-RESULT-BUFFER
;;;					  GET-RESULT-BUFFER
;;;					Changed definitions
;;;					  MEASURE-CREWS
;;;					  MEASURE.SYSTEM
;;;					  MEASURE.MODULE
;;;					  GET.MODULE.FILES
;;;					  MEASURE-DIR
;;;					  IGNORE.ELEMENT.P
;;;					  MEASURE-FILE
;;;					  MEASURE-HEADER
;;;					  GET.ELEMENT.AND.HEADER.LIMITS
;;;					  MODIFIED.AFTER.DATE.P
;;;					  PRINT.UNKNOWN.CONSTRUCTORS
;;;					  (SETQ %SELECT-FILE-REGEXPS%)
;;;					Deleted definitions
;;;					  PRINT.UNDOCUMENTED.ELEMS
;;;					  COLLECT.UNDOCUMENTED.ELEM
;;;					  RESET.UNDOCUMENTED.ELEMS
;;;					  *UNDOCUMENTED-ELEMS*
;;;					  %RESULT-FILE%
;;;	05/10/13	J. P. Varandas	Changed definitions
;;;					  MEASURE-BUFFER
;;;					  MEASURE-FILE
;;;	10/09/22	A. Frazao	Changed definitions
;;;					  MEASURE-WIN-MAPS
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;GET-RESULT-BUFFER
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun get-result-buffer ()
  (let ((buf (get-buffer "*MEASURE-ELEMENTS*")))
    (when buf
      (kill-buffer buf))
    (setq buf (get-buffer-create "*MEASURE-ELEMENTS*"))
    (set-buffer buf)
    (erase-buffer)
    buf))

;;;-----------------------------------------------------------------------------
;;;FIND-RESULT-BUFFER
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun find-result-buffer ()
  (let ((buf (get-buffer "*MEASURE-ELEMENTS*")))
    (when buf
      (set-buffer buf))
    buf))

;;;-----------------------------------------------------------------------------
;;;SHOW-RESULT-BUFFER
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun show-result-buffer ()
  (switch-to-buffer-other-window (find-result-buffer)))

(defvar %sample-base-date%)
(setq %sample-base-date% nil) ;;(make-sc-date 2001 01 01)

(defvar %log-file%)
(setq %log-file% "z:/tmp/log.txt")

(defvar %output-dir%)
(setq %output-dir% "z:/tmp/set")

(defvar %crews-dir%)
(setq %crews-dir% "m:/crews-vdev/crews")

(defvar %ignore-file-regexps%)
(setq %ignore-file-regexps% (list (regexp-quote "system-defs.lisp")
				  (regexp-quote "system.lisp")
				  "~$") ; Ignore file names with ~ at the end.
      )


(defvar %ignore-modules-crews%)
(setq %ignore-modules-crews% '("-x-maps" "-hci-plug" "-win-maps" "crews-search-panel"))


(defvar %ignore-modules-scg-util%)
(setq %ignore-modules-scg-util% '("high-level-util" "sike-maps" "reader" "mymeter" "meter" "graphics"))


(defvar *ignore-modules* nil)
			 

(defvar %ignore-constructor-regexps%)
(setq %ignore-constructor-regexps% (mapcar #'regexp-quote '("in-package"
							    "MAKE-SYSTEM"
							    "LOAD.DICTIONARIES"
							    "LOAD")))

(defvar %select-file-regexps%)
;;;-----------------------------------------------------------------------------
;;;(SETQ %SELECT-FILE-REGEXPS%)
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	
;;;-----------------------------------------------------------------------------
(setq %select-file-regexps% '("\\.cl$" "\\.lisp$" "\\.bil$"))

;;;-----------------------------------------------------------------------------
;;;%CONTROL-BY-FILE%
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar %control-by-file%)
(setq %control-by-file% nil)

;;;-----------------------------------------------------------------------------
;;;%CONTROL-BY-ELEMENT%
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar %control-by-element%)
(setq %control-by-element% nil)

;;;-----------------------------------------------------------------------------
;;;%STAT-PRETTY-PRINT%
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar %stat-pretty-print%)
(setq %stat-pretty-print% nil)

;;; Collects unknown constructors.
(defvar *unknown-constructors* nil)

(defun reset.unknown.constructors ()
  (setf *unknown-constructors* nil))

(defun collect.unknown.constructor (elem)
  (pushnew elem *unknown-constructors* :test #'string=))

;;;-----------------------------------------------------------------------------
;;;PRINT.UNKNOWN.CONSTRUCTORS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	
;;;-----------------------------------------------------------------------------
(defun print.unknown.constructors ()
  (when *unknown-constructors*
    (insert "UNKNOWN CONSTRUCTORS" 10))
  (dolist (constructor *unknown-constructors*)
    (insert 32 32 constructor 10)))

;;;-----------------------------------------------------------------------------
;;;*ELEMS-WITHOUT-HEADER*
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *elems-without-header* nil)
;;;-----------------------------------------------------------------------------
;;;*ELEMS-WITHOUT-DESC*
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *elems-without-desc* nil)
;;;-----------------------------------------------------------------------------
;;;*ELEMS-WITH-UNKNOWN-CONST*
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *elems-with-unknown-const* nil)
;;;-----------------------------------------------------------------------------
;;;*ELEMS-WITHOUT-FULLY-DESC*
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *elems-without-fully-desc* nil)

;;;-----------------------------------------------------------------------------
;;;RESET-COUNTERS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun reset-counters ()
  (setf *elems-without-header* nil)
  (setf *elems-without-desc* nil)
  (setf *elems-with-unknown-const* nil)
  (setf *elems-without-fully-desc* nil))

;;;-----------------------------------------------------------------------------
;;;COLLECT-ELEM-WITHOUT-HEADER
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{elem} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun collect-elem-without-header (elem)
  (push elem *elems-without-header*))

;;;-----------------------------------------------------------------------------
;;;COLLECT-ELEM-WITHOUT-DESC
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{elem} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun collect-elem-without-desc (elem)
  (push elem *elems-without-desc*))

;;;-----------------------------------------------------------------------------
;;;COLLECT-ELEM-WITH-UNKNOWN-CONST
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{elem} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun collect-elem-with-unknown-const (elem)
  (push elem *elems-with-unknown-const*))

;;;-----------------------------------------------------------------------------
;;;COLLECT-ELEM-WITHOUT-FULLY-DESC
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{elem} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun collect-elem-without-fully-desc (elem)
  (push elem *elems-without-fully-desc*))

;;;-----------------------------------------------------------------------------
;;;PRINT-COUNTERS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun print-counters ()
  (when %control-by-element%
    (when *elems-without-header*
      (insert "*ELEMENTS WITHOUT HEADER*" 10)
      (dolist (elem *elems-without-header*)
	(insert 32 32 elem 10)))
    (when *elems-without-desc*
      (insert "*ELEMENTS WITHOUT STANDARD DOCUMENTATION*" 10)
      (dolist (elem *elems-without-desc*)
	(insert 32 32 elem 10)))
    (when *elems-with-unknown-const*
      (insert "*ELEMENTS WITH UNKNOWN CONSTRUCTOR*" 10)
      (dolist (elem *elems-with-unknown-const*)
	(insert 32 32 elem 10)))
    (when *elems-without-fully-desc*
      (insert "*ELEMENTS WITHOUT FULL DOCUMENTATION*" 10)
      (dolist (elem *elems-without-fully-desc*)
	(insert 32 32 elem 10)))))

(defun match-regexps-p (string regexps)
  (find string regexps :test #'(lambda (str regexp)
				 (string-match regexp str))))


(defun number.of.lines ()
  "Gives current buffer total number of lines."
  (save-excursion
    (let ((lines 0))
      (beginning-of-buffer)
      (while (not (eobp))
	(incf lines)
	(forward-line 1))
      lines)))


(defstruct doc-stat
  (lines 0)
  (files 0)
  (total-elems 0)
  (elems-with-header 0)
  (elems-with-desc 0)
  (elems-fully-desc 0)
  (has-unknown-constructs nil))


(defun is.filled.p (start &optional end)
  (save-excursion
      (goto-char start)
      (ignore-errors (re-search-forward "\\w" end))))


(defun get.last.date ()
  "Gets the last date searching from the current point backwards."
  (when (re-search-backward "[ \t]+\\(..\\)/\\(..\\)/\\(..\\)[ \t]+")
    (let ((year (car (read-from-string (buffer-substring (match-beginning 1) (match-end 1)))))
	  (month (car (read-from-string (buffer-substring (match-beginning 2) (match-end 2)))))
	  (day (car (read-from-string (buffer-substring (match-beginning 3) (match-end 3))))))
      (setf year (+ year (if (> year 80) 1900 2000))) ;%%%CA, Depends on value 80 and year in two digits.
      (make-sc-date year month day))))

;;;-----------------------------------------------------------------------------
;;;GET.MOD.DATE
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun get.mod.date ()
  (when (string-match "\\(..\\)/\\(..\\)/\\(..\\)" *default-mod-date*)
    (let ((year (car (read-from-string (substring *default-mod-date* (match-beginning 1) (match-end 1)))))
	  (month (car (read-from-string (substring *default-mod-date* (match-beginning 2) (match-end 2)))))
	  (day (car (read-from-string (substring *default-mod-date* (match-beginning 3) (match-end 3))))))
      (setf year (+ year (if (> year 80) 1900 2000))) ;%%%CA, Depends on value 80 and year in two digits.
      (make-sc-date year month day))))

;;;(defun modified.after.date.p (date &optional header.point)
;;;  "Verifies if the current element was modified after <date>."
;;;  (save-excursion
;;;    (when header.point
;;;      (goto-char header.point))
;;;    (when (end.of.comment.block)
;;;      (let ((history.last.date (get.mod.date))) ;;get.last.date
;;;	(sc-date< date history.last.date)))))

;;;-----------------------------------------------------------------------------
;;;MODIFIED.AFTER.DATE.P
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{date} is a <>.
;;;		
;;;		\arg{header.point} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	
;;;-----------------------------------------------------------------------------
(defun modified.after.date.p (date &optional header.point)
  "Verifies if the current element was modified after <date>."
  (save-excursion
    (when header.point
      (goto-char header.point))
    (when (end.of.comment.block)
      (let ((history.last.date (get.last.date)))
	(= date history.last.date)))))

;;;-----------------------------------------------------------------------------
;;;GET.ELEMENT.AND.HEADER.LIMITS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	
;;;-----------------------------------------------------------------------------
(defun get.element.and.header.limits ()
  "Gives a list with next constructor and related header limits, if exist.
  If inside a definition, the result is nil."
  (let ((pos (point))
	end
	start
	ctruct
	limits
	res
	def)
    (ignore-errors (end-of-defun))
    (setq end (point))
    (when (ignore-errors (beginning-of-defun))
      (setq start (point))
      (when (>= start pos)
	(setq ctruct (save-excursion (ignore-errors (get.definition.constructor)))) ;; d 
	(setq def (save-excursion (ignore-errors (get-definition-id))))
	(when ctruct 
	  (forward-line -1)
	  (setq limits (doc.region.limits))
	  (setq res (cons ctruct (cons def limits))))))
    (goto-char end)
    res))


(defun attribute.region.end.limit (end)
  "Returns current attribute region end point."
  (let (min.end)
    (dolist (regexp (all.attrs.keywords))
      (save-excursion 
	(when (re-search-forward regexp end t)
	  (setq min.end (or (and min.end (min min.end (point)))
			    (point))))))
    (cond (min.end
	   (goto-char min.end)
	   (forward-line -1)
	   (end-of-line)
	   (point))
	  (t end))))


(defun check.attribute (attr start end)
  (let ((exist (attribute.exist.p attr start end)))
    (when exist
      (goto-char exist)
      (is.filled.p (point) (attribute.region.end.limit end)))))


(defun filter.package (string)
  (let ((idx (position (string-to-char ":") string :from-end t)))
    (if idx
	(subseq string (1+ idx))
	string)))


;;;-----------------------------------------------------------------------------
;;;MEASURE-HEADER
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{elem-limits} is a <>.
;;;		
;;;		\arg{stat} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	
;;;-----------------------------------------------------------------------------
(defun measure-header (elem-limits stat)
  (incf (doc-stat-total-elems stat))
  (let ((elem (car elem-limits))
	(def  (cadr elem-limits))
	(limits (cdr (cdr elem-limits))))
    (block measure-header
      (if (null limits)
	  (collect-elem-without-header def)
	  (let ((start (first limits))
		(end (second limits)))
	    (incf (doc-stat-elems-with-header stat))
	    (goto-char start)
	    (unless (is.filled.p start (attribute.region.end.limit end))
	      (collect-elem-without-desc def)
	      (return-from measure-header)
	      )
	    (incf (doc-stat-elems-with-desc stat))
	    (let ((ctruct (ignore-errors (get.constructor (filter.package elem)))))
	      (when (unknown.p ctruct) 
		(collect.unknown.constructor elem)
		(collect-elem-with-unknown-const def)
		(setf (doc-stat-has-unknown-constructs stat) t)
		(return-from measure-header)
		)
	      (dolist (attr (mandatory.attributes (constructor-attributes ctruct)))
		(unless (check.attribute attr start end)
		  (collect-elem-without-fully-desc def)
		  (return-from measure-header)))
	      (incf (doc-stat-elems-fully-desc stat))))))))
	

(defun unknown.p (cstruct)
  (string= "UNKNOWN" (constructor-keyword cstruct)))


;;;-----------------------------------------------------------------------------
;;;MEASURE-FILE
;;;Description
;;;	Measure the number of elements in the given file that are: with header, 
;;;	with documentation and with full documentation.
;;;	Also collects the elements that are not correctly documented.
;;;	At the end displays the statistics in the respective buffer
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{filename} is a \emph{pathname}
;;;		
;;;		\arg{stat} is a \elem{cl-struct-doc-stat}
;;;		
;;;		\arg{log.p} is a \emph{boolean}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	
;;;	05/10/13	J. P. Varandas	Do not kill the already opened buffers
;;;-----------------------------------------------------------------------------
(defun measure-file (filename stat &optional log.p)
  (let ((local-stat nil))
    (when log.p
      (find-file %log-file%)
      (insert (format "Measuring file %s ..." filename)))
    (let* ((old-buf (get-file-buffer filename))
	   (buf (or old-buf
		    (find-file-noselect filename))))
      (when buf
	(set-buffer buf)
	(when %control-by-file% 
	  (reset-counters)
	  (setq local-stat (make-doc-stat)))
	(measure-buffer (if %control-by-file% local-stat stat))
	(unless old-buf
	  (kill-buffer buf)))
      (when %control-by-file% 
	(find-result-buffer)
	(insert "Measuring file " filename "..." 10)
	(print-counters)
	(print-stat local-stat)
	(when (doc-stat-has-unknown-constructs local-stat)
	  (setf (doc-stat-has-unknown-constructs stat) t))
	(incf (doc-stat-lines stat) (doc-stat-lines local-stat))
	(incf (doc-stat-total-elems stat) (doc-stat-total-elems local-stat))
	(incf (doc-stat-elems-with-header stat) (doc-stat-elems-with-header local-stat))
	(incf (doc-stat-elems-with-desc stat) (doc-stat-elems-with-desc local-stat))
	(incf (doc-stat-elems-fully-desc stat) (doc-stat-elems-fully-desc local-stat)))
      (incf (doc-stat-files stat) 1)
      (when log.p
	(find-file %log-file%)
	(insert (format "Done. \n"))))))


;;;-----------------------------------------------------------------------------
;;;IGNORE.ELEMENT.P
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{element-start-end} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	
;;;-----------------------------------------------------------------------------
(defun ignore.element.p (element-start-end)
  "Verifies if element characterised by <element-start-end> should be ignored in statistics accounting."
  (if %sample-base-date%
      (or (match-regexps-p (car element-start-end) %ignore-constructor-regexps%)
	  (not (second element-start-end)) ; Elements without header are also ignored.
	  (not (modified.after.date.p %sample-base-date% (third element-start-end))))
      (match-regexps-p (car element-start-end) %ignore-constructor-regexps%)))

;;;-----------------------------------------------------------------------------
;;;MEASURE-BUFFER
;;;Description
;;;	Measure the number of elements in the current buffer that are: with header, 
;;;	with documentation and with full documentation.
;;;	Also collects the elements that are not correctly documented.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{stat} is a \elem{cl-struct-doc-stat}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	05/10/13	J. P. Varandas	Use 'save-excursion' at the begining
;;;-----------------------------------------------------------------------------
(defun measure-buffer (stat)
  (setf (doc-stat-lines stat) (+ (doc-stat-lines stat) (number.of.lines))) ; Considers all buffer length.
  (save-excursion 
    (beginning-of-buffer)
    (let ((pos (point))
	  (element-start-end nil)
	  (stop nil))
      (while (not stop)
	(setq element-start-end (get.element.and.header.limits))
	(cond ((not (car element-start-end))
	       (setq stop t))
	      (t (unless (ignore.element.p element-start-end)
		   (save-excursion (measure-header element-start-end stat))))))
      )))


(defun dir-contents (dir)
  (directory-files dir t))


(defun dir-files (dir)
  (delete-if #'(lambda (c) (file-directory-p c))
	     (dir-contents dir)))


(defun dir-subdirs (dir)
  (delete-if #'(lambda (c) (not (file-directory-p c)))
	     (dir-contents dir)))


;;;-----------------------------------------------------------------------------
;;;MEASURE-DIR
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	
;;;-----------------------------------------------------------------------------
(defun measure-dir (dir)
  (let ((stat (make-doc-stat)))
    (measure-dir-aux (list dir) stat)
    (get-result-buffer)
    (insert (format "\nModule measured: %s \n" dir))
    (print-stat stat)))

;;;-----------------------------------------------------------------------------
;;;PRINT-STAT
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{stat} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun print-stat (stat)
  (find-result-buffer)
  (if %stat-pretty-print%
      (progn
	(insert "--------------------------------------------------------------------------------" 10)
	(insert "Has unknown constructs                     " (format "%s" (doc-stat-has-unknown-constructs stat)) 10)
	(insert "Number of lines                            " (format "%d" (doc-stat-lines stat)) 10)
	;; (insert (format "%d" (doc-stat-files stat)) 10)
	(insert "Total number of elements                   " (format "%d" (doc-stat-total-elems stat)) 10)
	(insert "Number of elements with header             " (format "%d" (doc-stat-elems-with-header stat)) 
		(stat-perc (doc-stat-elems-with-header stat) (doc-stat-total-elems stat)) 10)
	(insert "Number of elements with documentation      " (format "%d" (doc-stat-elems-with-desc stat))
		(stat-perc (doc-stat-elems-with-desc stat) (doc-stat-total-elems stat)) 10)
	(insert "Number of elements with full documentation " (format "%d" (doc-stat-elems-fully-desc stat))
		(stat-perc (doc-stat-elems-fully-desc stat) (doc-stat-total-elems stat)) 10)
	(insert "--------------------------------------------------------------------------------" 10))
      (progn
	(insert (format "%s" (doc-stat-has-unknown-constructs stat)) 9)
	(insert (format "%d" (doc-stat-lines stat)) 9)
	(insert (format "%d" (doc-stat-files stat)) 9)
	(insert (format "%d" (doc-stat-total-elems stat)) 9)
	(insert (format "%d" (doc-stat-elems-with-header stat)) 9)
	(insert (format "%d" (doc-stat-elems-with-desc stat)) 9)
	(insert (format "%d" (doc-stat-elems-fully-desc stat)) 10 10))))

(defun stat-perc (min max)
  (if (= max 0)
      ""
      (format " - %d%s" (/ (* min 100) max) "%")))


(defun measure-dir-aux (dirs stat)
  (while dirs
    (let ((cur-dir (pop dirs)))
      (let ((files (delete-if #'(lambda (f)
				  (or (match-regexps-p (file-name-nondirectory f) %ignore-file-regexps%)
				      (not (match-regexps-p (file-name-nondirectory f) %select-file-regexps%))))
			      (dir-files cur-dir)))
	    (subdirs (delete-if #'(lambda (f)
				    (search "/." f))
				(dir-subdirs cur-dir))))
	(dolist (file files)
	  (measure-file file stat))
	(setq dirs (nconc subdirs dirs))))))


;(set-system-source-file "CREWS-DATA-MODELS-NORMAL-BEHAVIOUR"       
;			(concatenate 'string dir "/models/data-models/system.lisp"))


(defun get-next-string ()
  (next-non-space)
  (let ((start (1+ (point))))
    (forward-sexp)
    (downcase (buffer-substring start (1- (point))))))


(defun get.next.module.data ()
  (let ((pos (ignore-errors (re-search-forward " +(set-system-source-file"))))
    (when pos
      (let ((name (get-next-string))
	    rel-definition-file)
	(re-search-forward "(concatenate 'string dir")
	(setq rel-definition-file (get-next-string))
	(cons name rel-definition-file)))))


(defun get.next.file.data (end)
  (let ((pos (re-search-forward ":module." end t)))
    (when pos
      (forward-sexp)
      (get-next-string))))


;;;(defsystem "CREWS-TASK-SCHEDULER"
;;;
;;;  (:module param      "parameters")

;;;-----------------------------------------------------------------------------
;;;GET.MODULE.FILES
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a <>.
;;;		
;;;		\arg{definition-file} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Removed the 'view-file'
;;;-----------------------------------------------------------------------------
(defun get.module.files (name definition-file)
  (find-file definition-file)
  (beginning-of-buffer)
  (when (or (ignore-errors (re-search-forward (concatenate 'string "(defsystem " name)))
	    (re-search-forward (concatenate 'string "(defsystem \"" name "\"")))
    (let* ((end (save-excursion (end-of-defun)))
	   (base-dir (file-name-directory definition-file))
	   files
	   (file-name (get.next.file.data end)))
    (while file-name
      (push (concatenate 'string base-dir file-name ".lisp") files)
      (setq file-name (get.next.file.data end)))
    (kill-this-buffer)
    files)))


(defun measure.module.aux (files stat)
  (dolist (file files)
    (ignore-errors (measure-file file stat))))


(defun ignore.module.p (name)
  (dolist (mod-name *ignore-modules* nil)
    (when (string-match mod-name name)
      (return t))))


;;;-----------------------------------------------------------------------------
;;;MEASURE.MODULE
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a <>.
;;;		
;;;		\arg{definition-file} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Use of 'get-result-buffer'
;;;-----------------------------------------------------------------------------
(defun measure.module (name definition-file)
  (let ((stat (make-doc-stat)))
    (get-result-buffer)
    (insert (format "Measuring module: %s \n" name))
    (measure.module.aux (get.module.files name definition-file) stat)
    (cons name stat)))


;;;-----------------------------------------------------------------------------
;;;MEASURE.SYSTEM
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{definition-file} is a <>.
;;;		
;;;		\arg{root} is a <>.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Use of 'find-result-buffer'
;;;-----------------------------------------------------------------------------
(defun measure.system (definition-file &optional root)
  (let (stats
	modules)
    (unless root (setq root %crews-dir%))
    (find-file definition-file)
    
    (view-file definition-file)
    (beginning-of-buffer)
    (let ((module-data (get.next.module.data)))
      (while module-data
	(push module-data modules)
	(setq module-data (get.next.module.data))))
    (kill-this-buffer)
    
    (dolist (module-data modules)
      (unless (ignore.module.p (car module-data))
	(push (measure.module (car module-data) (concatenate 'string root (cdr module-data)))
	      stats)))
    
    (dolist (stat stats)
      (let ((name (car stat))
	    (data (cdr stat)))
	(find-result-buffer)
	
	;;("Measured-module" "Has-unknown-constructs" "Lines" "Files" "Elements" "Headers" "Descs" "Fully described")
        (insert (format "\n%s " name))
	(insert (format "%s " (doc-stat-has-unknown-constructs data)))
	(insert (format "%d " (doc-stat-lines data)))
	(insert (format "%d " (doc-stat-files data)))
	(insert (format "%d " (doc-stat-total-elems data)))
	(insert (format "%d " (doc-stat-elems-with-header data)))
	(insert (format "%d " (doc-stat-elems-with-desc data)))
	(insert (format "%d \n" (doc-stat-elems-fully-desc data)))))))


;;;; Example

;;; (measure.system "d:/users/cpla/siscog/code-20000502/crews5-0/system-defs.lisp" "d:/users/cpla/siscog/code-20000502/crews5-0/")
;;; (measure-dir "d:/users/cpla/siscog/code-20000502/crews5-0/win-maps/")


;;; Extracts N random files from DIR, considering a maximum population of MAX files.
(defun extract.random.files (dir n max)
  (when (file-directory-p %output-dir%)
    (sc-delete-directory %output-dir%))
  (sc-make-directory %output-dir%)
  (let ((cur-buf (current-buffer))
	(output (get-buffer "log.txt")))
    (when output
      (kill-buffer output))
    (setq output (find-file-noselect (concatenate 'string %output-dir% "/" (file-name-nondirectory %log-file%))))
    (set-buffer output)
    (erase-buffer)
    (insert (format "\nFile extraction started.\n"))
    (extract.files.aux (list dir) (generate.numbers n max) %output-dir% output)
    (set-buffer output)
    (insert (format "Ended file extraction.\n"))
    (save-buffer output)
    (set-buffer cur-buf)))

;;; Example: (extract-random-files "d:/users/cpla/siscog/code-20000717/crews5-0/" 10 600)
;;; Win-maps has many files!!!

;;; Randomly, generates N natural numbers in the interval [1, MAX].
(defun generate.numbers (n max)
  (let ((numbers nil))
    (random t)
    (dotimes (count n)
      (push (1+ (random max)) numbers))
    numbers))


(defun relevant.filename.p (filename)
  "Verifies if file is acceptable for *correctly described* sampling."
  (let ((stat (make-doc-stat)))
    (measure-file filename stat nil)
    (plusp (doc-stat-elems-fully-desc stat))));%%%CA , Assumes the required scope.

 
(defun extract.files.aux (dirs numbers output-dir log-file)
  (let ((index 0))
    (while dirs
      (let ((cur-dir (pop dirs)))
	(let ((files (delete-if #'(lambda (f)
				  (or (match-regexps-p (file-name-nondirectory f) %ignore-file-regexps%)
				      (not (match-regexps-p (file-name-nondirectory f) %select-file-regexps%))))
			      (dir-files cur-dir)))
	      (subdirs (delete-if #'(lambda (f)
				      (search "/." f))
				  (dir-subdirs cur-dir))))
	  (dolist (file files)
	    (incf index)
	    (when (member index numbers)
	      (cond ((relevant.filename.p file)
		     (sc-copy-file file (concatenate 'string output-dir "/" (file-name-nondirectory file)))
		     (set-buffer log-file)
		     (insert (format "(%d) %s extracted.\n" index file)))
		    (t (set-buffer log-file)
		       (insert (format "(%d) not extracted.\n" index))))))
	  (setq dirs (nconc subdirs dirs)))))))



;;;; Specific measures

(defun select-code (date)
  (update-crews-files-from-date "m:"
				"d:/users/cpla/siscog/code" date))


(defun measure-crews-cp ()
  (measure-dir "d:/users/cpla/siscog/code/crews-cp-vdev/"))

(defun measure-crews-ns ()
  (measure-dir "d:/users/cpla/siscog/code/crews-ns-vdev/"))

(defun measure-crews-nsb ()
  (measure-dir "d:/users/cpla/siscog/code/crews-nsb-vdev/"))

(defun measure-crews-wagn ()
  (measure-dir "d:/users/cpla/siscog/code/crews-wagn-vdev/"))

(defun measure-crews-ml ()
  (measure-dir "d:/users/cpla/siscog/code/crews-ml-vdev/"))

(defun measure-crews-stog ()
  (measure-dir "d:/users/cpla/siscog/code/crews-stog-vdev/"))

(defun measure-crews-vr ()
  (measure-dir "d:/users/cpla/siscog/code/crews-vr-vdev/"))

(defun measure-all (&optional date)
  (reset.unknown.constructors)
  (reset.undocumented.elems)
  (when date 
    (setq %sample-base-date% date))
  (measure-crews)
  (measure-crews-cp)
  (measure-crews-ns)
  (measure-crews-nsb)
  (measure-crews-ml)
  (measure-crews-stog)
  (measure-crews-vr)
  (measure-crews-wagn)
  (find-file %log-file%)
  (print.undocumented.elems)
  )


;;;-----------------------------------------------------------------------------
;;;MEASURE-CREWS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	
;;;-----------------------------------------------------------------------------
(defun measure-crews ()
  (setq *ignore-modules* %ignore-modules-crews%)
  (measure-dir (getenv "CREWS_VERSION_DIR")))

;;;-----------------------------------------------------------------------------
;;;MEASURE-WIN-MAPS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void
;;;
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;	10/09/22	A. Frazao	Removed work-recorder
;;;-----------------------------------------------------------------------------
(defun measure-win-maps ()
  (measure-dir (format "%s/win-maps/allocator" (getenv "CREWS_VERSION_DIR")))
  (measure-dir (format "%s/win-maps/application-controller-db" (getenv "CREWS_VERSION_DIR")))
  (measure-dir (format "%s/win-maps/data-manager" (getenv "CREWS_VERSION_DIR")))
  (measure-dir (format "%s/win-maps/data-manager-st" (getenv "CREWS_VERSION_DIR")))
  (measure-dir (format "%s/win-maps/scheduler" (getenv "CREWS_VERSION_DIR")))
  (measure-dir (format "%s/win-maps/scheduler-st" (getenv "CREWS_VERSION_DIR")))
  (measure-dir (format "%s/win-maps/roster" (getenv "CREWS_VERSION_DIR")))
  (measure-dir (format "%s/win-maps/scheduler-roster" (getenv "CREWS_VERSION_DIR")))
  (measure-dir (format "%s/win-maps/short-term" (getenv "CREWS_VERSION_DIR")))
  (measure-dir (format "%s/win-maps" (getenv "CREWS_VERSION_DIR"))))

;;;-----------------------------------------------------------------------------
;;;MEASURE-SISCOG-UTIL
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun measure-siscog-util ()
  (setq *ignore-modules* %ignore-modules-scg-util%)
  (measure-dir (getenv "SISCOG_VERSION_DIR")))

;;;-----------------------------------------------------------------------------
;;;MEASURE-WINGRAPH
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun measure-wingraph ()
  (measure-dir (format "%s/wingraphics" (getenv "SISCOG_VERSION_DIR"))))

;;;-----------------------------------------------------------------------------
;;;MEASURE-CREWS-X
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun measure-crews-x ()
  (setq *ignore-modules* %ignore-modules-crews%)
  (measure-dir (getenv "CREWS_CP_VERSION_DIR"))
  (measure-dir (getenv "CREWS_NS_VERSION_DIR"))
  (measure-dir (getenv "CREWS_NSB_VERSION_DIR"))
  (measure-dir (getenv "CREWS_WAGN_VERSION_DIR"))
  (measure-dir (getenv "CREWS_ML_VERSION_DIR"))
  (measure-dir (getenv "CREWS_STOG_VERSION_DIR"))
  (measure-dir (getenv "CREWS_VR_VERSION_DIR")))

;;;-----------------------------------------------------------------------------
;;;MEASURE-CREWS-X-MAPS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;History
;;;	Date		Author		Description
;;;	02/06/07	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun measure-crews-x-maps ()
  (measure-dir (format "%s/win-maps" (getenv "CREWS_CP_VERSION_DIR")))
  (measure-dir (format "%s/win-maps" (getenv "CREWS_NS_VERSION_DIR")))
  (measure-dir (format "%s/win-maps" (getenv "CREWS_NSB_VERSION_DIR")))
  (measure-dir (format "%s/win-maps" (getenv "CREWS_WAGN_VERSION_DIR")))
  (measure-dir (format "%s/win-maps" (getenv "CREWS_ML_VERSION_DIR")))
  (measure-dir (format "%s/win-maps" (getenv "CREWS_STOG_VERSION_DIR")))
  (measure-dir (format "%s/win-maps" (getenv "CREWS_VR_VERSION_DIR"))))
  
  

;;;(setq %control-by-file% nil)

;;;(measure-crews)
;;;(measure-siscog-util)
;;;(measure-win-maps)
;;;(measure-wingraph)
;;;(measure-crews-x)
;;;(measure-crews-x-maps)

