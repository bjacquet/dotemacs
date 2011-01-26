;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 1996, SISCOG - Sistemas Cognitivos Lda.
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
;;;	Reference Manual Generator
;;; History
;;;	Date		Author		Description
;;;	96/09/19	A. Vasconcelos	Changed definitions
;;;					  WORD-VARIABLE-REMARKS
;;;					  GET-REMARK-ARGS
;;;					  MAKE-FUNCTION-DOCUMENTATION
;;;					  ALL-FUNCTION-DOCUMENTATION
;;;					  FIND-FUNCTION-DOCUMENTATION
;;;					  WORD-CLASS-REMARKS
;;;					  WORD-CLASS-METHODS
;;;					  CLOS-CLASS-DOCUMENTATION
;;;					  SIKE-CLASS-DOCUMENTATION
;;;	96/09/23	A. Vasconcelos	Changed definitions
;;;					  WORD-CLASS-SYNTAX
;;;					  GET-REMARK-ARGS
;;;					  WORD-METHOD-SYNTAX
;;;					  GET-SIMPLE-ARGS
;;;					Added definitions
;;;					  ARG-KEYWORD-P
;;;	96/10/02	A. Vasconcelos	Changed definitions
;;;					  WORD-METHOD-REMARKS
;;;					  WORD-OBJECT-EXAMPLES
;;;					  WORD-METHOD-SYNTAX
;;;					  GET-REMARK-ARGS
;;;					  CREATE-WORD-DOCUMENT
;;;	96/10/21	J. P. Varandas	Changed definitions
;;;					  MACRO-DOCUMENTATION
;;;					  GET-SPECIALIZER-ARGS
;;;					  GET-SIMPLE-ARGS
;;;					  GET-REMARK-ARGS
;;;	96/10/21	A. Vasconcelos	Changed definitions
;;;					  CREATE-WORD-DOCUMENT
;;;					  WORD-CLASS-INTERFACE
;;;					  WORD-CLASS-METHODS
;;;	96/10/29	A. Vasconcelos	Changed definitions
;;;					  SIKE-CLASS-DOCUMENTATION
;;;	96/12/19	A. Frazao	Changed definitions
;;;					  WORD-METHOD-SYNTAX
;;;					  WORD-OBJECT-DOES
;;;					Added definitions
;;;					  DEFLR-DOCUMENTATION
;;;	96/12/19	A. Vasconcelos	Changed definitions
;;;					  SIKE-CLASS-DOCUMENTATION
;;;	97/01/16	J. P. Varandas	Changed definitions
;;;					  CLOS-CLASS-DOCUMENTATION
;;;	97/03/27	A. Vasconcelos	Changed definitions
;;;					  CREATE-WORD-DOCUMENT
;;;					  WORD-CLASS-METHODS
;;;					  WORD-OBJECT-EXAMPLES
;;;					  ALL-FUNCTION-DOCUMENTATION
;;;					  SIKE-CLASS-DOCUMENTATION
;;;					  WORD-METHOD-RETURN
;;;					  WORD-OBJECT-DATE
;;;					  WORD-OBJECT-SEE-ALSO
;;;					  WORD-VARIABLE-REMARKS
;;;					  WORD-METHOD-REMARKS
;;;					  WORD-CLASS-REMARKS
;;;					  WORD-CLASS-INTERFACE
;;;					  WORD-METHOD-SYNTAX
;;;					  WORD-CLASS-SYNTAX
;;;					  WORD-OBJECT-DOES
;;;	97/03/31	J. P. Varandas	Changed definitions
;;;					  WORD-CLASS-METHODS
;;;					  FIND-FUNCTION-DOCUMENTATION
;;;					  CLOS-CLASS-DOCUMENTATION
;;;					  SIKE-CLASS-DOCUMENTATION
;;;					  MAKE-FUNCTION-DOCUMENTATION
;;;					  ALL-FUNCTION-DOCUMENTATION
;;;					  METHOD-DOCUMENTATION
;;;					  GET-REMARK-ARGS
;;;					Added definitions
;;;					  OTHER-METHODS
;;;					  *LAST-DATE*
;;;-----------------------------------------------------------------------------

(in-package :user)

;;;-----------------------------------------------------------------------------
;;;*LAST-DATE*
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/03/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *last-date* "00/00/00")

;;;-----------------------------------------------------------------------------
;;;CREATE-WORD-DOCUMENT
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/10/02	A. Vasconcelos	
;;;	96/10/21	A. Vasconcelos	Redefined style Doc1
;;;	97/03/27	A. Vasconcelos	Added new styles
;;;-----------------------------------------------------------------------------
(defun create-word-document (stream)
  (format stream "{\\rtf1\\mac\\deff2 {\\fonttbl{\\f0\\fswiss Chicago;}{\\f2\\froman New York;}{\\f3\\fswiss Geneva;}{\\f4\\fmodern Monaco;}{\\f13\\fnil Zapf Dingbats;}{\\f14\\fnil Bookman;}{\\f15\\fnil N Helvetica Narrow;}{\\f16\\fnil Palatino;}{\\f20\\froman Times;}{\\f21\\fswiss Helvetica;}~%")
  (format stream "{\\f22\\fmodern Courier;}{\\f23\\ftech Symbol;}{\\f2498\\fnil AppleIconTwo;}}{\\colortbl\\red0\\green0\\blue0;\\red0\\green0\\blue255;\\red0\\green255\\blue255;\\red0\\green255\\blue0;\\red255\\green0\\blue255;\\red255\\green0\\blue0;\\red255\\green255\\blue0;\\red255\\green255\\blue255;}~%")
  (format stream "{\\stylesheet~%")
  (format stream "{\\s244\\qj\\sl360\\tqc\\tx4320\\tqr\\tx8640 \\f14\\fs20 \\sbasedon0\\snext244 header;}~%")
  (format stream "{\\s252\\qj\\sb240\\sa120\\sl360 \\b\\f14\\fs20 \\sbasedon0\\snext0 heading 4;}~%")
  (format stream "{\\s253\\qj\\sb240\\sa120\\sl360 \\b\\f14\\fs20 \\sbasedon0\\snext0 heading 3;}~%")
  (format stream "{\\s254\\qj\\sb240\\sa120\\sl360\\tx440 \\b\\f14\\fs20 \\sbasedon0\\snext0 heading 2;}~%")
  (format stream "{\\s255\\qj\\sb240\\sa120\\sl360\\tx360 \\b\\f14\\fs20 \\sbasedon0\\snext0 heading 1;}~%")
  (format stream "{\\qj\\sl360 \\f14\\fs20 \\sbasedon222\\snext0 Normal;}~%")
  (format stream "{\\s2\\fi-1440\\li1440\\sl360 \\f14\\fs20 \\sbasedon0\\snext2 Doc;}~%")
  (format stream "{\\s3\\sb240\\sa240\\sl480\\brdrt\\brdrs \\brdrb\\brdrs \\b\\f14 \\sbasedon2\\snext3 Form Header;}~%")
  (format stream "{\\s4\\fi-1440\\li1440\\sl360 \\v\\f14\\fs20 \\sbasedon2\\snext4 Last update;}~%")
  (format stream "{\\s5\\fi-1440\\li1440\\sl360 \\f22\\fs18 \\sbasedon2\\snext20 Examples;}~%")
  
  (format stream "{\\s7\\qj\\li560\\sl360 \\f14\\fs20 \\sbasedon0\\snext7 Description;}")
  (format stream "{\\s8\\qj\\li1120\\sl360 \\f14\\fs20 \\sbasedon7\\snext8 Description 1;}")
  (format stream "{\\s9\\qj\\fi-280\\li1400\\sl360 \\f14\\fs20 \\sbasedon7\\snext9 Points 1;}")  
  (format stream "{\\s11\\fi-320\\li2040\\sl360\\tqr\\tx8280 \\f14\\fs20 \\sbasedon0\\snext11 Doc - description;}")
  (format stream "{\\s12\\fi-1780\\li1780\\sl360\\tx1440 \\f14\\fs20 \\sbasedon2\\snext12 Doc 1;}")
  (format stream "{\\s13\\fi-340\\li1780\\sl360 \\f14\\fs20 \\sbasedon12\\snext23 Doc 2;}")
  (format stream "{\\s14\\fi-340\\li2380\\sl360 \\f14\\fs20 \\sbasedon13\\snext14 Doc 3;}")
  (format stream "{\\s15\\fi-340\\li2700\\sl360\\tqr\\tx8280 \\f14\\fs20 \\sbasedon14\\snext15 Doc 4;}")
  (format stream "{\\s16\\fi-340\\li1780\\sl360 \\f14\\fs20 \\sbasedon2\\snext16 Interface 1;}")  
  (format stream "{\\s17\\fi-340\\li2600\\sl360\\tqr\\tx8280 \\f14\\fs20 \\sbasedon0\\snext17 Interface 2;}")
  (format stream "{\\s18\\li2600\\sl360\\tqr\\tx8280 \\f14\\fs20 \\sbasedon2\\snext18 Interface 2 - Description;}")
  (format stream "{\\s19\\fi-340\\li3240\\sl360\\tqr\\tx8280 \\f14\\fs20 \\sbasedon0\\snext19 Interface 3;}")
  (format stream "{\\s20\\li1440 \\f22\\fs18 \\sbasedon5\\snext20 Examples 1;}")
  
  (format stream "}~%")
  (format stream "{\\info{\\title CREWS Reference Manual}{\\subject CREWS Reference Manual}{\\author SISCOG}~%")
  (format stream "}~%"))

(defun word-introduction (stream)
  (format stream "\\widowctrl\\ftnbj \\sectd \\sbknone\\linemod0\\linex0\\cols1\\endnhere ~%"))

(defun word-object-header (stream string)
  (format stream "\\pard\\plain \\s3\\sb240\\sa240\\sl480\\brdrt\\brdrs \\brdrb\\brdrs \\b\\f14 {\\v {\\xe\\pard\\plain \\s3\\sb240\\sa240\\sl480\\brdrt\\brdrs \\brdrb\\brdrs \\b\\f14 ~A}}\\par ~%" string))

;;;-----------------------------------------------------------------------------
;;;WORD-OBJECT-DOES
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/12/19	A. Frazao	If args are not supplied, just write
;;;					the string.
;;;	97/03/27	A. Vasconcelos	Does: in italic.
;;;-----------------------------------------------------------------------------
(defun word-object-does (stream string &rest args)
  (format stream "\\pard\\plain \\s2\\fi-1440\\li1440\\sl360 \\f14\\fs20 {\\i Does:}\\tab ")
  (when string
    (if args
	(apply #'format stream string args)
	(write-string string stream)))
  (format stream "\\par ~%"))

;;;-----------------------------------------------------------------------------
;;;WORD-CLASS-SYNTAX
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/23	A. Vasconcelos	Chnaged tab stop
;;;	97/03/27	A. Vasconcelos	Syntax: in italic.
;;;-----------------------------------------------------------------------------
(defun word-class-syntax (stream class type &optional first rest)
  (format stream "\\pard \\s2\\fi-1440\\li1440\\sl360\\tqr\\tx8640 {\\i Syntax:}\\tab {\\b ~A} " class)
  (if first
      (if rest
	  (progn
	    (format stream "(~A\\tab [{\\i ~A}]\\par ~%" first type)
	    (dolist (pair (butlast rest))
	      (format stream "\\tab ~A\\par ~%" pair))
	    (format stream "\\tab ~A)\\par ~%" (util::rac rest)))
	  (format stream "(~A)\\tab [{\\i ~A}]\\par ~%" first type))
      (format stream "()\\tab [{\\i ~A}]\\par ~%" type)))

;;;-----------------------------------------------------------------------------
;;;WORD-METHOD-SYNTAX
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/23	A. Vasconcelos	Prints the keywords correctely
;;;					Chnaged tab stop
;;;	96/10/02	A. Vasconcelos	Keyword arguments are writen in Courier
;;;					and are preceeded by :
;;;	96/12/19	A. Frazao	Correct for sub-list arguments (macros)
;;;	97/03/27	A. Vasconcelos	Syntax: in italic: tab at tx8640
;;;-----------------------------------------------------------------------------
(defun word-method-syntax (stream class type &optional first rest)
  (format stream "\\pard \\s2\\fi-1440\\li1440\\sl360\\tqr\\tx8640 {\\i Syntax:}\\tab {\\b ~A} " class)
  (labels ((write-syntax (first rest)
	     (let ((key-arg nil))
	       (when first
		 (cond ((consp first)
			(format stream "{\\f22 (}" first)
			(write-syntax (car first) (cdr first))
			(format stream "{\\f22 )}" first))
		       ((arg-keyword-p first)
			(if (string-equal first "&KEY")
			    (setf key-arg t)
			    (setf key-arg nil))
			(format stream "{\\f22 ~A}" first))
		       (t (format stream "{\\i ~A}" first))))
	       (when rest
		 (dolist (el rest)
		   (if (arg-keyword-p el)
		       (progn
			 (format stream " {\\f22 ~A}" el)
			 (if (string-equal el "&KEY")
			     (setf key-arg t)
			     (setf key-arg nil)))
		       (if key-arg
			   (format stream " {\\f22 :~A}" el)
			   (format stream " {\\i ~A}" el))))))))
    (write-syntax first rest)
    (format stream "\\tab [{\\i ~A}]\\par ~%" type)))



    
;;;-----------------------------------------------------------------------------
;;;WORD-CLASS-INTERFACE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/10/21	A. Vasconcelos	Used Doc1 style
;;;					Disregards CONTINUE
;;;	97/03/27	A. Vasconcelos	Interface: in italic
;;;-----------------------------------------------------------------------------
(defun word-class-interface (stream continue)
  (declare (ignore continue))
  (format stream "\\pard \\s2\\fi-1440\\li1440\\sl360 {\\i Interface:}\\par ~%"))


;;;-----------------------------------------------------------------------------
;;;WORD-CLASS-METHODS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/19	A. Vasconcelos	STRING is writen in italic+bold
;;;	96/10/21	A. Vasconcelos	Use Doc1 style.
;;;					Each method is printed in a paragraph
;;;	97/03/27	A. Vasconcelos	Changed s6 by s16 an tab by line
;;;	97/03/31	J. P. Varandas	Uses the continue arg
;;;-----------------------------------------------------------------------------
(defun word-class-methods (stream string methods continue)
  (format stream "\\pard\\plain \\s16\\fi-340\\li1780\\sl360 \\f14\\fs20 {\\i\\b ~A} \\par ~%" string)
  (dolist (method methods)
    (format stream "\\s16 {\\i ~A}~A\\par ~%" method (if continue "\\line" ""))))

;;;-----------------------------------------------------------------------------
;;;WORD-CLASS-REMARKS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/19	A. Vasconcelos	Changed text of Precedence list
;;;					Precedence list classes are in bold
;;;	97/03/27	A. Vasconcelos	Remarks: a italico.
;;;					Replaced Remarks style
;;;-----------------------------------------------------------------------------
(defun word-class-remarks (stream precedence)
  (if precedence
      (progn
	(format stream "\\pard \\s2\\fi-1440\\li1440\\sl360 {\\i Remarks:}~%")
	(format stream "\\tab Precedence list is [{\\b ~A~{ ~A~}}].\\par ~%" (car precedence) (cdr precedence)))
      (format stream "\\pard \\s12\\fi-1780\\li1780\\sl360\\tx1440 {\\i Remarks:}\\tab \\par ~%")))

;;;-----------------------------------------------------------------------------
;;;WORD-METHOD-REMARKS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/10/02	A. Vasconcelos	Keyword args are writen in Courier
;;;	97/03/27	A. Vasconcelos	Remarks: em italico
;;;					\\s2 -> \\s12
;;;-----------------------------------------------------------------------------
(defun word-method-remarks (stream args)
  (flet ((arg-format (arg)
		     (if (char= (elt arg 0) #\:)
			 "f22"
			 "i")))
    (format stream "\\pard \\s12\\fi-1780\\li1780\\sl360\\tx1440 {\\i Remarks:}\\tab ")
    (if args
	(progn
	  (format stream "{\\~a ~A} ~A\\par ~%" (arg-format (caar args)) (caar args) (cadar args))
	  (format stream "\\s13\\fi-340\\li1780\\sl360 ")
	  (dolist (arg (cdr args))
	    (format stream "{\\~a ~A} ~A\\par ~%" (arg-format (car arg)) (car arg) (cadr arg))))
	(format stream "\\par ~%"))))

;;;-----------------------------------------------------------------------------
;;;WORD-VARIABLE-REMARKS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/19	A. Vasconcelos	Checks if value is nil
;;;	97/03/27	A. Vasconcelos	Remarks: in italic.
;;;-----------------------------------------------------------------------------
(defun word-variable-remarks (stream value)
  (format stream "\\pard \\s2\\fi-1440\\li1440\\sl360 {\\i Remarks:}\\tab The default value is ~A \\par ~%"
	  (if (string-equal value "NIL")
	      "{\\f22 nil}"
	      value)))

;;;-----------------------------------------------------------------------------
;;;WORD-METHOD-RETURN
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/03/27	A. Vasconcelos	Returns: in italic.
;;;-----------------------------------------------------------------------------
(defun word-method-return (stream &optional value)
  (format stream "\\pard \\s2\\fi-1440\\li1440\\sl360 {\\i Returns:}\\tab ")
  (when value
    (format stream "~A " value))
  (format stream "\\par ~%"))

;;;-----------------------------------------------------------------------------
;;;WORD-OBJECT-EXAMPLES
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/10/02	A. Vasconcelos	Examples -> Examples:
;;;					Changed "Examples:" font
;;;	97/03/27	A. Vasconcelos	Examples: in italic.
;;;-----------------------------------------------------------------------------
(defun word-object-examples (stream)
  (format stream "\\pard\\plain \\s5\\fi-1440\\li1440\\sl360 \\f22\\fs18 {\\i\\f14\\fs20 Examples:}\\tab \\par ~%"))

;;;-----------------------------------------------------------------------------
;;;WORD-OBJECT-SEE-ALSO
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/03/27	A. Vasconcelos	See also: in italic.
;;;-----------------------------------------------------------------------------
(defun word-object-see-also (stream alsos)
  (if alsos
      (progn
	(format stream "\\pard\\plain \\s2\\fi-1440\\li1440\\sl360 \\f14\\fs20 {\\i See also:}\\tab")
	(format stream "{\\b ~A}~{, {\\b ~A}~}" (car alsos) (cdr alsos))
	(format stream "\\par ~%"))
      (format stream "\\pard\\plain \\s2\\fi-1440\\li1440\\sl360 \\f14\\fs20 {\\i See also:}\\tab \\par ~%")))

;;;-----------------------------------------------------------------------------
;;;WORD-OBJECT-DATE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/03/27	A. Vasconcelos	Last update: in italic.
;;;-----------------------------------------------------------------------------
(defun word-object-date (stream date)
  (format stream "\\pard\\plain \\s4\\fi-1440\\li1440\\sl360 \\v\\f14\\fs20 {\\i Last update:}\\tab ~A\\par ~%" date))

(defun end-word-document (stream)
  (format stream "\\pard\\plain \\sl360 \\f14\\fs20 \\par ~%}"))

;;;-----------------------------------------------------------------------------
;;;SIKE-CLASS-DOCUMENTATION
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/19	A. Vasconcelos	Replaced "Direct accessors:" by "Accessors:"
;;;					Commented Inherit Methods.
;;;					Replaced "Direct methods:" by "Methods:"
;;;					Replaced "Direct modifiers:" by "Modifiers:"
;;;					"Modifiers:" comes before "Methods:"
;;;	96/10/29	A. Vasconcelos	Replaced "Modifiers:" by "Writers:"
;;;	96/12/19	A. Vasconcelos	References to the class and the knowledge
;;;					base are writen in bold instead of italic
;;;	97/03/27	A. Vasconcelos	SiKE -> SIKE
;;;	97/03/31	J. P. Varandas	The see also is a list of its sub-classes
;;;-----------------------------------------------------------------------------
(defun sike-class-documentation (class doc date &optional (stream t))
  (labels ((get-parent (sike-parent)
		       (format nil "({\\i ~a ~a})" (string-downcase (sike::name sike-parent)) (string-downcase (sike::name (sike::kb sike-parent)))))
	   (get.modifiers (methods slots)
			  ;; Retorna os metodos writers
			  (util::filter #'(lambda (method) (and (consp method)
								(not (member-if #'(lambda (slot) (eq (util::rac method) (sike::slotname slot)))
										slots))))
					methods))
	   (get.methods (methods slots)
			(util::filter #'(lambda (method) (and (atom method)
							      (not (member-if #'(lambda (slot) (eq method (sike::slotname slot)))
									      slots))))
				      methods))
	   (sort-slots (slots)
		       (sort slots #'string< :key #'sike::slotname))
	   (sort-methods (methods)
			 (sort methods #'(lambda (m1 m2) (string< (if (consp m1) (util::rac m1) m1) (if (consp m2) (util::rac m2) m2)))))
	   )
    (when (string< *last-date* date)
      (when (symbolp class)
	(setq class (find-class class)))
      (let ((class-name (string-downcase (clos::class-name class)))
	    (sike-class (sike::find.class (clos::class-name class))))
	(WORD-object-header stream class-name)
	(let ((parents  (mapcar #'get-parent (sike::parents sike-class)))
	      (direct-slots (sort-slots (util::filter #'(lambda (slot) (eq (sike::owner slot) sike-class)) (sike::slots sike-class))))
	      (inherit-slots (sort-slots (util::filter #'(lambda (slot) (not (eq (sike::owner slot) sike-class))) (sike::slots sike-class))))
	      (methods (delete-duplicates (mapcar #'(lambda (method) (slot-value (slot-value method 'clos::generic-function) 'clos::name))
						  (slot-value class 'clos::direct-methods))))
	      (precedence (mapcar #'clos::class-name (slot-value class 'clos::precedence-list)))
	      (sub-classes (sort (mapcar #'(lambda (c1) (string-downcase (symbol-name (slot-value c1 'clos::name))))
					 (slot-value class 'clos::direct-subclasses)) #'string<)))
	  (setf precedence (subseq precedence 0 (position 'SIKE::*OBJ* precedence)))
	  (let ((modifiers (sort-methods (get.modifiers methods direct-slots)))
		(other-methods (sort-methods (get.methods methods direct-slots))))
	    (WORD-object-does stream "Defines the ~ASIKE class {\\b ~A}\\line ~A"
			      (if (sike::uninterned? sike-class) "uninterned " "") class-name doc)
	    ;; Prints the SYNTAX
	    (WORD-class-syntax stream class-name "SIKE Class" (car parents) (cdr parents))
	    (WORD-class-interface stream (or direct-slots inherit-slots other-methods modifiers))
	    ;; Prints the DIRECT ACCESSORS
	    (when direct-slots
	      (WORD-class-methods stream "Accessors:"
				  (mapcar #'(lambda (slot)
					      (let ((relation (sike::relation slot)))
						(if relation
						    (format nil "~A}\\tab Is related with accessor {\\i ~A} of class {\\b ~A} from base {\\b ~A"
							    (string-downcase (sike::slotname slot)) (string-downcase (car relation))
							    (string-downcase (cadr relation)) (string-downcase (caddr relation)))
						    (string-downcase (sike::slotname slot)))))
					  direct-slots)
				  t)) ;;;inherit-slots
	    ;; Prints the INHERIT ACCESSORS
;;;	  (when inherit-slots
;;;	    (WORD-class-methods stream "Inherit accessors:"
;;;				(mapcar #'(lambda (slot) (format nil "~A} ({\\i ~A}){\\i " (string-downcase (sike::slotname slot))
;;;								 (string-downcase (sike::name (sike::owner slot)))))
;;;					inherit-slots)
;;;				t))
	    ;; Prints the DIRECT MODIFIERS
	    (when modifiers
	      (WORD-class-methods stream "Writers:" (mapcar #'(lambda (method) (string-downcase (util::rac method))) modifiers) t))
	    ;; Prints the DIRECT METHODS
	    (when other-methods
	      (WORD-class-methods stream "Methods:" (mapcar #'string-downcase other-methods) nil))
	    ;; Prints the REMARKS
	    (WORD-class-remarks stream (mapcar #'string-downcase precedence))
	    (WORD-object-see-also stream sub-classes)
	    (WORD-object-date stream date)
	    ))))))

;;;-----------------------------------------------------------------------------
;;;CLOS-CLASS-DOCUMENTATION
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/19	A. Vasconcelos	Replaced "Direct accessors:" by "Accessors:"
;;;					Replaced "Direct methods:" by "Methods:"
;;;					Replaced "Direct modifiers:" by "Modifiers:"
;;;					"Modifiers:" comes before "Methods:"
;;;	97/01/16	J. P. Varandas	Os accessores sao todos os methodos que sao readers e writers ao mesmo tempo
;;;					As diferencas sao entao os readers e writers
;;;	97/03/31	J. P. Varandas	The see also is a list of its sub-classes
;;;-----------------------------------------------------------------------------
(defun clos-class-documentation (class doc date &optional (stream t))
  (labels ((clos.name (thing)
		      (let ((name (slot-value (slot-value thing 'clos::generic-function) 'clos::name)))
			(if (consp name) (util::rac name) name)))
	   (sort-methods (methods)
			 (sort (delete-duplicates methods) #'string<))
	   )
    (when (string< *last-date* date)
      (when (symbolp class)
	(setq class (find-class class)))
      (let ((class-name (string-downcase (clos::class-name class))))
	(WORD-object-header stream class-name)
	(let ((parents (mapcar #'(lambda (class) (format nil "{\\i ~A}" (string-downcase (clos::class-name class))))
			       (remove 'STANDARD-OBJECT (slot-value class 'clos::direct-superclasses) :key #'clos::class-name)))
	      (methods (slot-value class 'clos::direct-methods))
	      (precedence (mapcar #'clos::class-name (slot-value class 'clos::precedence-list))))
	  (setf precedence (subseq precedence 0 (position 'STANDARD-OBJECT precedence)))
	  (let ((readers (util::mapcar-f #'clos.name (util::filter #'(lambda (method) (eq (type-of method) 'clos::STANDARD-READER-METHOD)) methods)))
		(writers (util::mapcar-f #'clos.name (util::filter #'(lambda (method) (eq (type-of method) 'clos::STANDARD-WRITER-METHOD)) methods)))
		(methods (util::mapcar-f #'clos.name (util::filter #'(lambda (method) (eq (type-of method) 'clos::STANDARD-METHOD)) methods)))
		(clos-doc (getf (slot-value class 'clos::plist) :documentation)))
	    (let ((accessors (sort-methods (intersection readers writers)))
		  (sub-classes (sort (mapcar #'(lambda (c1) (string-downcase (symbol-name (slot-value c1 'clos::name))))
					     (slot-value class 'clos::direct-subclasses)) #'string<)))
	      (setf readers (sort-methods (set-difference readers accessors)))
	      (setf writers (sort-methods (set-difference writers accessors)))
	      (setf methods (sort-methods methods))
	      (if clos-doc
		  (WORD-object-does stream "Defines the class {\\b ~A}\\line ~A\\line ~A" class-name doc clos-doc)
		  (WORD-object-does stream "Defines the class {\\b ~A}\\line ~A" class-name doc))
	      ;; Prints the SYNTAX
	      (WORD-class-syntax stream class-name "CLOS Class" (car parents) (cdr parents))
	      (WORD-class-interface stream (or accessors readers writers methods))
	      ;; Prints the ACESSORS
	      (when accessors
		(WORD-class-methods stream "Accessors:" (mapcar #'string-downcase accessors) t))
	      ;; Prints the READERS
	      (when readers
		(WORD-class-methods stream "Readers:" (mapcar #'string-downcase readers) t))
	      ;; Prints the WRITERS
	      (when writers
		(WORD-class-methods stream "Writers:" (mapcar #'string-downcase writers) t))
	      ;; Prints the METHODS
	      (when methods
		(WORD-class-methods stream "Methods:" (mapcar #'string-downcase methods) nil))
	      ;; Prints the REMARKS
	      (WORD-class-remarks stream (mapcar #'string-downcase precedence))
	      (WORD-object-see-also stream sub-classes)
	      (WORD-object-date stream date)
	      )))))))


(defun print-arg-name (arg)
  (string-downcase (princ-to-string arg)))


;;;-----------------------------------------------------------------------------
;;;ARG-KEYWORD-P
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/23	A. Vasconcelos	Created
;;;-----------------------------------------------------------------------------
(defun arg-keyword-p (arg)
  (member arg '("&KEY" "&OPTIONAL" "&REST" "&BODY" "&ALLOW-OTHER-KEYS" ) :test #'string-equal))


(defun get-arg-name (arg)
  (if (consp arg)
      (print-arg-name (car arg))
      (print-arg-name arg)))

;;;-----------------------------------------------------------------------------
;;;GET-REMARK-ARGS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/19	A. Vasconcelos	NIL replaced by {\\f22 nil}
;;;					References to other forms are in bold.
;;;	96/09/23	A. Vasconcelos	&REST arguments where not printed.
;;;	96/10/02	A. Vasconcelos	keyword -> keyword argument
;;;					Keyword args are preapended with ":"
;;;	96/10/21	J. P. Varandas	Deal with when args is an atom 
;;;	97/03/31	J. P. Varandas	
;;;-----------------------------------------------------------------------------
(defun get-remark-args (args &optional key)
  (when args
    (let ((arg (if (atom args) args (car args))))
      (cond ((and (atom arg)
		  (member arg '("&KEY" "&OPTIONAL" "&REST" "&BODY" "BODY") :test #'string-equal))
	     (get-remark-args (unless (atom args) (cdr args)) arg))
	    ((string-equal key "&KEY")
	     (if (consp arg)
		 (cons (list (concatenate 'string ":" (print-arg-name (car arg)))
			     (format nil "is a keyword argument with the default value ~S" (cadr arg)))
		       (get-remark-args (unless (atom args) (cdr args)) key))
		 (cons (list (concatenate 'string ":" (print-arg-name arg))
			     "is a keyword argument with the default value {\\f22 nil}")
		       (get-remark-args (unless (atom args) (cdr args)) key))))
	    ((string-equal key "&OPTIONAL")
	     (if (consp arg)
		 (cons (list (print-arg-name (car arg))
			     (format nil "is an optional argument with the default value ~S" (cadr arg)))
		       (get-remark-args (unless (atom args) (cdr args)) key))
		 (cons (list (print-arg-name arg)
			     (format nil "is an optional argument with the default value {\\f22 nil}"))
		       (get-remark-args (unless (atom args) (cdr args)) key))))
	    ((string-equal key "&REST")
	     (get-remark-args args nil)
	     )
	    ((consp arg)
	     (cons (list (print-arg-name (car arg))
			 (format nil "is a {\\b ~A} object" (print-arg-name (cadr arg))))
		   (get-remark-args (unless (atom args) (cdr args)) key)))
	    (t (cons (list (print-arg-name arg) "")
		     (get-remark-args (unless (atom args) (cdr args)) key)))))))


;;;-----------------------------------------------------------------------------
;;;GET-SIMPLE-ARGS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/23	A. Vasconcelos	
;;;	96/10/21	J. P. Varandas	Deal with when args is an atom 
;;;-----------------------------------------------------------------------------
(defun get-simple-args (args)
  (when args
    (if (atom args)
	(list (print-arg-name args))
	(let ((arg (car args)))
	  (cond ((consp arg)
		 (cons (print-arg-name (car arg)) (get-simple-args (cdr args))))
		((member arg '("&KEY" "&OPTIONAL" "&REST") :test #'string-equal)
		 (cond ((null (cdr args)) nil)
		       (t (cons (print-arg-name arg) (get-simple-args (cdr args))))))
		(t (cons (print-arg-name arg) (get-simple-args (cdr args)))))))))

;;;-----------------------------------------------------------------------------
;;;GET-SPECIALIZER-ARGS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/10/21	J. P. Varandas	Deal with when args is an atom 
;;;-----------------------------------------------------------------------------
(defun get-specializer-args (args)
  (when args
    (let ((arg (car args)))
      (cond ((consp arg)
	     (cons (cadr arg) (get-specializer-args (unless (atom args) (cdr args)))))
	    ((member arg '("&KEY" "&OPTIONAL" "&REST") :test #'string-equal)
	     nil)
	    (t (cons t (get-specializer-args (unless (atom args) (cdr args)))))))))

;;;-----------------------------------------------------------------------------
;;;METHOD-DOCUMENTATION
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/03/31	J. P. Varandas	See also with 'other-methods'
;;;-----------------------------------------------------------------------------
(defun method-documentation (name args doc date &optional (stream t))
  (when (string< *last-date* date)
    (let ((lambda-list (get-simple-args args))
	  (method-name (string-downcase (format nil "~A ~A" name (get-specializer-args args)))))
      (WORD-object-header stream method-name)
      (WORD-object-does stream doc)
      (WORD-method-syntax stream name "Method" (car lambda-list) (cdr lambda-list))
      (WORD-method-remarks stream (get-remark-args args))
      (WORD-method-return stream)
      (WORD-object-examples stream)
      (WORD-object-see-also stream (other-methods name))
      (WORD-object-date stream date))))

(defun function-documentation (function-name args doc date &optional (stream t))
  (when (string< *last-date* date)
    (let ((lambda-list (get-simple-args args)))
      (WORD-object-header stream function-name)
      (WORD-object-does stream doc)
      (WORD-method-syntax stream function-name "Function" (car lambda-list) (cdr lambda-list))
      (WORD-method-remarks stream (get-remark-args args))
      (WORD-method-return stream)
      (WORD-object-examples stream)
      (WORD-object-see-also stream nil)
      (WORD-object-date stream date))))


;;;-----------------------------------------------------------------------------
;;;DEFLR-DOCUMENTATION
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/12/19	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun deflr-documentation (function-name args doc date &optional (stream t))
  (when (string< *last-date* date)
    (setf args (cons (car args)
		     (cons "&key" (cdr args))))
    (let ((lambda-list (get-simple-args args))
	  (remark-args (get-remark-args args)))    (WORD-object-header stream function-name)
	  (WORD-object-does stream doc)
	  (WORD-method-syntax stream function-name "Function" (car lambda-list) (cdr lambda-list))
	  (WORD-method-remarks stream remark-args)
	  (WORD-method-return stream)
	  (WORD-object-examples stream)
	  (WORD-object-see-also stream nil)
	  (WORD-object-date stream date))))

;;;-----------------------------------------------------------------------------
;;;MACRO-DOCUMENTATION
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/10/21	J. P. Varandas	Makes the append of the 'get-simple-args' when the (car args) is a cons
;;;-----------------------------------------------------------------------------
(defun macro-documentation (function-name args doc date &optional (stream t))
  (let ((lambda-list (cond ((null args) nil)
			   ((consp (car args)) (append (get-simple-args (car args)) (get-simple-args (cdr args))))
			   (t (get-simple-args args))))
	(remarks (cond ((null args) nil)
		       ((consp (car args)) (append (get-remark-args (car args)) (get-remark-args (cdr args))))
		       (t (get-remark-args args)))))
    (when (string< *last-date* date)
      (WORD-object-header stream function-name)
      (WORD-object-does stream doc)
      (WORD-method-syntax stream function-name "Macro" (car lambda-list) (cdr lambda-list))
      (WORD-method-remarks stream remarks)
      (WORD-method-return stream)
      (WORD-object-examples stream)
      (WORD-object-see-also stream nil)
      (WORD-object-date stream date))))

(defun variable-documentation (type variable-name value doc date &optional (stream t))
  (when (string< *last-date* date)
    (WORD-object-header stream variable-name)
    (WORD-object-does stream doc)
    ;; Prints the SYNTAX
    (WORD-method-syntax stream variable-name type)
    ;; Prints the REMARKS
    (WORD-variable-remarks stream value)
    (WORD-object-see-also stream nil)
    (WORD-object-date stream date)))

(defun function-get-spec (l-list)
  (let ((rest nil)
	(spec-list nil))
    (do* ((lambda l-list (cdr lambda))
	  (lambda* (get-arg-name lambda) (get-arg-name lambda)))
	((null lambda) (nreverse spec-list))
      (if (char= (elt lambda* 0) #\&)
	  (progn
	    (setf rest (string-equal lambda* "&rest"))
	    (push (format nil "{\\f22 ~A}" lambda*) spec-list))
	  (if rest
	      (push (format nil "\\{{\\i ~A}\\}*" lambda*) spec-list)
	      (push (format nil "{\\i ~A}" lambda*) spec-list))))))

;;;-----------------------------------------------------------------------------
;;;FIND-FUNCTION-DOCUMENTATION
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/19	A. Vasconcelos	Changed return and does text
;;;	97/03/31	J. P. Varandas	Changed the return
;;;-----------------------------------------------------------------------------
(defun find-function-documentation (type function-name args doc class date &optional (stream t))
  (when (string< *last-date* date)
    (let ((lambda-list (function-get-spec args))
	  (class-name (string-downcase class)))
      (WORD-object-header stream function-name)
      (WORD-object-does stream "Tries to find an object the class {\\b ~A} with the given name.\\line Returns on object of the class or {\\f22 nil} otherwise.\\line ~A" class-name doc)
      ;; Prints the SYNTAX
      (WORD-method-syntax stream function-name type (car lambda-list) (cdr lambda-list))
      ;; Prints the REMARKS
      (WORD-method-remarks stream (list (list "name" "Is a symbol or a string that represents the name of the object.")))
      (WORD-method-return stream (format nil "A {\\b ~A} object or {\\f22 nil}." class-name))
      (WORD-object-examples stream)
      (WORD-object-see-also stream (list "find.obj"))
      (WORD-object-date stream date))))

;;;-----------------------------------------------------------------------------
;;;ALL-FUNCTION-DOCUMENTATION
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/19	A. Vasconcelos	Changed does and return text.
;;;	97/03/27	A. Vasconcelos	SiKE -> SIKE
;;;	97/03/31	J. P. Varandas	New 'WORD-method-return' string
;;;-----------------------------------------------------------------------------
(defun all-function-documentation (type function-name args doc class date &optional (stream t))
  (declare (ignore args))
  (when (string< *last-date* date)
    (let ((class-name (string-downcase class)))
      (WORD-object-header stream function-name)
      (WORD-object-does stream "Returns all instances of the SIKE class {\\b ~A}.\\line ~A" class-name doc)
      ;; Prints the SYNTAX
      (WORD-method-syntax stream function-name type)
      ;; Prints the REMARKS
      (WORD-method-return stream (format nil "A list with {\\b ~A} objects." class-name))
      (WORD-object-see-also stream (list "childs"))
      (WORD-object-date stream date))))

;;;-----------------------------------------------------------------------------
;;;MAKE-FUNCTION-DOCUMENTATION
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/19	A. Vasconcelos	Changed does and return text.
;;;	97/03/31	J. P. Varandas	Changed slots
;;;-----------------------------------------------------------------------------
(defun make-function-documentation (type function-name args doc class date &optional (stream t))
  (labels ((get-spec2 (slots)
		      (cons (list "name" "Is a symbol or a string that represents the name of the object.")
			    (list
			     (if (cdr slots)
				 (list "args" (format nil "Is a list of pairs {\\i key-value} that specifies the initial value to assign to the class attribute refered by the key.  The keys can have the following values: ~{{\\f22\\fs20 :~A}, ~}~{and {\\f22\\fs20 :~A}~}." (butlast slots) (last slots)))
				 (list "args" (format nil "Is a list of pairs {\\i key-value} that specifies the initial value to assign to the class attribute refered by the key.  The keys can have the following value: {\\f22\\fs20 :~A}." (car slots)))))
			    )))
    
    (when (string< *last-date* date)
      (let* ((lambda-list (function-get-spec args))
	     (class-name (string-downcase class))
	     (direct-slots (sort (mapcar #'(lambda (slot) (string-downcase (slot-value slot 'clos::name)))
					 (slot-value (find-class class) 'clos::slots)) #'string<)))
	(WORD-object-header stream function-name)
	(WORD-object-does stream "Constructor function of the class {\\b ~A}.\\line ~A" class-name doc)
	;; Prints the SYNTAX
	(WORD-method-syntax stream function-name type (car lambda-list) (cdr lambda-list))
	;; Prints the REMARKS
	(WORD-method-remarks stream (get-spec2 direct-slots))
	(WORD-method-return stream (format nil "A {\\b ~A} object" class-name))
	(WORD-object-examples stream)
	(WORD-object-see-also stream (list "make.inst"))
	(WORD-object-date stream date)))))

;;;-----------------------------------------------------------------------------
;;;OTHER-METHODS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/03/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun other-methods (symbol)
  (sort (mapcar #'(lambda (s) (string-downcase (symbol-name (car (util::rac s)))))
		(delete-if-not #'listp (apply #'append (mapcar #'(lambda (sym) (mapcar #'car (lucid::get-source-file sym nil t)))
							       (lucid::find-all-symbols (intern (string-upcase symbol)))))))
	#'string<))

