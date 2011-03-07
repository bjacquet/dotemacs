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
;;;                      Campo Grande 30, 6 B
;;;                           1700 LISBOA
;;;                             PORTUGAL
;;;
;;;-----------------------------------------------------------------------------
;;; Description
;;;	Module that adds the menu "Doc" to ease the task of 
;;;     internal documentation (see quality document "Internal
;;;	Documentation Rules").
;;;     The options are divided in two parts, considering the following goals:
;;;	1- Help on the insertion of meaningful attribute keywords, depending upon the
;;;        language definition being documented;
;;;     2- Help the programmer dealing with the bigger headers that document
;;;        the code.
;;;     
;;;     Part 1
;;;
;;;		"Mandatory attributes" option
;;;			Inserts the mandatory attribute keywords, and
;;;			respective default values (if exist).
;;;			(Active when cursor is over an header) 
;;;
;;;		"Optional attributes..." option
;;;			Considering the constructor that follows the cursor, presents
;;;			a popup menu with its optional attribute keywords.
;;;			(Active when cursor is over an header)
;;;
;;;
;;;     Part 2
;;;
;;;		"Outline" submenu
;;;			Outlining means turning text representation more useful
;;;			(e.g., some parts get hidden). 
;;;
;;;			"Outline header" option
;;;				Outlines only current header.
;;;
;;;		   	"Turn on outline" option
;;;			        Outline state is turned on, meaning that outlined parts with
;;;				invisible property get hidden.
;;;
;;;		   	"Turn off outline" option
;;;				Outline state is turned off, meaning that outlined parts with
;;;				invisible property are also shown.
;;;
;;;		   	"Switch visibility" option
;;;				Changes the visibility state of current outlined section.
;;;				It can be visible or invisible.
;;;		
;;;		   	"Outline buffer" option
;;;				Outlines the whole current buffer (takes some time :-().
;;;
;;;
;;;	Notes:
;;;	   Some of the functions used are defined by "sc-mod.el".
;;; History
;;;	Date		Author		Description
;;;	99/11/11	cpla	        Created.
;;;	00/01/31	J. P. Varandas	Added definitions
;;;					  INSERT-CLASS-ATTRIBUTES
;;;					  GET-CLASS-ATTRIBUTES
;;;					  INSERT-ARGUMENTS
;;;					  GET-ARGUMENTS
;;;					  GET-NEXT-ARGUMENTS
;;;					Changed definitions
;;;					  INSERT.ATTRIBUTE
;;;	00/02/08	J. P. Varandas	Changed definitions
;;;					  GET.LISP.DEFINITION.CONSTRUCTOR
;;;	00/02/08	J. P. Varandas	Changed definitions
;;;					  INSERT.ATTRIBUTE
;;;					  INSERT-ARGUMENTS
;;;					Added definitions
;;;					  INSERT-RETURN-TYPES
;;;	00/02/18	J. P. Varandas	Changed definitions
;;;					  INSERT-RETURN-TYPES
;;;	00/03/21	cpla    	Changed definitions
;;;					  INSERT-ARGUMENTS
;;;					  INSERT.ATTRIBUTE
;;;	00/04/14	cpla    	Changed definitions
;;;					  INSERT-RETURN-TYPES
;;;					  INSERT-CLASS-ATTRIBUTES
;;;					  INSERT-ARGUMENTS
;;;	77/77/77	Dario 		Added doc utilities contributed by
;;;					A. Frazao.
;;;	01/01/05	Dario		Changed definitions
;;;					  INSERT-RETURN-TYPES
;;;					  INSERT.ATTRIBUTE
;;;	01/07/04	J. P. Varandas	Changed definitions
;;;					  INSERT-DOC-MARK
;;;	02/01/18	Dario		Changed definitions
;;;					  GET.LISP.DEFINITION.CONSTRUCTOR
;;;	02/03/13	Pedro Matos	Changed definitions
;;;					  INSERT-ITEM-MARK
;;;					  INSERT-NUMBERED-MARK
;;;					  INSERT-BULLETED-MARK
;;;					  INSERT-PRE-MARK
;;;					  INSERT-BEGIN-END-MARK
;;;					  INSERT-ARG-MARK
;;;					  INSERT-ELEM-MARK
;;;					  INSERT-REF-MARK
;;;					  INSERT-EMPH-MARK
;;;	03/07/28	A. Frazao	Changed definitions
;;;					  EXECUTE-DOCUMENTATION-ATTRIBUTE-ITEM
;;;					  (SETF MOUSE-DOCUMENTATION-MENU-ITEMS)
;;;					  MOUSE-DOCUMENTATION-MENU-ITEMS
;;;					  (SETF BASIC-DOCUMENTATION-MENU-ITEMS)
;;;					  INSERT-PARAGRAPH
;;;					  BASIC-DOCUMENTATION-MENU-ITEMS
;;;					Removed main utilities
;;;	06/08/14	J. P. Varandas	Changed definitions
;;;					  ALL.CONSTRUCTORS.TABLE
;;;					  (SETQ %ALL.CONSTRUCTORS%)
;;;-----------------------------------------------------------------------------

(load "sc-doc-parameters")
(load "sc-doc-outline")

(defvar +tab+ 9)
(defvar +new.line+ 10)
(defvar +backslash+ 92)
(defvar +point+ 46)


(defvar *defstrings* nil
  "Keeps all constructor keyword regexp strings.")



(defun insert.in.separated.lines (items &key prefix)
  "Inserts items, one in each line and prefixed by PREFIX."
  (unless prefix (setq prefix ""))
  (dolist (item items)
    (insert prefix item +new.line+)))



(defun all.keywords (table)
  "Prints all keywords of TABLE."
  (maphash #'(lambda (key value)
	       (print key))
	   table))

;;;; Substituted by regexp-quote.
;;;(defun convert.to.rexp (str)
;;;  "Converts a string to a regular expression that accepts characters '\ and '. ."
;;;  (let ((rexp ""))
;;;    (setq str (string-to-sequence str 'list))
;;;    (dolist (char str)
;;;      (setq rexp
;;;	(concat rexp
;;;		(cond ((char-equal +backslash+ char)
;;;		       "\\\\")
;;;		      ((char-equal +point+ char)
;;;		       "\\.")
;;;		      (t (char-to-string char))))))
;;;    rexp))


;;; Attributes

(defvar *all.attrs.keywords* nil
  "Keeps all attribute keywords ready to be used by a rexp search.")

(setq *all.attrs.keywords* nil)


(defun all.attrs.keywords ()
  *all.attrs.keywords*)


(defstruct attribute
  keyword
  rexp ;; just for efficiency.
  optional.p
  order
  default)

(defun mandatory.attributes (attributes)
  (remove-if #'attribute-optional.p attributes))


(defun optional.attributes (attributes)
  (remove-if-not #'attribute-optional.p attributes))


;; Attribute constructors

(defun make.attribute (keyword optional.p order &optional default)
  (let ((rexp (regexp-quote (downcase keyword))))
    (pushnew rexp *all.attrs.keywords* :test #'string=)
    (make-attribute :keyword keyword :optional.p optional.p
		    :order order :rexp rexp
		    :default default)))


;;; Constructors

(defstruct constructor
  keyword
  attributes)


(defvar %all.constructors% nil
    "Keeps all constructors.")

;;;-----------------------------------------------------------------------------
;;;(SETQ %ALL.CONSTRUCTORS%)
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	06/08/14	J. P. Varandas	Changed the test from STRING= to EQUAL
;;;-----------------------------------------------------------------------------
(setq %all.constructors% (make-hash-table :test #'equal))


;;;-----------------------------------------------------------------------------
;;;ALL.CONSTRUCTORS.TABLE
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
;;;	06/08/14	J. P. Varandas	Changed the test from STRING= to EQUAL
;;;-----------------------------------------------------------------------------
(defun all.constructors.table ()
  "Returns the table that keeps all the constructors."
  (or %all.constructors%
      (setq %all.constructors% (make-hash-table :test #'equal))))


(defun add.constructor (con)
  "Adds a constructor to the table."
  (let ((rexp (regexp-quote (upcase (constructor-keyword con)))))
    (pushnew rexp *defstrings* :test #'string=)
    (setf (gethash (constructor-keyword con) (all.constructors.table))
      con)))


(defun get.constructor (key)
  "Returns the constructor with keyword KEY."
  (let ((cstruct (gethash key (all.constructors.table))))
    (unless cstruct
      (beep)
      (setq cstruct (gethash "UNKNOWN" (all.constructors.table))))
    cstruct))
    
;;;-----------------------------------------------------------------------------
;;;GET-NEXT-ARGUMENTS
;;;Description
;;;	Returns the arguments of a function/method one-by-one.
;;;History
;;;	Date		Author		Description
;;;	00/01/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun get-next-arguments ()
  (next-non-space)
  (cond ((= (char-after (point)) 41)	; )
	 nil)
	((= (char-after (point)) 38)	; &
	 (forward-sexp 1)
	 :key)
	((= (char-after (point)) 40)	; (
	 (let (start end list)
	   (forward-char 1)
	   (setq start (point))
	   (while (not (= (char-after (point)) 41))
	     (forward-sexp)
	     (push (buffer-substring start (point)) list)
	     (next-non-space)
	     (setq start (point)))
	    (forward-char 1)
	   (reverse list)))
	(t (setq start (point))
	   (forward-sexp 1)
	   (buffer-substring start (point)))))

;;;-----------------------------------------------------------------------------
;;;GET-ARGUMENTS
;;;Description
;;;	Gets the arguments of the function/method separating them between normal arguments
;;;	and &key/&rest/&optional arguments
;;;History
;;;	Date		Author		Description
;;;	00/01/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun get-arguments ()
  (let ((optional nil)
	(keys nil)
	(rest nil)
	(args nil)
	(list nil)
	(key  nil))
    (while (not (= (char-after (point)) 40))
      (forward-char 1))
    (forward-char 1)
    (while (setq arg (get-next-arguments))
      (if (stringp arg)
	  (if key
	      (push arg keys)
	      (push arg args))
	  (if (eq arg :key)
	      (setf key t)
	      (if key
		  (push arg keys)
		  (push arg args)))))
    (list (reverse args) (reverse keys))))

;;;-----------------------------------------------------------------------------
;;;INSERT-ARGUMENTS
;;;Description
;;;	Inserts the documentation template of the function/method arguments
;;;History
;;;	Date		Author		Description
;;;	00/01/31	J. P. Varandas	Created
;;;	00/02/08	J. P. Varandas	Replace \\emph by \\elem
;;;	00/03/21	cpla    	Changed "void." to "void".
;;;	00/04/14	cpla    	Changed "is an object.." to "is a".
;;;-----------------------------------------------------------------------------
(defun insert-arguments ()
  (let ((args-list nil)
	(mode-fillers (mode-fillers)))
    (save-excursion 
      (end-of-defun)
      (beginning-of-defun)
      (forward-char 1)
      (forward-sexp 2)
      (setf args-list (save-excursion (get-arguments))))
    (let ((args (first args-list))
	  (keys (second args-list))
	  (comment (first mode-fillers))
	  (key nil))
      
      (if (or args keys)
	  (progn
	    (dolist (arg args)
	      (cond ((listp arg)
		     (insert comment +tab+ +tab+ "\\arg{" (car arg) "} is a \\elem{" (car (cdr arg)) "}." 
			     +new.line+ comment +tab+ +tab+ +new.line+))
		    (t
		     (insert comment +tab+ +tab+ "\\arg{" arg "} is a <>." +new.line+ comment +tab+ +tab+ +new.line+))))
	    (dolist (arg keys)
	      (cond ((listp arg)
		     (insert comment +tab+ +tab+ "\\arg{" (car arg) "} is a <>." +new.line+ comment +tab+ +tab+ +new.line+))
		    (t
		     (insert comment +tab+ +tab+ "\\arg{" arg "} is a <>." +new.line+ comment +tab+ +tab+ +new.line+)))))
	  (insert comment +tab+ +tab+ "void" +new.line+ comment +tab+ +tab+ +new.line+)))))

;;;-----------------------------------------------------------------------------
;;;GET-CLASS-ATTRIBUTES
;;;Description
;;;	Returns the name of the class atributes.
;;;History
;;;	Date		Author		Description
;;;	00/01/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun get-class-attributes ()
  (let ((list nil)
	(attrs nil)
	(start (point))
	(end nil))
    (forward-sexp 1)
    (setf end (point))
    (setf list (car (read-from-string (buffer-substring start end))))
    (dolist (elem list)
      (push (format "%s" (car elem)) attrs))
    (reverse attrs)))

;;;-----------------------------------------------------------------------------
;;;INSERT-CLASS-ATTRIBUTES
;;;Description
;;;	Inserts the documentation template of the class attributes
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/01/31	J. P. Varandas	Created
;;;	00/04/14	cpla    	Changed "void." to "void". 
;;;-----------------------------------------------------------------------------
(defun insert-class-attributes ()
  (let ((attr-list nil)
	(mode-fillers (mode-fillers)))
    (save-excursion 
      (end-of-defun)
      (beginning-of-defun)
      (forward-char 1)
      (while (not (= (char-after (point)) 40))
	(forward-char 1))
      (forward-sexp 1)
      (while (not (= (char-after (point)) 40))
	(forward-char 1))
      (setf attr-list (save-excursion (get-class-attributes))))
    (let ((comment (first mode-fillers)))
      (if attr-list
	  (dolist (attr attr-list)
	    (insert comment +tab+ +tab+ "\\emph{" attr "} is a <>." +new.line+ comment +tab+ +tab+ +new.line+))
	  (insert comment +tab+ +tab+ "void" +new.line+ comment +tab+ +tab+ +new.line+)))))


;;;-----------------------------------------------------------------------------
;;;INSERT-RETURN-TYPES
;;;Description
;;;	Inserts the documentation template of the return type.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/02/08	J. P. Varandas	Created
;;;	00/02/18	J. P. Varandas	Gets the comment from the mode-fillers
;;;	00/04/14	cpla    	Removed insertion of "void".
;;;	01/01/05	Dario		Added DEFAULT parameter.
;;;-----------------------------------------------------------------------------
(defun insert-return-types (&optional default)
  (let* ((mode-fillers (mode-fillers))
	 (comment (first mode-fillers)))
    ;; "void" should not be inserted automatically. This value means that the
    ;; programmer has the explicit intention of making irrelevant possible return
    ;; values.
    ;(insert comment +tab+ +tab+ +new.line+ comment +tab+ +new.line+ )
    (cond ((functionp default)
	   (insert.in.separated.lines (funcall default) :prefix (concatenate 'string comment "\t\t")))
	  ((consp default)
	   (insert.in.separated.lines default :prefix (concatenate 'string comment "\t\t")))
	  ((stringp default)
	   (insert comment +tab+ +tab+ default +new.line+)))
    (insert comment +tab+ +tab+ +new.line+)))

;;;; Main functions

;;;-----------------------------------------------------------------------------
;;;INSERT.ATTRIBUTE
;;;Description
;;;	Inserts an attribute at the beginning of the line following POS.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{attr} is a <>.
;;;		
;;;		\arg{pos} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/01/31	J. P. Varandas	When the attribute is '\args' or '\class-attrs'
;;;					the system inserts the list os arguments or 
;;;					the list of the class attributes of the element
;;;	00/02/08	J. P. Varandas	When the attribute is '\return-types'
;;;					the system inserts the return type documentation
;;;					of the element
;;;	00/03/21	cpla    	Added positioning of cursor.
;;;                                     Default value can be a function.
;;;	01/01/05	Dario		Also allows default values for:
;;;					RETURN-TYPES.
;;;					The value of DEFAULT can be a list of
;;;					strings which are to be inserted in
;;;					separated lines.
;;;-----------------------------------------------------------------------------
(defun insert.attribute (attr pos)
  "Inserts an attribute at the beginning of the line following POS."
  (let* ((mode-fillers (mode-fillers))
	 (comment (first mode-fillers))
	 (default (attribute-default attr)))
    (goto-char pos)
    (forward-line 1)
    (beginning-of-line)
    (insert comment +tab+ (attribute-keyword attr) +new.line+)
    (setq pos (point))
    (if (string-equal "\\args" (attribute-keyword attr))
	(insert-arguments)
	(if (string-equal "\\class-attrs" (attribute-keyword attr))
	    (insert-class-attributes)
	    (if (string-equal "\\return-types" (attribute-keyword attr))
		(insert-return-types default)
		(progn
		  (when default
		    (cond ((functionp default)
			   (insert.in.separated.lines (funcall default) :prefix (concatenate 'string comment "\t\t")))
			  ((consp default)
			   (insert.in.separated.lines default :prefix (concatenate 'string comment "\t\t")))
			  (t 
			   (insert comment +tab+ +tab+ default +new.line+))))
		  (insert comment +tab+ +tab+ +new.line+)))))
    (goto-char pos)
    (end-of-line)))


(defun attribute.exist.p (attr start end)
  "Verifies if keyword of ATTR already exists in region delimited by START and END.
  If exists returns its start point."
  (save-excursion
      (goto-char start)
      (re-search-forward (attribute-rexp attr) end t)))


(defun doc.region.limits ()
  "Returns the limits of the documentation region (description): (start end)."
  (save-excursion
    (let ((start (start.of.comment.block)))
      (when start
	(let ((end (end.of.comment.block)))
	  (when end
	    (goto-char start)
	    (setq start (re-search-forward (main.header.rexp) end t))
	    (when start
	      (setq end (main.end.body.fn end))
	      (when (and start end)
		(list start end)))))))))


(defun attribute.position (attr start end &optional context)
  "Defines the start position where the attribute must be placed."
  (block attribute.position
    (when context
      (let ((attribs (stable-sort (copy-list context) #'> :key #'attribute-order))
	    (pos nil)
	    (pos-aux nil))
	(dolist (attrib attribs)
	  (when (>= (attribute-order attr) (attribute-order attrib))
	    (return))
	  (setq pos-aux (attribute.exist.p attrib start end))
	  (when pos-aux (setq pos pos-aux)))
	(when pos
	  (return-from attribute.position
	    (save-excursion
	      (goto-char pos)
	      (forward-line -1)
	      (end-of-line)
	      (point))))
	))
    end))


(defun insert.attribute.cond (attr &optional context)
  "Inserts the attribute keyword at the current comment region (if it not exists)."
  (let ((limits (doc.region.limits)))
    (when limits
      (let ((start (first limits))
	    (end (second limits)))
	(when (not (attribute.exist.p attr start end))
	  (insert.attribute attr (attribute.position attr start end context)))))))


;;;-----------------------------------------------------------------------------
;;;GET.LISP.DEFINITION.CONSTRUCTOR
;;;Description
;;;	Returns the current lisp definition constructor.
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
;;;	00/02/08	J. P. Varandas	Skips the elements "let", "unless", 
;;;					"with-stable-string-output-stream", and "eval-when"
;;;	02/01/18	Dario		Skips the element "let*".
;;;-----------------------------------------------------------------------------
(defun get.lisp.definition.constructor ()
  "Returns the current lisp definition constructor."
  (let (start end def current-point)
    (and (ignore-errors
	  (end-of-defun)
	  (setq end (point))
	  (beginning-of-defun)
	  (setq start (point))
	  (setq current-point start))
	 (or (ignore-errors
	      (while (< current-point end)
		(while (not (= (char-after (point)) 40))
		  (forward-char 1))
		(forward-char 1)
		(setq current-point (point))
		(forward-sexp)
		(setq def (downcase (buffer-substring current-point (point))))
		(cond ((or (string-equal def "let")
			   (string-equal def "let*")
			   (string-equal def "unless") 
			   (string-equal def "with-stable-string-output-stream")
			   (string-equal def "eval-when"))
		       (forward-sexp)
		       (setq current-point (point)))
		      (t 
		       (setq current-point end))))
	      (upcase def))
	     (progn
	       (goto-char start)
	       (forward-char 1)
	       (get-next-name))))))


(defun get.definition.constructor ()
  "Returns the current definition constructor."
  (get.lisp.definition.constructor))


(defun always-t ()
  t)


(defun create.menu.items (ctruct &key filter)
  "Menu items of constructor CTRUCT."
  (unless filter (setq filter #'always-t))
  (let ((attrs (remove-if-not filter (constructor-attributes ctruct)))
	items)
    (dolist (attr (stable-sort (copy-list attrs) #'string< :key #'attribute-keyword))
      (push (sc-make-menu-item (attribute-keyword attr) `(lambda () (insert.attribute.cond ,attr ',attrs)))
	    items))
    (nreverse items)))


(defun insert.mandatory.attributes ()
  "Inserts mandatory attribute keywords. Uses current definition constructor."
  (interactive)
  (let ((ctruct (save-excursion (get.definition.constructor))))
    (when ctruct
      (let ((constructor (get.constructor ctruct)))
	(when constructor
	  (let ((attributes (constructor-attributes constructor)))
	    (dolist (attr (mandatory.attributes attributes))
	      (insert.attribute.cond attr attributes))))))))
    

(defun insert.optional.attributes (arg)
  "Inserts optional attribute keywords. Uses current definition constructor."
  (interactive "e")
  (insert.mandatory.attributes)
  (let ((ctruct (save-excursion (get.definition.constructor))))
    (when ctruct
      (let ((constructor (get.constructor ctruct)))
	(when constructor
	  (let* ((items (create.menu.items constructor :filter #'attribute-optional.p))
		 (op (sc-popup-menu arg (format "%s optional attributes" ctruct) items)))
	    (when op (funcall op))))))))



;;;-----------------------------------------------------------------------------
;;; Menu Documentação Interna
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;INSERT-DOC-MARK
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \elem{string}.
;;;		
;;;	\return-types
;;;	\unable-to-document
;;;		01/07/04	J. P. Varandas
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/07/04	J. P. Varandas	If the mark is not active it will move 
;;;					backwards to discover the nearst word and 
;;;					surround it with the \arg{name} doc mark
;;;-----------------------------------------------------------------------------
(defun insert-doc-mark (name)
  (let ((transient-mark-mode nil))
    (if mark-active
	(let ((start (min (point) (mark)))
	      (end (max (point) (mark))))
	  (goto-char end)
	  (when (= (char-after (- end 1)) 46)
	    (goto-char (- end 1)))
	  (insert "}")
	  (goto-char start)
	  (insert "\\" name "{")
	  (while (not (= (char-after (point)) 125))
	    (forward-char 1))
	  (forward-char 1))
	(let ((start (point))
	      (end (point)))
	  (when (= (char-after (- end 1)) 46)
	    (goto-char (- end 1)))
	  (if (= (char-after (- (point) 1)) 62)
	      (progn
		(backward-char 4)
		(delete-char 4)
		(insert "to ignore")
		)
	      (progn
		(insert "}")
		(backward-char 1)
		(while (and (not (= (char-after (point)) 32))
			    (not (= (char-after (point)) 9)))
		  (backward-char 1))
		(forward-char 1)
		(insert "\\" name "{")
		(while (not (= (char-after (point)) 125))
		  (forward-char 1))
		(forward-char 1))))
      )))



;;;-----------------------------------------------------------------------------
;;;INSERT-EMPH-MARK
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
;;;	02/03/13	Pedro Matos	Added interactive
;;;-----------------------------------------------------------------------------
(defun insert-emph-mark ()
  (interactive)
  (insert-doc-mark "emph"))

;;;-----------------------------------------------------------------------------
;;;INSERT-REF-MARK
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
;;;	02/03/13	Pedro Matos	Added interactive
;;;-----------------------------------------------------------------------------
(defun insert-ref-mark ()
  (interactive)
  (insert-doc-mark "ref"))

;;;-----------------------------------------------------------------------------
;;;INSERT-ELEM-MARK
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
;;;	02/03/13	Pedro Matos	Added interactive
;;;-----------------------------------------------------------------------------
(defun insert-elem-mark ()
  (interactive)
  (insert-doc-mark "elem"))

;;;-----------------------------------------------------------------------------
;;;INSERT-ARG-MARK
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
;;;	02/03/13	Pedro Matos	Added interactive
;;;-----------------------------------------------------------------------------
(defun insert-arg-mark ()
  (interactive)
  (insert-doc-mark "arg"))

;;;-----------------------------------------------------------------------------
;;;INSERT-BEGIN-END-MARK
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{names} is a <>.
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	02/03/13	Pedro Matos	Added interactive
;;;-----------------------------------------------------------------------------
(defun insert-begin-end-mark (names)
  (interactive)
  (let ((transient-mark-mode nil)
	(filler ";;;		"))
    (if mark-active
	(let ((start (min (point) (mark)))
	      (end (max (point) (mark))))
	  (goto-char end)
	  (beginning-of-line)
	  (if (not (= end (point)))
	      (forward-line))
	  (insert filler "\\end{" (car names) "}" 10)
	  (goto-char start)
	  (beginning-of-line)
	  (insert filler "\\begin{" (car names))
	  (dolist (name (cdr names))
	    (insert "," name))
	  (insert "}" 10))
	(progn
	  (forward-line)
	  (insert filler "\\begin{" (car names))
	  (dolist (name (cdr names))
	    (insert "," name))
	  (insert "}" 10)
	  (insert filler "\\end{" (car names) "}" 10)))))

;;;-----------------------------------------------------------------------------
;;;INSERT-PRE-MARK
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
;;;	02/03/13	Pedro Matos	Added interactive
;;;-----------------------------------------------------------------------------
(defun insert-pre-mark ()
  (interactive)
  (insert-begin-end-mark '("pre")))

;;;-----------------------------------------------------------------------------
;;;INSERT-BULLETED-MARK
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
;;;	02/03/13	Pedro Matos	Added interactive
;;;-----------------------------------------------------------------------------
(defun insert-bulleted-mark ()
  (interactive)
  (insert-begin-end-mark '("itemize" "bulleted")))

;;;-----------------------------------------------------------------------------
;;;INSERT-NUMBERED-MARK
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
;;;	02/03/13	Pedro Matos	Added interactive
;;;-----------------------------------------------------------------------------
(defun insert-numbered-mark ()
  (interactive)
  (insert-begin-end-mark '("itemize" "numbered")))

;;;-----------------------------------------------------------------------------
;;;INSERT-ITEM-MARK
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
;;;	02/03/13	Pedro Matos	Added interactive
;;;-----------------------------------------------------------------------------
(defun insert-item-mark ()
  (interactive)
  (insert "\\item "))

;;;-----------------------------------------------------------------------------
;;;INSERT-PARAGRAPH
;;;Description
;;;	Inserts two documentation empty lines.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Updated documentation.
;;;-----------------------------------------------------------------------------
(defun insert-paragraph ()
  (indent-new-comment-line)
  (indent-new-comment-line))


;;;-----------------------------------------------------------------------------
;;;BASIC-DOCUMENTATION-MENU-ITEMS
;;;Description
;;;	Stores a list of documentation menu items for the menu bar.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Changed name from fr-basic-documentation-list
;;;-----------------------------------------------------------------------------
(defvar basic-documentation-menu-items nil)

;;;-----------------------------------------------------------------------------
;;;(SETF BASIC-DOCUMENTATION-MENU-ITEMS)
;;;Description
;;;	Sets the list of documentation menu items for the menu bar.
;;;	
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Changed name from fr-basic-documentation-list
;;;-----------------------------------------------------------------------------
(setf basic-documentation-menu-items
  (list (sc-make-menu-item "Insert Paragraph" 'insert-paragraph)
	""
	(sc-make-menu-item "Element" 'insert-elem-mark)
	(sc-make-menu-item "Emphasys" 'insert-emph-mark)
	(sc-make-menu-item "Reference" 'insert-ref-mark)
	(sc-make-menu-item "Argument" 'insert-arg-mark)
	(sc-make-menu-item "Item" 'insert-item-mark)
	(sc-make-menu-item "Pre-formated" 'insert-pre-mark)
	(sc-make-menu-item "Bulleted" 'insert-bulleted-mark)
	(sc-make-menu-item "Numbered" 'insert-numbered-mark)))


;;; Definition of Doc Map


(defvar doc-map)

(setq doc-map
  (make-sparse-keymap "Documentation tool"))

(fset 'doc doc-map)

(define-key doc-map [Outline]
  '("Outline" . outline))

(define-key doc-map [separator1]
  '("--" . nil))

(define-key doc-map [optional]
  '("Optional attributes..." . insert.optional.attributes))

(define-key doc-map [mandatory]
  '("Mandatory attributes" . insert.mandatory.attributes))

(put 'insert.mandatory.attributes 'menu-enable '(comment.line.p))
(put 'insert.optional.attributes 'menu-enable '(comment.line.p))

(define-key global-map %insert-mandatory-attr-key%
   'insert.mandatory.attributes)

;;; Addition of Menu Doc to global-map.

(define-key-after (lookup-key global-map [menu-bar])
    [doc] '("Doc" . doc) 'edit)

;;; doc utils
;;;-----------------------------------------------------------------------------
;;; Actualização do item doc da barra de menus
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;EXECUTE-DOCUMENTATION-ATTRIBUTE-ITEM
;;;Description
;;;	Executes a documentation attribute item.
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
;;;	03/07/28	A. Frazao	fr-basic-documentation-list -> basic-documentation-menu-items
;;;-----------------------------------------------------------------------------
(defun execute-documentation-attribute-item (arg)
  (interactive "e")
  (let ((choice (sc-popup-menu arg "Documentation attributes" basic-documentation-menu-items)))
    (if choice
	(funcall choice))))

(define-key doc-map [separator2]
  '("--" . nil))

(define-key doc-map [attributes]
  '("Attributes" . execute-documentation-attribute-item))

(put 'execute-documentation-attribute-item 'menu-enable '(comment.line.p))


;;;; Load of specific modules

(load "sc-constructors")

;;;-----------------------------------------------------------------------------
;;;MOUSE-DOCUMENTATION-MENU-ITEMS
;;;Description
;;;	Stores a list of documentation menu items for the mouse menu.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Changed name from fr-documentation-list
;;;-----------------------------------------------------------------------------
(defvar mouse-documentation-menu-items nil)

;;;-----------------------------------------------------------------------------
;;;(SETF MOUSE-DOCUMENTATION-MENU-ITEMS)
;;;Description
;;;	Sets the list of documentation menu items for the mouse menu.
;;;	
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Changed name from fr-documentation-list
;;;					Added the online optional attributes
;;;-----------------------------------------------------------------------------
(let ((attrs (common.optional.attributes))
      (items nil))
  (dolist (attr (stable-sort (copy-list attrs) #'string< :key #'attribute-keyword))
    (push (sc-make-menu-item (attribute-keyword attr) `(insert.attribute.cond ,attr ',attrs))
	  items))
  (push "" items)
  (push (sc-make-menu-item "Mandatory attributes" 'insert.mandatory.attributes) items)
  (setf mouse-documentation-menu-items
    (append basic-documentation-menu-items
	    '("")
	    (nreverse items))))

