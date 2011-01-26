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
;;;	Defines all possible documentation constructors.
;;;
;;;	Notes: most of the constructors here listed are also listed in
;;;	the quality technical document "Lisp Constructors".
;;;
;;; History
;;;	Date		Author		Description
;;;	99/11/11	cpla	        Created.
;;;	00/03/21	cpla    	c.kb -> c.sike.kb
;;;					Added c.unknown.
;;;	00/03/28	cpla    	Added new constructors 
;;;					  (reported by Tiago Pires and Fausto Almeida).
;;;	00/04/14	cpla    	Changed definitions
;;;					  C.MACRO
;;;					Added new constructors
;;;					  (reported by Dario, Manuel Oliveira, 
;;;					   Hugo Mendonça, Ricardo Roda and A. Silva). 
;;;	00/06/08	Dario		 Added definitions
;;;					  C.SPECIAL
;;;					Changed definitions
;;;					  C.SYSTEM
;;;					  C.PACKAGE
;;;					  C.SIKE.KB
;;;					  C.METHOD
;;;					  C.SIKE.CLASS
;;;					  C.CLASS
;;;					  C.CONSTANT
;;;					  C.PARAMETER
;;;					  C.VARIABLE
;;;					  C.MACRO
;;;					  C.FUNCTION
;;;					  C.BASIC
;;;					  C.UNKNOWN
;;;					  %ALL.CONSTRUCTOR.SPEC%
;;;	00/08/22	Dario		Changed definitions
;;;					  %ALL.CONSTRUCTOR.SPEC%
;;;					Added definitions
;;;					  C.METHOD.WITHOUT.RETURN.TYPES
;;;	01/01/05	Dario		Changed definitions
;;;					  C.FUNCTION
;;;					  C.SPECIAL
;;;					  C.SYSTEM
;;;					  C.PACKAGE
;;;					  C.SIKE.KB
;;;					  C.SIKE.CLASS
;;;					  C.CLASS
;;;					  C.CONSTANT
;;;					  C.PARAMETER
;;;					  C.VARIABLE
;;;					  C.MACRO
;;;					  C.BASIC
;;;					  C.UNKNOWN
;;;					  C.METHOD
;;;					  %ALL.CONSTRUCTOR.SPEC%
;;;					Added definitions
;;;					  UPDATE.CATEGORY.ATTRS
;;;					  SUBSTITUTE.ATTRS
;;;					Deleted definitions
;;;					  C.METHOD.WITHOUT.RETURN.TYPES
;;;	01/02/20	Dario		Changed definitions
;;;					  SUBSTITUTE.ATTRS>
;;;	01/04/20	Dario		Changed definitions
;;;					  %ALL.CONSTRUCTOR.SPEC%
;;;	01/07/04	J. P. Varandas	Changed definitions
;;;					  %ALL.CONSTRUCTOR.SPEC%
;;;	01/10/12	Dario		Changed definitions
;;;					  %ALL.CONSTRUCTOR.SPEC%
;;;	02/01/18	Dario		Changed definitions
;;;					  %ALL.CONSTRUCTOR.SPEC%
;;;	02/02/26	Dario		Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	02/03/01	Dario		Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	02/06/07	J. P. Varandas	Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	02/08/28	Dario		Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	03/05/27	Dario		Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	04/01/15	Dario		Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	04/01/21	Dario		Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	04/07/29	Dario		Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	04/08/02	Dario		Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	04/10/13	J. P. Varandas	Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	06/04/06	J. P. Varandas	Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	08/02/22	Tom Weissmann	Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	08/10/09	Tom Weissmann	Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	99/88/77	Tom Weissmann	Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;	10/11/19	Rui Patrocinio	Changed definitions
;;;					  (SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;-----------------------------------------------------------------------------

(load "sc-attributes")


;;;; Utilities

;;;-----------------------------------------------------------------------------
;;;SUBSTITUTE.ATTRS
;;;Description
;;;	Substitutes, adds and removes attributes in a given category.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs} is a \emph{list} of attributes.
;;;		
;;;		\arg{default.attrs.values} is a \emph{list} of attributes
;;;		and default values.
;;;		
;;;	\return-types
;;;		\emph{list} with the possibly new default attributes and values.
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		Created
;;;	01/02/20	Dario		Use remove-if and append instead of
;;;					their destructive counterparts.
;;;-----------------------------------------------------------------------------
(defun substitute.attrs (default.attrs default.attrs.values)
  ;; First delete the attributes.
  (let ((last (first (last default.attrs.values))))
    (when (eql (first last) 'remove)
      (let ((attrs.to.remove (rest last)))
	(dolist (attr.to.remove attrs.to.remove)
	  (setf default.attrs
		(remove-if #'(lambda (default.attr)
			       (eql attr.to.remove (first default.attr)))
			   default.attrs)))
	(setf default.attrs.values (butlast default.attrs.values)))))
  ;; Then substitute them.
  (dolist (default.attr.value default.attrs.values)
    (setf default.attrs
	  (if (find-if #'(lambda (default.attr)
			   (eql (first default.attr) (first default.attr.value)))
		       default.attrs)
	      (nsubstitute-if default.attr.value 
			      #'(lambda (default.attr)
				  (eql (first default.attr) (first default.attr.value)))
			      default.attrs)
	      (append default.attrs (list default.attr.value)))))
  default.attrs)

;;;-----------------------------------------------------------------------------
;;;UPDATE.CATEGORY.ATTRS
;;;Description
;;;	Creates the attributes structure by evaluting the list of attributes,
;;;	possibly replacing some of the values for attributes given in
;;;	\arg{default.attrs.values}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs} is a \emph{list} of attributes.
;;;		
;;;		\arg{default.attrs.values} is a \emph{list} of attributes
;;;		and default values.
;;;		
;;;	\return-types
;;;		\emph{list} with the possibly new default attributes and values.
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		Created
;;;-----------------------------------------------------------------------------
(defun update.category.attrs (default.attrs default.attrs.values)
  (mapcar #'eval (if default.attrs.values
		       (substitute.attrs default.attrs default.attrs.values)
		       default.attrs)))

;;;; Attributes by constructor category

;;;-----------------------------------------------------------------------------
;;;C.UNKNOWN
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} is list of 
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		Calls \ref{update.category.attrs}.
;;;					Added optional \arg{default.attrs.values}.
;;;-----------------------------------------------------------------------------
(defun c.unknown (&optional default.attrs.values) ;; Default attributes for unknown constructors. 
  (let ((default.attrs `((attr.visibility) 
			 (attr.args t) 
			 (attr.class-attrs t) 
			 (attr.return-types t) 
			 (attr.error))))
    (update.category.attrs default.attrs default.attrs.values)))
  
;;;-----------------------------------------------------------------------------
;;;C.BASIC
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
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		Calls \ref{update.category.attrs}.
;;;					Added optional \arg{default.attrs.values}.
;;;-----------------------------------------------------------------------------
(defun c.basic (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility))))
    (update.category.attrs default.attrs default.attrs.values)))

;;;-----------------------------------------------------------------------------
;;;C.FUNCTION
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} is a list of attributes and optional
;;;		default values.
;;;		Each attribute consists of a call to the respective
;;;		function (see any of the attribute functions for example see
;;;		\ref{attr.return-types}).
;;;		The last element of the list can be a list of attributes to remove
;;;		example: (remove attr.args attr.return-types)
;;;		
;;;		
;;;	\return-types
;;;		\emph{list} of elements of type \emph{attribute}.
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		Calls \ref{update.category.attrs}.
;;;					Added optional \arg{default.attrs.values}.
;;;-----------------------------------------------------------------------------
(defun c.function (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility) (attr.args) (attr.return-types))))
    (update.category.attrs default.attrs default.attrs.values)))

;;;-----------------------------------------------------------------------------
;;;C.MACRO
;;;Description
;;;	MACRO constructor category specific attributes. 
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see \ref{c.function}.
;;;		
;;;	\return-types
;;;		see \ref{c.function}.
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/04/14	cpla    	Changed "args" to mandatory;
;;;					"class-attrs" were removed.
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		Calls \ref{update.category.attrs}.
;;;					Added optional \arg{default.attrs.values}.
;;;-----------------------------------------------------------------------------
(defun c.macro (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility) 
			 (attr.args) 
			 (attr.return-types t))))
    (update.category.attrs default.attrs default.attrs.values)))

;;;-----------------------------------------------------------------------------
;;;C.VARIABLE
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see \ref{c.function}.
;;;		
;;;	\return-types
;;;		see \ref{c.function}.
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		Calls \ref{update.category.attrs}.
;;;					Added optional \arg{default.attrs.values}.
;;;-----------------------------------------------------------------------------
(defun c.variable (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility))))
    (update.category.attrs default.attrs default.attrs.values)))
  
;;;-----------------------------------------------------------------------------
;;;C.PARAMETER
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see ref{c.function}
;;;		
;;;	\return-types
;;;		see ref{c.function}
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		Calls \ref{update.category.attrs}.
;;;					Added optional \arg{default.attrs.values}.
;;;-----------------------------------------------------------------------------
(defun c.parameter (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility))))
    (update.category.attrs default.attrs default.attrs.values)))
	
;;;-----------------------------------------------------------------------------
;;;C.CONSTANT
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see \ref{c.function}.
;;;		
;;;	\return-types
;;;		see ref{c.function}
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		Calls \ref{update.category.attrs}.
;;;					Added optional \arg{default.attrs.values}.
;;;-----------------------------------------------------------------------------
(defun c.constant (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility))))
    (update.category.attrs default.attrs default.attrs.values)))

;;;-----------------------------------------------------------------------------
;;;C.CLASS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see \ref{c.function}.
;;;		
;;;	\return-types
;;;		see ref{c.function}
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		Calls \ref{update.category.attrs}.
;;;					Added optional \arg{default.attrs.values}.
;;;-----------------------------------------------------------------------------
(defun c.class (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility) (attr.class-attrs))))
    (update.category.attrs default.attrs default.attrs.values)))

;;;-----------------------------------------------------------------------------
;;;C.SIKE.CLASS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see \ref{c.function}.
;;;		
;;;	\return-types
;;;		see ref{c.function}
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		Calls \ref{update.category.attrs}.
;;;					Added optional \arg{default.attrs.values}.
;;;-----------------------------------------------------------------------------
(defun c.sike.class (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility) (attr.class-attrs))))
    (update.category.attrs default.attrs default.attrs.values)))

;;;-----------------------------------------------------------------------------
;;;C.METHOD
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see \ref{c.function}.
;;;		
;;;	\return-types
;;;		see ref{c.function}
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		Calls \ref{update.category.attrs}.
;;;					Added optional \arg{default.attrs.values}.
;;;-----------------------------------------------------------------------------
(defun c.method (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility) (attr.args) (attr.return-types))))
    (update.category.attrs default.attrs default.attrs.values)))
 

;;;-----------------------------------------------------------------------------
;;;C.SIKE.KB
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see \ref{c.function}.
;;;		
;;;	\return-types
;;;		see ref{c.function}
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun c.sike.kb (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility))))
    (update.category.attrs default.attrs default.attrs.values)))

;;;-----------------------------------------------------------------------------
;;;C.PACKAGE
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see \ref{c.function}.
;;;		
;;;	\return-types
;;;		see ref{c.function}
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun c.package (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility))))
    (update.category.attrs default.attrs default.attrs.values)))
	
;;;-----------------------------------------------------------------------------
;;;C.SYSTEM
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see \ref{c.function}.
;;;		
;;;	\return-types
;;;		see ref{c.function}
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added visibility attribute.
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun c.system (&optional default.attrs.values)
  (let ((default.attrs `((attr.visibility))))
    (update.category.attrs default.attrs default.attrs.values)))

;;;-----------------------------------------------------------------------------
;;;C.SPECIAL
;;;Description
;;;	New category for documenting special types of operators that are not
;;;	necessarily constructors.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{default.attrs.values} see \ref{c.function}.
;;;		
;;;	\return-types
;;;		see ref{c.function}
;;;	
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Created
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun c.special (&optional default.attrs.values)
  (declare (ignore default.attrs.values))
  )

;;;; Constructor definition

(defvar %all.constructor.spec% nil
  "Specifies all constructors")

;;;-----------------------------------------------------------------------------
;;;(SETQ %ALL.CONSTRUCTOR.SPEC%)
;;;Description
;;;	Specifies all constructors
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Added new constructor SETF.
;;;					Moved DEFSETF to language specific
;;;					section.
;;;	00/08/22	Dario		Added constructors:
;;;					DEF.ROSTER.OPER,
;;;					DEF.ST.OPER,
;;;					DEFINE.PROPERTY,
;;;					DEF.VOID.ACCESSOR.SLOT,
;;;					DEF.ROSTER.DELEGATED.METHOD,
;;;					DEF.METACLASS,
;;;					DEF.EXISTENCE.CONSTRAINT,
;;;					DEF.LOAD.INSTALLED.FILE,
;;;					DEF.GET.SAVE.CONCEPT.FN,
;;;					DEF.COLOR.
;;;	01/01/05	Dario		Added constructors:
;;;					Removed references to category
;;;					C.METHOD.WITHOUT.RETURN.TYPES.
;;;					Added constructors
;;;					DEF.EXTERNAL.CONSTRUCTOR
;;;					DEF.SHORT.TERM.STATE.METHOD
;;;					SET-MACRO-CHARACTER
;;;					DEFMETHOD.IS?
;;;					DEF.PATTERN.PREDICATE
;;;					DEF.PATTERN.LR
;;;					DEF.BASIC.DUTY.SETF.AROUND
;;;					DEFINE-PROJECT
;;;	01/04/20	Dario		Added constructors:
;;;					DEF.STRATEGY.ITEM.CLASS
;;;					DEF.STRATEGY.ITEM.BEHAVIOUR
;;;					MAKE.STRATEGY.ITEM
;;;					DEF.NEW.ACCESSORS
;;;					DEF.PROBLEM.CLASS
;;;					DEF.ENTITY.SHELL.CLASS
;;;					DEF.ROD.CLASS
;;;					DEF.TOD.CLASS
;;;					DEF.ROD.FILTER.CLASS
;;;					DEF.ROD.FILTER.CONSTRUCTOR
;;;					DEF.ROD.FILTER.P.METHOD
;;;					DEF.ROD.SORTER.CLASS
;;;					DEF.ROD.SORTER.CONSTRUCTOR
;;;	01/07/04	J. P. Varandas	Changed constructors:
;;;					DEF.COLOR
;;;					Added constructors:
;;;					DEF.GCONTEXT
;;;	01/10/12	Dario		Added constructor:
;;;					REMOVE.FROM.PROBLEM.LOCAL.FILES
;;;	02/01/18	Dario		Added constructors:
;;;					SET.CURRENT.COMPANY
;;;					DEFINE.WEEKDAYS
;;;					CHOOSE.LANGUAGE
;;;					DEFCOMPONENT
;;;					DEFINE-SETF-EXPANDER
;;;					DEFINE.PLIST.PROPERTY
;;;					DEF.SCHED.PARAMETER
;;;					DEF.BLOCK.GEN.RULE.CLASS
;;;					DEF.STRATEGY.ITEM.NAME
;;;					MAKE.STRATEGY
;;;					DEF.STRAT.CLASS
;;;					SETQ
;;;	02/02/26	Dario		Added constructors:
;;;					DEF.TRIP.FEATURE.ACCESSORES
;;;					DEFINE.MEMO.VERSION
;;;	02/03/01	Dario		Added constructors:
;;;					DEF.FREQ.TRAIN.FEATURE
;;;					DEF.LOCAL.FEATURE.ACCESSORES
;;;	02/06/07	J. P. Varandas	Updated constructors
;;;					  DEFCOMPONENT -> CG:DEFCOMPONENT
;;;					Added constructors:
;;;					  "MAP.OBJECTS.TO.LABELS"
;;;					  "MAP.OBJECTS.TO.APP.TOPICS"
;;;					  "MAP.OBJECTS.TO.COMMON.TOPICS"
;;;	02/08/28	Dario		Added constructors:
;;;					  "DEFINE.GENERIC.LOCAL.FILES"
;;;					  "DEFINE.SPECIFIC.LOCAL.FILES"
;;;	03/05/27	Dario		Added constructor:
;;;					  "DEF.ADDITIONAL.KNOWLEDGE.TABLE"
;;;	04/01/15	Dario		Added constructors:
;;;					  "DEF.EVALUATION"
;;;					  "DEF.EVALUATION.MIXIN"
;;;					  "DEF.SEARCH.OPERATOR"
;;;					  "DEF.OP.MIXIN"
;;;					  "DEF.FS.TEST"
;;;					  "DEF.FS.TEST.MIXIN"
;;;					  "DEF.SEARCH.STRATEGY"
;;;					  "DEF.SEARCH.STRATEGY.MIXIN"
;;;					  "DEF.SAM.VP.ACCESSOR"
;;;	04/01/21	Dario		Added constructor:
;;;					  "DEFINE.EQUIVALENT.TEMPORAL.TYPES"
;;;	04/07/29	Dario		Added constructors:
;;;					  "DEF.TASK.RECOGNIZER"
;;;					  "SET.SUPPLEMENTARY.VALIDATION.RULES"
;;;					  "SET.CONSTRAINT.RESERVE.TASK.TYPE"
;;;					  "DEF.ROSTER.PATTERN.OPERATION"
;;;	04/08/02	Dario		Added constructor:
;;;					  "DEFINE.PERIOD.PROPERTY"
;;;	04/10/13	J. P. Varandas	Added constructors
;;;					  "DEF.DRAW.WINDOW"
;;;					  "DEF.SELECTION.WINDOW"
;;;					  "DEF.TASK.RECOGNIZER"
;;;	06/04/06	J. P. Varandas	Added constructors
;;;					  "EXPORT"
;;;					  "SC-DEFINE-SYSTEM"
;;;					  "DEF.STATISTIC"
;;;					  "INSTALL.MENU.ITEM.SHORTCUTS"
;;;					  "DEF.STATISTICS.MENU.ITEMS"
;;;					  "DEF.STATISTICS.OCCUR.OUTPUT"
;;;					  "DEF.STATISTICS.TIME.OUTPUT"
;;;					  "DEF.VARIATION.SCHED.STATISTICS"
;;;					  "DEF.VARIATION.STATISTICS.MENU.ITEMS"
;;;					  "DEF.DELEGATED.METHOD"
;;;					  "ADD.TIMETABLE.SEQUENCING.METHOD"
;;;	08/02/22	Tom Weissmann	Added constructors
;;;                                       "ADD.BATCH.VALID.COMMAND"
;;;                                       "DEFINE.FUNCTIONALITY"
;;;	08/10/09	Tom Weissmann	"\elem" -> "\\elem" for DEFMETHOD.IS? (POA 12960.0)
;;;	99/88/77	Tom Weissmann	Added constructor
;;;					  "DEF.FPLANNER.OPERATOR" (POA 13404.0)
;;;	10/11/19	Rui Patrocinio	Added constructors (POA 12701.0)
;;;					  "DEF.DELEGATED.METHOD.IF"
;;;					  "DEF.ONTIME.OPERATOR"
;;;					  "DEF.IPT.SEARCH.STRATEGY"
;;;					  "DEF.TEST.METHOD"
;;;					  "DEF.RECOGNIZER.MANUAL.DUTIES.FOR.NOTIFICATIONS"
;;;-----------------------------------------------------------------------------
(setq %all.constructor.spec%
  (list

   `("UNKNOWN" ,(c.unknown))
   
   ;; Language specific

   `("DEFGENERIC" ,(c.function))
   `("DEFINE-CONDITION" ,(c.class))
   `("DEFINE-MODIFY-MACRO" ,(c.macro))
   `("DEFINE-SETF-EXPANDER" ,(c.basic))
   `("DEFUN" ,(c.function))
   `("DEFVAR" ,(c.variable))
   `("DEFMACRO" ,(c.macro))
   `("DEFPARAMETER" ,(c.parameter))
   `("DEFCONSTANT" ,(c.constant))
   `("DEFCLASS" ,(c.class))
   `("DEFMETHOD" ,(c.method))
   `("DEFPACKAGE" ,(c.package))
   `("DEFSETF" ,(c.method))
   `("DEFSTRUCT" ,(c.class))
   `("DEFTYPE" ,(c.macro))
   `("SET-DISPATCH-MACRO-CHARACTER" ,(c.basic))
   `("SETF" ,(c.special))		;SETF is not really a constructor so it is included in the
					; special category.
   `("SETQ" ,(c.special))
   `("SET-MACRO-CHARACTER" ,(c.basic))
   
   
   ;; Siscog specific
   `("ADD.BATCH.VALID.COMMAND" ,(c.basic))   
   `("CHOOSE.LANGUAGE" ,(c.basic))
   `("DEFINE.FUNCTIONALITY" ,(c.basic))
   `("DEFINE.GENERIC.LOCAL.FILES" ,(c.basic))
   `("DEFINE.SPECIFIC.LOCAL.FILES" ,(c.basic))
   `("DEFINE-PROJECT" ,(c.basic))
   `("DEFINE.WEEKDAYS" ,(c.basic))
   `("DEFINE.PERIOD.PROPERTY" ,(c.basic))
   `("DEFINE.PROPERTY" ,(c.method `((remove attr.return-types))))
   `("CG:DEFCOMPONENT" ,(c.class))
   `("DEFSYSTEM" ,(c.system))
   `("D@" ,(c.parameter))
   `("DEF.ADDITIONAL.KNOWLEDGE.TABLE" ,(c.macro))
   `("DEF.ALL.INSTS" ,(c.basic))
   `("DEF.AUX" ,(c.method))
   `("DEF.BASIC.DUTY.SETF.AROUND" ,(c.basic))
   `("DEF.CLASS" ,(c.sike.class))
   `("DEF.DRAW.WINDOW" ,(c.class))
   `("DEF.SELECTION.WINDOW" ,(c.class))
   `("DEF.EVALUATION" ,(c.sike.class))
   `("DEF.EVALUATION.MIXIN" ,(c.sike.class))
   `("DEF.SEARCH.OPERATOR" ,(c.sike.class))
   `("DEF.OP.MIXIN" ,(c.sike.class))
   `("DEF.FS.TEST" ,(c.sike.class))
   `("DEF.FS.TEST.MIXIN" ,(c.sike.class))
   `("DEF.SEARCH.STRATEGY" ,(c.sike.class))
   `("DEF.SEARCH.STRATEGY.MIXIN" ,(c.sike.class))
   `("DEF.SAM.VP.ACCESSOR" ,(c.basic))
   `("DEFINE.EQUIVALENT.TEMPORAL.TYPES" ,(c.method `((remove attr.return-types))))
   
   
   `("DEF.COLOR" ,(c.basic))
   `("DEF.GCONTEXT" ,(c.basic))
   `("DEFINE.PLIST.PROPERTY" ,(c.basic))
   `("DEF.CONSTRUCTOR" ,(c.basic))
   `("DEF.SCHED.PARAMETER" ,(c.basic))
   `("DEF.BLOCK.GEN.RULE.CLASS" ,(c.sike.class))
   `("DEF.CONSTRUCTOR&NAME" ,(c.basic))
   `("DEF-DATABASE-OPERATION" ,(c.basic))
   `("DEF.ENTITY.SHELL.CLASS" ,(c.sike.class))
   `("DEF.EXISTENCE.CONSTRAINT" ,(c.method `((remove attr.return-types))))
   `("DEF.EXTERNAL.CONSTRUCTOR" ,(c.basic))
   `("DEFINE.MEMO.VERSION" ,(c.basic))
   `("DEF.TRIP.FEATURE.ACCESSORES" ,(c.basic))
   `("DEF.FREQ.TRAIN.FEATURE" ,(c.basic))
   `("DEF.LOCAL.FEATURE.ACCESSORES" ,(c.basic))
   `("DEF.FIND.INST" ,(c.basic))
   `("DEF-FOREIGN-FUNCTION" ,(c.function))
   `("DEF.GET.INST" ,(c.basic))
   `("DEF.GET.SAVE.CONCEPT.FN" ,(c.special))
   `("DEF.GF" ,(c.method))
   `("DEF.GF.CLASS" ,(c.sike.class))
   `("DEF.HF" ,(c.method))
   `("DEF.HF.CLASS" ,(c.sike.class))
   `("DEF.STRATEGY.ITEM.NAME" ,(c.basic))
   `("DEF.KB" ,(c.sike.kb))
   `("DEF-LOAD-FROM-DB" ,(c.basic))
   `("DEF.LOAD.INSTALLED.FILE" ,(c.special))
   `("DEF.LR" ,(c.method))
   `("DEF.LR.CLASS" ,(c.sike.class))
   `("DEF.LR.PARAMETER" ,(c.parameter))
   `("DEF.EXTERNAL.ID" ,(c.basic))
   `("DEF.METACLASS" ,(c.sike.class))
   `("DEF.NEW.ACCESSORS" ,(c.basic))
   `("DEF.PROBLEM.CLASS" ,(c.sike.class))
   `("DEFMETHOD.IS?" ,(c.basic `((attr.return-types nil "A \\elem{boolean}"))))
   `("DEF.PATTERN.PREDICATE" ,(c.basic `((attr.return-types nil "An object of class \\elem{}"))))
   `("DEF.PATTERN.LR" ,(c.macro `((attr.remarks nil (list "Operations:" 
							  "\tRule's Pattern:"
							  "\tPattern Objects -"
							  "\tIgnore Objects -"
							  "Violation Culprits:"
							  "\tThe pattern objects."
							  "Violation Container:"
							  "\tThe last pattern object."
							  "Other Arguments:"
							  "\tMore information on rule construction is available in the ref{def.pattern.lr} explanation."))
				  (attr.return-types nil "A boolean and a list of violations."))))
   
   `("DEF.RECOGNIZER" ,(c.basic))
   `("DEF.TASK.RECOGNIZER" ,(c.basic))
   `("DEF.ROD.CLASS" ,(c.class))
   `("DEF.ROD.FILTER.CLASS" ,(c.class))
   `("DEF.ROD.FILTER.CONSTRUCTOR" ,(c.basic))
   `("DEF.ROD.FILTER.P.METHOD" ,(c.method))
   `("DEF.ROD.SORTER.CLASS" ,(c.class))
   `("DEF.ROD.SORTER.CONSTRUCTOR" ,(c.basic))
   `("DEF.ROSTER.DELEGATED.METHOD" ,(c.special))
   `("DEF.ROSTER.GF" ,(c.method))
   `("DEF.ROSTER.GF.CLASS" ,(c.sike.class))
   `("DEF.ROSTER.HF" ,(c.method))  
   `("DEF.ROSTER.HF.CLASS" ,(c.sike.class))
   `("DEF.ROSTER.OPER" ,(c.function))
   `("DEF.ROSTER.PATTERN.OPERATION" ,(c.function))
   `("DEF.ROSTER.STATE.FUNCTION" ,(c.function))
   `("DEF.ROSTER.STATE.METHOD" ,(c.method))
   `("DEF.ROSTER.STRATEGY" ,(c.basic))
   `("DEF.ROSTER.TR" ,(c.method))
   `("DEF.ROSTER.TR.CLASS" ,(c.sike.class))
   `("DEF-SAVE-IN-DB" ,(c.basic))
   `("DEF.SHORT.TERM.STATE.METHOD" ,(c.method `((attr.implem-notes nil "The first argument must be called 'state' and the macro will automaticaly execute its body within that state."))))
   `("DEF.ST.OPER" ,(c.function))
   `("DEF.FPLANNER.OPERATOR" ,(c.function))
   `("DEF.ONTIME.OPERATOR" ,(c.function))
   `("DEF.STRATEGY" ,(c.basic))
   `("DEF.STRATEGY.CLASS" ,(c.sike.class))
   `("DEF.STRATEGY.ITEM.BEHAVIOUR" ,(c.method))
   `("DEF.STRATEGY.ITEM.CLASS" ,(c.sike.class))
   `("DEF.STRAT.CLASS" ,(c.class))
   `("DEF.TASK.ACCESSOR" ,(c.function))
   `("DEF.TASK.RECOGNIZER" ,(c.basic))
   `("DEF.TOD.CLASS" ,(c.class))
   `("DEF.TR" ,(c.method))
   `("DEF.TR.CLASS" ,(c.sike.class))
   `("DEF.VOID.ACCESSOR.SLOT" ,(c.special))
   `("EXPORT"  ,(c.basic))
   `("MAKE.STRATEGY.ITEM" ,(c.basic))
   `("MAKE.STRATEGY" ,(c.basic))
   `("REMOVE.FROM.PROBLEM.LOCAL.FILES" ,(c.basic))
   `("SET.CURRENT.COMPANY" ,(c.basic))
   `("SET-VARIABLE-VALUES-KEYWORD" ,(c.basic))
   `("MAP.OBJECTS.TO.LABELS" ,(c.special))
   `("MAP.OBJECTS.TO.APP.TOPICS" ,(c.special))
   `("MAP.OBJECTS.TO.COMMON.TOPICS" ,(c.special))
   `("SET.SUPPLEMENTARY.VALIDATION.RULES" ,(c.basic))
   `("SET.CONSTRAINT.RESERVE.TASK.TYPE" ,(c.basic))
   
   `("DEF.STATISTIC" ,(c.basic))
   `("INSTALL.MENU.ITEM.SHORTCUTS" ,(c.basic))
   `("DEF.STATISTICS.MENU.ITEMS" ,(c.basic))
   `("DEF.STATISTICS.OCCUR.OUTPUT" ,(c.basic))
   `("DEF.STATISTICS.TIME.OUTPUT" ,(c.basic))
   `("DEF.VARIATION.SCHED.STATISTICS" ,(c.basic))
   `("DEF.VARIATION.STATISTICS.MENU.ITEMS" ,(c.basic))
   `("DEF.DELEGATED.METHOD" ,(c.basic))
   `("DEF.DELEGATED.METHOD.IF" ,(c.basic))
   `("ADD.TIMETABLE.SEQUENCING.METHOD" ,(c.basic))
   `("DEF.IPT.SEARCH.STRATEGY" ,(c.class))
   `("DEF.TEST.METHOD"         ,(c.method))
   `("DEF.RECOGNIZER.MANUAL.DUTIES.FOR.NOTIFICATIONS" ,(c.basic))
   
   ;; Emacs specific
   `("SC-DEFINE-SYSTEM" ,(c.basic))

   ))

;;;; Constructor addition

(defun add.constructors (spec)
  "Builds all specified constructors."
  (dolist (con.spec spec)
    (add.constructor (make-constructor :keyword (first con.spec)
				       :attributes
				       (append (common.attributes)
					       (second con.spec))))))

(add.constructors %all.constructor.spec%)
