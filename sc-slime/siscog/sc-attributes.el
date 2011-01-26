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
;;;	Defines all possible documentation attributes.
;;;
;;;     Notes: attributes are described in the quality technical document
;;;	"Documentation Attributes".
;;;
;;; History
;;;	Date		Author		Description
;;;	99/11/11	cpla	        Created.
;;;	00/03/21	cpla    	Added attributes attr.error and attr.unable.
;;;	00/06/08	Dario		Changed definitions
;;;					  COMMON.MANDATORY.ATTRIBUTES
;;;	01/01/05	Dario		Changed definitions
;;;					  ATTR.ERROR
;;;					  ATTR.UNABLE
;;;					  ATTR.REFS
;;;					  ATTR.EXAMPLE
;;;					  ATTR.CLASS-ATTRS
;;;					  ATTR.RETURN-TYPES
;;;					  ATTR.ARGS
;;;					  COMMON.OPTIONAL.ATTRIBUTES
;;;					  ATTR.REMARKS
;;;					  ATTR.VISIBILITY
;;;					  ATTR.IMPLEM-NOTES
;;;	01/09/21	Dario		Changed definitions
;;;					  ATTR.ERROR
;;;-----------------------------------------------------------------------------


;;;; Basic attribute definition

;;;-----------------------------------------------------------------------------
;;;ATTR.VISIBILITY
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{optional} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun attr.visibility (&optional optional default)
  (make.attribute "\\visibility" optional 10 (if default default "SI")))

;;;-----------------------------------------------------------------------------
;;;ATTR.REMARKS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{optional} is a <>.
;;;		
;;;		\arg{default} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun attr.remarks (&optional optional default)
   (make.attribute "\\remarks" optional 60 default))

;;;; Common attributes

;;;-----------------------------------------------------------------------------
;;;COMMON.MANDATORY.ATTRIBUTES
;;;Description
;;;	Return the list of common mandatory attributes.
;;;History
;;;	Date		Author		Description
;;;	00/06/08	Dario		Removed visibility.
;;;-----------------------------------------------------------------------------
(defun common.mandatory.attributes ()
  ;;(list (attr.visibility))
  )


;;;-----------------------------------------------------------------------------
;;;COMMON.OPTIONAL.ATTRIBUTES
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
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun common.optional.attributes ()
  (list (attr.remarks t)
	(attr.implem-notes t)
	(attr.refs t)
	(attr.example t)
	(attr.unable t)))


(defun common.attributes ()
  (append (common.mandatory.attributes)
	  (common.optional.attributes)))


;;;; Attribute definition

;;;-----------------------------------------------------------------------------
;;;ATTR.ARGS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{optional} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun attr.args (&optional optional default)
   (make.attribute "\\args" optional 20 default))

;;;-----------------------------------------------------------------------------
;;;ATTR.RETURN-TYPES
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{optional} is a <>.
;;;		
;;;		\arg{default} is a string or a list of strings or a function. 
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun attr.return-types (&optional optional default)
   (make.attribute "\\return-types" optional 30 default))

;;;-----------------------------------------------------------------------------
;;;ATTR.CLASS-ATTRS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{optional} is a <>.
;;;		
;;;		\arg{default} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun attr.class-attrs (&optional optional default)
   (make.attribute "\\class-attrs" optional 40 default))

;;;-----------------------------------------------------------------------------
;;;ATTR.EXAMPLE
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{optional} is a <>.
;;;		
;;;		\arg{default} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun attr.example (&optional optional default)
   (make.attribute "\\example" optional 50 default))

;;;-----------------------------------------------------------------------------
;;;ATTR.IMPLEM-NOTES
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{optional} is a <>.
;;;		
;;;		\arg{default} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun attr.implem-notes (&optional optional default)
  (make.attribute "\\implem-notes" optional 80 default))

;;;-----------------------------------------------------------------------------
;;;ATTR.REFS
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{optional} is a <>.
;;;		
;;;		\arg{default} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun attr.refs (&optional optional default)
  (make.attribute "\\refs" optional 70 default))

;;;-----------------------------------------------------------------------------
;;;ATTR.UNABLE
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{optional} is a <>.
;;;		
;;;		\arg{default} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		
;;;-----------------------------------------------------------------------------
(defun attr.unable (&optional optional default)
  (make.attribute "\\unable-to-document" optional 800 (if default default #'(lambda () (list (concatenate 'string (current-date-string) "\t" *default-author*))))))

;;;-----------------------------------------------------------------------------
;;;ATTR.ERROR
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{optional} is a <>.
;;;		
;;;		\arg{default} is a <>.
;;;		
;;;	\return-types
;;;		
;;;	
;;;History
;;;	Date		Author		Description
;;;	01/01/05	Dario		
;;;	01/09/21	Dario		Changed message.
;;;-----------------------------------------------------------------------------
(defun attr.error (&optional optional default)
  (make.attribute "\\error" optional 1 (if default default "Unknown constructor! Please, create a POA notifying the responsible of internal production support tools!")))








