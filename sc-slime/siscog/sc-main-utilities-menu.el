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
;;;	
;;; History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  SC-MAIN-UTILITIES-MENU-DEFINITIONS
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;SC-MAIN-UTILITIES-MENU-TITLE
;;;Description
;;;	Stores the title of the main utilities menu.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar sc-main-utilities-menu-title nil)

;;;-----------------------------------------------------------------------------
;;;SC-MAIN-UTILITIES-MENU-ITEMS
;;;Description
;;;	Stores the menu items of the main utilities menu.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar sc-main-utilities-menu-items nil)

;;;-----------------------------------------------------------------------------
;;;SC-MAIN-UTILITIES-MENU-DEFINITIONS
;;;Description
;;;	Returns the main utilities menu definitions.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of menu definitions where a menu definition is a list with
;;;		the syntax
;;;		(<symbol> <title> <menu items>)
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	09/02/10	J. P. Varandas	sc-crews-utilities-menu-items -> sc-product-utilities-menu-items
;;;-----------------------------------------------------------------------------
(defun sc-main-utilities-menu-definitions ()
  (list (list 'documentation  "Documentation"  mouse-documentation-menu-items)
	(list 'utilities      "Utilities"      sc-product-utilities-menu-items)))

;;;-----------------------------------------------------------------------------
;;;SC-SET-MAIN-UTILITIES-MENU-ITEMS
;;;Description
;;;	Sets the main utilities menu items.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{key} is a \emph{symbol} that identifies a menu definition
;;;		in \elem{sc-main-utilities-menu-definitions}
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-set-main-utilities-menu-items (key)
  (let ((switch-items nil))
    (dolist (menu (sc-main-utilities-menu-definitions))
      (if (eq key (first menu))
	  (progn
	    (setf sc-main-utilities-menu-items (third menu))
	    (setf sc-main-utilities-menu-title (second menu)))
	  (push (sc-make-menu-item  (format "Switch to %s" (second menu))
				    `(sc-switch-utilities-menu ',(first menu)))
		switch-items)))
    (setf sc-main-utilities-menu-items (append sc-main-utilities-menu-items '("") switch-items))))

;;;-----------------------------------------------------------------------------
;;;SC-SWITCH-UTILITIES-MENU
;;;Description
;;;	Switches the main utilities menu items to the menu definition identified
;;;	by \arg{key}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{key} is a \emph{symbol} that identifies a menu definition
;;;		in \elem{sc-main-utilities-menu-definitions}
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-switch-utilities-menu (key)
  (sc-set-main-utilities-menu-items key)
  (sc-execute-main-utilities last-input-event))


;;;-----------------------------------------------------------------------------
;;;SC-EXECUTE-MAIN-UTILITIES
;;;Description
;;;	Popups the main utilities menu. Executes the selected option.
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
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-execute-main-utilities (arg)
  (interactive "e")
  (let ((choice (sc-popup-menu arg sc-main-utilities-menu-title sc-main-utilities-menu-items)))
    (if choice
	(sc-execute-menu-item arg choice))))


;;;-----------------------------------------------------------------------------
;;;SC-SET-MAIN-UTILITIES-MENU-ITEMS
;;;Description
;;;	Sets the default menu items for the main uitilities menu
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(sc-set-main-utilities-menu-items 'utilities)

;;;-----------------------------------------------------------------------------
;;;[C-M-DOWN-MOUSE-1]
;;;Description
;;;	Assigns the keyboard and mouse keys for the main uitilities menu
;;;		
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(global-set-key [C-M-down-mouse-1] 'sc-execute-main-utilities)

