;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 1995, SISCOG - Sistemas Cognitivos Lda.
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
;;;	It supplies a utilities menu available throught Control-Alt-mouse-left with the following options:
;;;	Set package		Shows a menu with packages that can be set
;;;	Set definition package  Sets the Lisp buffer package to the package of the definition found by
;;;				searching the "in-package" form that precedes the defintion.
;;;				Useful for compiling definitions in a patch file.
;;;	Find in dictionaries	Search for a given string in all the dictionaries files. 
;;;				If only one file is found it opens that file in the place of the string.
;;;				If more than one files was found it shows a menu for the user to choose
;;;				Which file he wants to open.
;;;	Insert in dictionaries	Search for a given string in all the dictionaries files. 
;;;				If no file was found, it inserts the keyword in a dictionary file
;;;				according to a given criterium. If the criterium is not fulfilled 
;;;				it ask the user to choose which file he wants to insert the keyword.
;;;	Insert all keywords	Runs the current buffer and for all found strings, that does not 
;;;				have the til behind, checks if the string is already in a dictionary.
;;;				If not, it asks if the user wants to insert it in a dictionary.
;;;				When the string is in a dictionary or was inserted it add the til before the string.
;;;				All the rejected string are stored to avoid ask twince about the same string.
;;;	Reset ignored keywords	Resets the list of rejected strings.
;;;	Insert comment tabs	Inserts the right number of tabs when making documentation on a definition header.
;;;				If the user is writing the description it inserts "\n ;;;<tab>".
;;;				If the user is writing the history it inserts "\n;;;<tab><tab><tab><tab><tab>".
;;;	Insert in system	Inserts a selected string at the beginning of the respective system.lisp file 
;;;				to avoid forget exporting it.
;;;	Find In Other Window	With two open buffers it search on the other buffer some similar definition
;;;				to the one where the cursor is. Usefull when editing the source and the original files.
;;;	Kill Line With Pattern	Erases all the lines that contains a given pattern (eg, erase all the lines with ";;;".
;;;	PC -> UNIX		Removes the character 13
;;;	PC -> UNIX (Reopen)	Removes the character 13, by closing the buffer and reopening it in binary mode
;;;	Set all file as new	Sets a file as NEW inserting the file header and headers in all definitions
;;;	Install Crews Image	Presents a sequence of two menus and a question that helps the user 
;;;				to build the command for the functionality M-x install-crews-image
;;;	Digest Modifications	Reads the answer mail files about the instalation of modifications and patches
;;;				and digests them into a text that can be used to fill the Action window of a POA
;;;-----------------------------------------------------------------------------
;;; History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;					Changed definitions
;;;					  *SC-SYSTEM-OPERATIONS-MENU-ITEMS*
;;;					  *SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS*
;;;					Added definitions
;;;					  GET-ALL-SUB-SYSTEMS
;;;					  SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS
;;;	03/07/28	A. Frazao	Changed definitions
;;;					  PROCESS-SYSTEM
;;;					  GET-VERSIONS-HEADER
;;;	03/09/18	A. Frazao	Changed definitions
;;;					  PROCESS-SUB-SYSTEM
;;;					  GET-ALL-SUB-SYSTEMS
;;;	04/01/16	A. Frazao	Changed definitions
;;;					  *SC-PACKAGES-LIST*
;;;	04/07/09	Dario		Changed definitions
;;;					  SC-CREWS-UTILITIES-MENU-ITEMS
;;;					Added definitions
;;;					  SC-SET-DEFINITION-PACKAGE
;;;					  REMOVE-CHARS
;;;	04/07/09	Dario		Updated file header documentation
;;;	05/10/12	A. Frazao	Changed definitions
;;;					  GET-SUB-SYSTEM-OF
;;;					  GET-SYSTEM-OF
;;;	05/10/13	J. P. Varandas	Changed definitions
;;;					  PROCESS-SYSTEM
;;;					  SC-CREWS-DICTIONARY-DIRS
;;;					  DIGEST-MODIFICATIONS
;;;					  INSERT-COMMENT-TABS
;;;					Deleted definitions
;;;					  ADD-RESP-TO-LIST
;;;	05/10/19	J. P. Varandas	Changed definitions
;;;					  DIGEST-MODIFICATIONS
;;;					Added definitions
;;;					  *DIGEST-MAIL-NAME*
;;;	05/10/19	A. Frazao	Added definitions
;;;					  CHANGE-UNIX2PC-REOPEN
;;;					Changed definitions
;;;					  SC-CREWS-UTILITIES-MENU-ITEMS
;;;	05/10/19	J. P. Varandas	Changed definitions
;;;					  DIGEST-MODIFICATIONS
;;;	06/04/06	J. P. Varandas	Changed definitions
;;;					  INSERT-IN-SYSTEM
;;;					  SC-CREWS-DICTIONARY-DIRS
;;;					  CHECK-ON-DIC
;;;					  GET-DIC-FILE
;;;					Deleted definitions
;;;					  *SC-CREWS-DICTIONARY-DIRS*
;;;	06/05/19	A. Frazao	Changed definitions
;;;					  SC-CREWS-UTILITIES-MENU-ITEMS
;;;					  SC-SET-PACKAGE
;;;	08/07/11	A. Frazao	Changed definitions
;;;					  COMPANY-SYSTEMS-MENU
;;;					  SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS
;;;	08/11/03	A. Frazao	Changed definitions
;;;					  GET-SYSTEM-OF
;;;	09/02/04	J. P. Varandas	Changed definitions
;;;					  COMPANY-SYSTEMS-MENU
;;;					  GET-SUB-SYSTEM-OF
;;;					  GET-SYSTEM-OF
;;;					  PROCESS-SUB-SYSTEM
;;;					  GET-ALL-SUB-SYSTEMS
;;;					Added definitions
;;;					  SC-PRODUCT-MENU-ITEMS
;;;					  *SC-PRODUCT-MENU-ITEMS*
;;;					Deleted definitions
;;;					  *SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS*
;;;					Updated definitions
;;;					  SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS -> SC-COMPANY-SYSTEMS-MENU-ITEMS
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  DIGEST-MODIFICATIONS
;;;					  CHECK-ON-DIC
;;;					  INSERT-IN-DIC
;;;					  FIND-IN-DIC
;;;					  SC-PRODUCT-DICTIONARY-DIRS
;;;					  SC-PRODUCT-UTILITIES-MENU-ITEMS
;;;					  SC-COMPANY-SYSTEMS-MENU-ITEMS
;;;					  COMPANY-SYSTEMS-MENU
;;;					  SC-CREWS-UTILITIES-MENU-ITEMS
;;;					  GET-DIC-FILE
;;;					  SC-CREWS-DICTIONARY-DIRS
;;;					Updated definitions
;;;					  SC-CREWS-COMPANY-SYSTEMS-MENU-ITEMS -> SC-COMPANY-SYSTEMS-MENU-ITEMS
;;;	09/02/18	J. P. Varandas	Changed definitions
;;;					  COMPANY-SYSTEMS-MENU
;;;	09/02/19	Rui Patrocinio	Corrected characters problems in file
;;;	09/03/06	Rui Patrocinio	Corrected erroneous change to file
;;;	09/03/19	J. P. Varandas	Changed definitions
;;;					  GET-DIC-FILE
;;;					Deleted definitions
;;;					  GET-DIC-FILE-AUX
;;;	09/09/23	P. Madeira	Changed definitions
;;;					  INSERT-KEYWORDS
;;;					  INSERT-COMMENT-TABS
;;;					  SC-KILL-LINE-WITH-PATTERN
;;;					  DIGEST-MODIFICATIONS
;;;					  GET-SYSTEM-OF
;;;					  GET-VERSIONS-HEADER
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;SC-CREWS-DICTIONARY-DIRS
;;;Description
;;;	Returns the list of dictionary directories.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of \emph{string}
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	05/10/13	J. P. Varandas	Check if the created folder name exists as a folder
;;;	06/04/06	J. P. Varandas	Collect the dictionaries directories of CREWS and 
;;;					adds only the respective CREWS dictionaries directory
;;;	09/02/10	J. P. Varandas	Use function 'sc-current-product-env-name' 
;;;					to generalise to several products
;;;-----------------------------------------------------------------------------
(defun sc-product-dictionary-dirs ()
  (let ((filename (buffer-file-name (current-buffer)))
	(basic-dirs (list (format "%s/dictionaries" (getenv (sc-current-product-env-name "VERSION_DIR")))
			  (format "%s/dictionaries" (getenv "SISCOG_VERSION_DIR")))))
    
    (if (or (string-match (getenv (sc-current-product-env-name "VERSION_DIR")) filename)
	    (string-match (getenv "SISCOG_VERSION_DIR") filename))
	basic-dirs
	(let ((dic-dirs basic-dirs))
	  (dolist (system *sc-all-systems*)
	    (dolist (version-dir (sc-system-src-version-dirs system))
	      (let ((dir (if (consp version-dir)
			     (eval version-dir)
			     version-dir)))
		
		(when (and dir (string-match dir filename))
		  (setf dic-dirs (cons (format "%s/dictionaries" dir) dic-dirs))))))
	  dic-dirs))))


;;;-----------------------------------------------------------------------------
;;;*LAST-FIND-IN-DIC-ITEMS*
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *last-find-in-dic-items* nil)

;;;-----------------------------------------------------------------------------
;;;*LAST-FIND-IN-DIC-PATTERN*
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *last-find-in-dic-pattern* "")

;;;-----------------------------------------------------------------------------
;;;SEARCH-FORWARD-DIC
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun search-forward-dic (pattern)
  (let ((found nil)
	(not-found t))
    (while (and not-found (not found))
      (if (search-forward pattern nil t)
	(when (save-excursion 
		(progn
		  (beginning-of-line)
		  (not (eq (char-after (point)) 59))))
	  (setf found t))
	(setf not-found nil)))
    found))

;;;-----------------------------------------------------------------------------
;;;FIND-IN-DIC
;;;Description
;;;	Searches for a keyword in the dictionaries.
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
;;;	99/08/31	J. P. Varandas	Created
;;;	99/09/10	J. P. Varandas	If no files were find makes a beep
;;;					Corrects the question.
;;;	00/01/31	J. P. Varandas	Erases the variable '*last-find-in-dic-items*'
;;;	03/07/28	A. Frazao	Calls function sc-crews-dictionary-dirs
;;;	09/02/10	J. P. Varandas	sc-crews-dictionary-dirs -> sc-product-dictionary-dirs
;;;-----------------------------------------------------------------------------
(defun find-in-dic (arg)
  (interactive "e")
  (let ((*current-x-arg* arg))
    (unless (and *last-find-in-dic-items*
		 (not (x-beep-confirm "Do you want to perform a new search?")))
      (setf *last-find-in-dic-pattern* (read-string "Keyword: "))
      (setf *last-find-in-dic-items* nil)
      (let ((files nil))
	(dolist (dir (sc-product-dictionary-dirs))
	  (setf files (append files (find-in-dic-aux *last-find-in-dic-pattern* dir t))))
	(if (= (length files) 1)
	    (progn
	      (switch-to-buffer-other-window (find-file-noselect (car files)))
	      (search-forward-dic *last-find-in-dic-pattern*))
	    (let ((items nil))
	      (dolist (file files)
		(setf items (cons (cons file file) items)))
	      (setf *last-find-in-dic-items* items)))))
    (if *last-find-in-dic-items*
	(let ((choice (sc-popup-menu arg "Select" *last-find-in-dic-items*)))
	  (when choice
	    (switch-to-buffer-other-window (find-file-noselect choice))
	    (search-forward-dic *last-find-in-dic-pattern*)))
	(beep))))


;;;-----------------------------------------------------------------------------
;;;FIND-IN-DIC-AUX
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun find-in-dic-aux (pattern dir &optional case-search)
  (let ((names (directory-files dir))
	(files nil))
    (dolist (name names)
      (unless (char-equal (elt name 0) 46)
	(let ((new-dir (format "%s/%s" dir name)))
	  (if (file-directory-p new-dir)
	      (find-in-dic-aux pattern new-dir)
	    (when (string-match "\\.dic$" name)
	      (let ((old-buf (current-buffer))
		    (buf (find-file-noselect new-dir)))
		(switch-to-buffer buf)
		(beginning-of-buffer)
		(let ((case-fold-search case-search))
		  (when (search-forward-dic pattern)
		    (setf files (cons new-dir files))))
		(kill-buffer buf)
		(switch-to-buffer old-buf)))))))
    files))


;;;-----------------------------------------------------------------------------
;;;INSERT-IN-DIC
;;;Description
;;;	Inserts a keyword in a dictionary.
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
;;;	99/08/31	J. P. Varandas	Created
;;;	03/07/28	A. Frazao	Calls function sc-crews-dictionary-dirs
;;;	09/02/10	J. P. Varandas	sc-crews-dictionary-dirs -> sc-product-dictionary-dirs
;;;-----------------------------------------------------------------------------
(defun insert-in-dic (arg)
  (interactive "e")
  (let ((pattern (read-string "Keyword: "))
	(found nil))
    (dolist (dir (sc-product-dictionary-dirs))
      (when (find-in-dic-aux pattern dir)
	(setf found t)
	(return)))
    (unless found
      (let ((file-list (get-dic-file)))
	(let ((filename (car file-list))
	      (dir (cadr file-list)))
	  (if (file-exists-p filename)
	      (insert-in-dic-aux pattern filename)
	      (insert-in-dic-aux pattern (choose-dic-file dir filename arg))))))))


;;;-----------------------------------------------------------------------------
;;;INSERT-IN-DIC-AUX
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun insert-in-dic-aux (pattern filename)
  (when filename
    (let ((old-buf (current-buffer))
	  (buf     (find-file-noselect filename)))
      (switch-to-buffer buf)
      (unless (file-exists-p filename)
	(add-mod-file-header)
	(add-header-change-entry)
	(end-of-buffer)
	(insert "(in-package :USER)" 10 10)
	(insert "(:ENGLISH :PORTUGUES )" 10))
      (end-of-buffer)
      (when (char-equal (elt pattern 0) 34)
	(setf pattern (substring pattern 1)))
      (when (char-equal (elt pattern (- (length pattern) 1)) 34)
	(setf pattern (substring pattern 0 (- (length pattern) 1))))
      (insert 10 126 34 pattern 34 10)
      (insert 32 32 34 pattern 34 10)
      (insert 32 32 34 "Translation to :PORTUGUES" 34 10)
      (save-excursion (set-definition-modified pattern "Added keywords"))
      (set-mod-file-changed-if)
      (save-buffer)
      (kill-buffer buf)
      (switch-to-buffer old-buf))))


;;;-----------------------------------------------------------------------------
;;;GET-DIC-FILE
;;;Description
;;;	Searches for a dictionary file where the keywords referenced in the
;;;	current file should be saved.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{string} or \emph{nil} if it did not find a dictionary
;;;		file that matches the current file.
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	00/01/31	J. P. Varandas	Diferenciates also the folder 'models'
;;;	01/09/27	A. Vasconcelos	Updated for CREWS_ML and CREWS_DSB.
;;;	02/01/13	Fausto		Updated for CREWS_STOG.
;;;	02/01/31	Pedro Matos	Updated for CREWS_VR.
;;;	02/05/21	Fausto		Updated for CREWS_DSB.
;;;	02/07/18	Carlos Ribeiro	Updated for CREWS_BRISA.
;;;	02/12/09	A. Frazao	Updated for CREWS_SISCOG
;;;	03/07/28	A. Frazao	Uses *SC-ALL-SYSTEMS*
;;;	06/04/06	J. P. Varandas	When handling the situation of "CREWS_VERSION_DIR" 
;;;					it was not returning any value
;;;					Handle the special case of middleware
;;;	09/02/10	J. P. Varandas	Use function 'sc-current-product-env-name' 
;;;					to generalise to several products
;;;	09/03/19	J. P. Varandas	Removed the special cases and uses only the first directory name
;;;					Merged with function 'get-dic-file-aux'
;;;-----------------------------------------------------------------------------
(defun get-dic-file ()
  (let ((filename (buffer-file-name (current-buffer))))
    (block exit
      (dolist (system *sc-all-systems*)
	(dolist (version-dir (sc-system-src-version-dirs system))
	  (let ((dir (if (consp version-dir)
			 (eval version-dir)
			 version-dir)))
	    (if (string-equal (getenv (sc-current-product-env-name "DIR")) dir)
		(let ((version-dir (getenv (sc-current-product-env-name "VERSION_DIR"))))
		  (when (string-match version-dir filename)
		    (let ((file (substring (file-name-directory filename) (+ (match-end 0) 1)
					   (- (length (file-name-directory filename)) 1))))
		    
		      (when (string-match "/" file)
			(setf file (substring file 0 (- (match-end 0) 1))))

		      (return-from exit (list (format "%s/dictionaries/%s.dic" version-dir file)
					      (format "%s/dictionaries" version-dir))))))

		(when (string-match dir filename)
		  (let ((file (substring (file-name-directory filename) (+ (match-end 0) 1)
					 (- (length (file-name-directory filename)) 1))))
		    (when (string-match "/" file)
		      (setf file (substring file 0 (- (match-end 0) 1))))
		    (return-from exit (list (format "%s/dictionaries/%s.dic" dir file) 
					    (format "%s/dictionaries" dir))))))))))))


;;;-----------------------------------------------------------------------------
;;;CHOOSE-DIC-FILE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	00/01/31	J. P. Varandas	Returns the first element returned by 'sc-popup-menu'
;;;-----------------------------------------------------------------------------
(defun choose-dic-file (dir filename arg)
  (first (sc-popup-menu arg "Select" (cons (sc-make-menu-item (format "Create %s" filename) (list filename 0)) (files-items dir)))))


;;;-----------------------------------------------------------------------------
;;;*IGNORED-KEYWORDS*
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *ignored-keywords* nil)


;;;-----------------------------------------------------------------------------
;;;RESET-KEYWORDS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun reset-keywords ()
  (interactive)
  (setf *ignored-keywords* nil))


;;;-----------------------------------------------------------------------------
;;;INSERT-KEYWORDS
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/09/23	P. Madeira	`next-line' -> `forward-line' (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun insert-keywords ()
  (interactive)
  (let (start-region end-region)
    (beginning-of-buffer)
    (while (char-after (point))
      (cond ((= (char-after (point)) 59)
	     (forward-line 1)
	     (beginning-of-line))
	    ((or (= (char-after (point)) 126)
		 (and (= (char-after (point)) 35)
		      (or (= (char-after (+ 1 (point))) 112)
			  (= (char-after (+ 1 (point))) 80))))
	     (while (and (char-after (point))
			 (not (= (char-after (point)) 34)))
	       (forward-char 1))
	     (forward-char 1)
	     (while (and (char-after (point))
			 (not (= (char-after (point)) 34)))
	       (forward-char 1))
	     (forward-char 2))
	    ((= (char-after (point)) 34)
	     (setf start-region (point))
	     (forward-char 1)
	     (while (and (char-after (point))
			 (not (= (char-after (point)) 34)))
	       (forward-char 1))
	     (forward-char 1)
	     (setf end-region (point))
	     (let ((text (save-excursion (buffer-substring start-region end-region))))
	       (unless (sc-member text *ignored-keywords*)
		 (when (save-excursion
			 (check-on-dic text))
		   
		   (goto-char start-region)
		   (insert 126)
		   (goto-char end-region)
		   (forward-char 1))))
	     (forward-char 1))
	    (t
	     (forward-char 1))))))


;;;-----------------------------------------------------------------------------
;;;SC-MEMBER
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun sc-member (text lista)
  (let ((found nil))
    (dolist (elem lista)
      (when (string-equal text elem)
	(setf found t)
	(return)))
    found))


;;;-----------------------------------------------------------------------------
;;;MY-BEEP-CONFIRM
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun my-beep-confirm (str)
  (beep)
  (y-or-n-p str))


;;;-----------------------------------------------------------------------------
;;;CHECK-ON-DIC
;;;Description
;;;	Searches a string in the dictionaries. If not found, inserts it in the
;;;	corresponding dictionary.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{pattern} is a \emph{string}
;;;		
;;;	\return-types
;;;		A \emph{boolean}
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	03/07/28	A. Frazao	Calls function sc-crews-dictionary-dirs
;;;	06/04/06	J. P. Varandas	Gives a message when is not possible to 
;;;					guess the correspondent dictionary file.
;;;	09/02/10	J. P. Varandas	sc-crews-dictionary-dirs -> sc-product-dictionary-dirs
;;;-----------------------------------------------------------------------------
(defun check-on-dic (pattern)
  (unless (string-equal "\"\"" pattern)
    (let ((found nil))
      (dolist (dir (sc-product-dictionary-dirs))
	(when (find-in-dic-aux pattern dir)
	  (setf found t)
	  (return)))
      (if found
	  t
	  (if (my-beep-confirm pattern)
	      (let ((file-list (get-dic-file)))
		(if file-list
		    (let ((filename (car file-list))
			  (dir (cadr file-list)))
		      (if (file-exists-p filename)
			  (insert-in-dic-aux pattern filename)
			  (insert-in-dic-aux pattern (choose-dic-file dir filename t)))
		      t)
		    (progn
		      (message "Not able to guess the correspondent dictionary file")
		      nil)))
	      (progn
		(if *ignored-keywords*
		    (setf *ignored-keywords* (cons pattern *ignored-keywords*))
		    (setf *ignored-keywords* (list pattern)))
		nil))))))

  
;;;-----------------------------------------------------------------------------
;;;INSERT-COMMENT-TABS
;;;Description
;;;	Inserts the charaters "Newline", 3 "Semicolons" and a number of tabs 
;;;	according with context.
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
;;;	99/08/31	J. P. Varandas	Created
;;;	05/10/13	J. P. Varandas	Handle more situations
;;;	09/09/23	P. Madeira	`previous-line' -> `forward-line' (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun insert-comment-tabs ()
  (interactive)
  (let ((comments ";;;")
	(tabs 0))
    (save-excursion
      (beginning-of-line)
      (let ((continue t))
	(while continue
	  (cond ((looking-at "Description:")
		 (setq tabs 2)
		 (setq continue nil)
		 (setq comments ""))
		((save-excursion
		   (forward-char 5)
		   (or (looking-at "return-types")(looking-at "args")))
		 (setq tabs 2)
		 (setq continue nil))
		((or (looking-at ";;;Description") (looking-at ";;; Description"))
		 (setq tabs 1)
		 (setq continue nil))
		((or (looking-at ";;;History") (looking-at ";;; History"))
		 (setq tabs 5)
		 (setq continue nil))
		(t
		 (forward-line -1))))))
    (insert 10 comments)
    (dotimes (i tabs)
      (insert 9))))


;;;-----------------------------------------------------------------------------
;;;INSERT-IN-SYSTEM
;;;Description
;;;	Inserts the selected symbol on the top of the respective system file
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
;;;	99/08/31	J. P. Varandas	Created
;;;	06/04/06	J. P. Varandas	Corrects the error message
;;;-----------------------------------------------------------------------------
(defun insert-in-system ()
  (interactive)
  (let* ((dir (file-name-directory (buffer-file-name (current-buffer))))
	 (filename (format "%s/system.lisp" dir))
	 (string (buffer-substring (region-beginning) (region-end))))
    (cond ((file-exists-p filename)
	   (let ((old-buf (current-buffer))
		 (buf     (find-file-noselect filename)))
	     (switch-to-buffer buf)
	     (beginning-of-buffer)
	     (insert 34 (upcase string) 34 10)
	     (save-buffer)
	     (kill-buffer buf)
	     (switch-to-buffer old-buf)))
	  (t
	   (beep)
	   (message (format "File %s does not exist!" filename))))))


;;;-----------------------------------------------------------------------------
;;;GET-SEARCH-DEFINITION
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun get-search-definition ()
  (let (start end def string current-point)
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
			    (string-equal def "unless")
			    (string-equal def "with-stable-string-output-stream")
			    (string-equal def "eval-when"))
			(forward-sexp)
			(setq current-point (point)))
		       (t
			(cond ((or (string-equal def "defmethod")
				   (string-equal def "def.tr.class")
				   (string-equal def "def.tr")
				   (string-equal def "def.aux")
				   (string-equal def "def.lr"))
			       )
			      ((or (equal def "def-foreign-callable")
				   (equal def "def-foreign-function"))
			       (next-non-space)
			       (forward-char 1)
			       (setq string (buffer-substring current-point (get-next-name-point))))
			      ((equal def "export")
			       (setq string (buffer-substring current-point (point))))
			      (t (setq string (buffer-substring current-point (get-next-name-point)))))
			(setq current-point end))))
	       string)
	     (progn
	       (goto-char start)
	       (forward-char 1)
	       (setq string (buffer-substring start (get-next-name-point))))))))


;;;-----------------------------------------------------------------------------
;;;FIND-IN-OTHER-WINDOW
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun find-in-other-window ()
  (interactive)
  (let ((str1 nil)
	(str2 nil)
	(other-file nil)
	(buffer (current-buffer))
	(filename (buffer-file-name (current-buffer))))
    (save-excursion
      (beginning-of-definition)
      (let ((start (point)))
	(while (not (= (char-after (point)) 10))
	  (forward-char 1))
	(setq str1 (buffer-substring start (- (point) 1)))))
    (save-excursion
      (setq str2 (get-definition-id)))
    (block nil
      (other-window 1)
      (setf other-file (buffer-file-name (current-buffer)))
      (unless (string-equal filename other-file)
	(beginning-of-buffer)
	(when (or (search-forward str1 nil t)
		  (search-forward str2 nil t))
	  (return))))))


;;;-----------------------------------------------------------------------------
;;;SC-KILL-LINE-WITH-PATTERN
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/09/23	P. Madeira	Use `sc-kill-line'
;;;-----------------------------------------------------------------------------
(defun sc-kill-line-with-pattern ()
  (interactive)
  (let ((pattern (read-string "Pattern: ")))
    (beginning-of-buffer)
    (while (search-forward pattern nil t)
      (beginning-of-line)
      (sc-kill-line)
      (sc-kill-line))))


;;;-----------------------------------------------------------------------------
;;;CHANGE-PC2UNIX
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun change-pc2unix ()
  (save-excursion
    (beginning-of-buffer)
    (remove-cr)))


;;;-----------------------------------------------------------------------------
;;;CHANGE-PC2UNIX-REOPEN
;;;Description
;;;	Reopens the file in binary mode, removes the carriage return and saves
;;;	the file.
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
;;;	99/09/28	A. Frazao	Created
;;;	00/07/26	J. P. Varandas	If the buffer is modified saves it first 
;;;					before kill it.
;;;-----------------------------------------------------------------------------
(defun change-pc2unix-reopen ()
  (let* ((point (point))
	 (buf (current-buffer))
	 (file (buffer-file-name buf)))
    (if (buffer-modified-p) (save-buffer buf))
    (kill-buffer buf)
    (setq buf (find-file-binary file))
    (remove-cr)
    (save-buffer buf)
    (if (<= point (point-max))
	(goto-char point)
	(end-of-buffer))))


;;;-----------------------------------------------------------------------------
;;;CHANGE-UNIX2PC-REOPEN
;;;Description
;;;	Reopens the file in binary mode, removes the carriage return and saves
;;;	the file.
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
;;;	05/10/19	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun change-unix2pc-reopen ()
  (let* ((point (point))
	 (buf (current-buffer))
	 (file (buffer-file-name buf)))
    (if (buffer-modified-p) (save-buffer buf))
    (kill-buffer buf)
    ;;(setq buf (find-file-binary file))
    (setq buf (find-file-binary file))
    (add-cr)
    (save-buffer buf)
    (kill-buffer buf)
    (setq buf (find-file-text file))
    (if (<= point (point-max))
	(goto-char point)
	(end-of-buffer))))


;;;-----------------------------------------------------------------------------
;;;SET-FILE-NEW
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun set-file-new ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (add-mod-file-header)
    (add-header-change-entry)
    (beginning-of-buffer)
    (while (search-forward "(def" nil t)
      (next-space)
      (forward-char 2)
      (set-definition-new))))


;;;-----------------------------------------------------------------------------
;;;*DIGEST-MAIL-NAME*
;;;Description
;;;	Indicates the pathname where the user stores the instalation 
;;;	confirmation messages.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	05/10/19	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *digest-mail-name* nil)


;;;-----------------------------------------------------------------------------
;;;DIGEST-MODIFICATIONS
;;;Description
;;;	Reads the answer mail files about the instalation of modifications and 
;;;	patches and digests them into a text that can be used to fill the Action 
;;;	window of a POA
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	01/11/13	J. P. Varandas	Created
;;;	05/10/13	J. P. Varandas	Changed the output by reducing each entry to a single line.
;;;	05/10/19	J. P. Varandas	Reads the file indicated by the parameter *digest-mail-name*
;;;	05/10/19	J. P. Varandas	Handles the way patchnames are writen with the new CRI
;;;	09/02/10	J. P. Varandas	Removed case od '*windows-cri*'
;;;	09/09/23	P. Madeira	`Reference' -> `reference'
;;;-----------------------------------------------------------------------------
(defun digest-modifications ()
  (interactive)
  (if (file-exists-p *digest-mail-name*)
      (let ((buffer (find-file-noselect *digest-mail-name*))
	    (modifs nil)
	    (directory nil)
	    (type nil)
	    (number nil)
	    (reference nil)
	    (system nil)
	    (install nil)
	    (start nil)
	    (end nil))
	(set-buffer buffer)
	(beginning-of-buffer)
	(while (search-forward "Installation directory:" nil t)
	  (end-of-line)
	  (setf end (point))
	  (while (not (= (char-after (point)) 92))
	    (backward-char 1))
	  (setf end (point))
	  (backward-char 1)
	  (while (not (= (char-after (point)) 92))
	    (backward-char 1))
	  (setf start (point))
	  (setf directory (format "%s" (buffer-substring (+ start 1) end)))
	  
	  (search-forward ": " nil t)
	  (setf start (point))
	  (end-of-line)
	  (setf end (point))
	  (setf number (buffer-substring start end))
	  (beginning-of-line)
	  (if (looking-at "Patch file:")
	      (setf type "")
	      (setf type "Mod. "))
	  
	  (search-forward "System:	" nil t)
	  (next-non-space)
	  (setf start (point))
	  (end-of-line)
	  (setf end (point))
	  (setf system (buffer-substring start end))
	  
	  (search-forward "Reference:	" nil t)
	  (setf start (point))
	  (end-of-line)
	  (setf end (point))
	  (setf reference (buffer-substring start end))
	  
	  (search-forward "Installation:	" nil t)
	  (next-non-space)
	  (setf start (point))
	  (while (not (= (char-after (point)) 9))
	    (forward-char 1))
	  (setf end (point))
	  (setf install (buffer-substring start end))
	  
	  (let ((ref (member* reference modifs :key 'car :test 'string-equal)))
	    (if ref
		(setf (car (cdr (cdr (cdr (car ref))))) (cons (list directory type number) (car (cdr (cdr (cdr (car ref)))))))
		(if modifs 
		    (setf modifs (cons (list reference system install (list (list directory type number))) modifs))
		    (setf modifs (list (list reference system install (list (list directory type number)))))))))
	
	
	(let ((pms-buf (get-buffer "*PMS-POAS*")))
	  (when pms-buf
	    (kill-buffer pms-buf))
	  (setq pms-buf (get-buffer-create "*PMS-POAS*"))
	  (set-buffer pms-buf)
	  (erase-buffer)
	  (dolist (modif (reverse modifs))
	    (insert (car modif) 10 (car (cdr modif)) 10 "20" (car (cdr (cdr modif))) 10)
	    (dolist (elem (reverse (car (cdr (cdr (cdr modif))))))
	      (insert (car (cdr elem)) (car (cdr (cdr elem))) " of " (car elem) ";"10))
	    (insert 10))
	  (switch-to-buffer-other-window pms-buf))
	)
      (beep-message (format "File %s does not exist" *digest-mail-name*)))
  )


;;;-----------------------------------------------------------------------------
;;;*SC-PRODUCT-MENU-ITEMS*
;;;Description
;;;	Stores the menu items with all rplan products
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *sc-product-menu-items* nil)


;;;-----------------------------------------------------------------------------
;;;SC-PRODUCT-MENU-ITEMS
;;;Description
;;;	Compute the rplan products menu items
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A list of \elem{sc-menu-item}s
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun sc-product-menu-items ()
  (unless *sc-product-menu-items*
    (dolist (product *sc-all-products*)
      (when (sc-product-rplan? product)
	(push (sc-make-menu-item (sc-product-name product) product) *sc-product-menu-items*)))
    (setf *sc-product-menu-items* (reverse *sc-product-menu-items*)))
  *sc-product-menu-items*)


;;;-----------------------------------------------------------------------------
;;;SC-COMPANY-SYSTEMS-MENU-ITEMS
;;;Description
;;;	Returns the menu items of the CREWS applications for each system.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{product} is a \elem{sc-product}
;;;		
;;;	\return-types
;;;		A list of menu items.
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/04	J. P. Varandas	Created
;;;	09/02/10	J. P. Varandas	'*sc-current-product*' is a \elem{sc-product}
;;;					Changed name sc-crews-company-systems-menu-items -> sc-company-systems-menu-items
;;;-----------------------------------------------------------------------------
(defun sc-company-systems-menu-items (product)
  (let ((sc-company-systems-menu-items nil))
    (dolist (system (sc-product-systems product))
      (when (sc-system-company-name system)
	(let ((items nil))
	  (dolist (sc-appl (sc-system-applications system))
	    (push (sc-make-menu-item (sc-application-external-id sc-appl)
				     (list (sc-system-company-name system) (sc-application-name sc-appl)))
		  items))
	  (push (cons (upcase (sc-system-company-name system)) (reverse items)) sc-company-systems-menu-items))))
    (reverse sc-company-systems-menu-items)))


;;;-----------------------------------------------------------------------------
;;;*SC-SYSTEM-OPERATIONS-MENU-ITEMS*
;;;Description
;;;	Is a list of menu items with the options for application images.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/09/10	J. P. Varandas	Created
;;;	03/07/28	A. Frazao	Changed name
;;;-----------------------------------------------------------------------------
(defvar *sc-system-operations-menu-items*
    (list (sc-make-menu-item "Compile Code" "-c")
	  (sc-make-menu-item "Development Image" "-i")
	  (sc-make-menu-item "Distribution Image" "-d")
	  (sc-make-menu-item "Compile + Development" "-ci")
	  (sc-make-menu-item "Compile + Development + Distribution" "-cid")
	  (sc-make-menu-item "Development + Distribution" "-id")
	  (sc-make-menu-item "Compile + Distribution" "-cd")))


;;;-----------------------------------------------------------------------------
;;;COMPANY-SYSTEMS-MENU
;;;Description
;;;	Processes an image for a CREWS application of a specific system.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{arg} is a Emacs event.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/09/10	J. P. Varandas	Created
;;;	03/07/28	A. Frazao	Computes the value of sc-crews-company-systems
;;;	08/07/11	A. Frazao	Updates for new compilation
;;;	09/02/10	J. P. Varandas	Chooses the product first
;;;					Remove setting of 'install-crews-image-default'
;;;					sc-crews-company-systems-menu-items -> sc-company-systems-menu-items
;;;					install-crews-image -> install-images
;;;	09/02/18	J. P. Varandas	Use only function 'install-images'
;;;					Remove call to ''install-crews-image-old'
;;;-----------------------------------------------------------------------------
(defun company-systems-menu (arg)
  (interactive "e")
  (let ((*current-x-arg* arg)
	(product         (sc-popup-menu arg "Choose product" (sc-product-menu-items))))
    (when product
      ;; Only CREWS uses the old products configuration
      (when (and *old-products-configuration*
                 (not (string= "CREWS" (sc-product-name product))))
        (sc-set-new-products))
      (let ((company-app  (sc-popup-menu arg "Choose company & system"
                                         (if *old-products-configuration*
                                             (sc-crews-company-systems-menu-items-old)
                                             (sc-company-systems-menu-items product))
                                         t)))
        (when company-app
          (destructuring-bind (company-name application-name) company-app
            (let ((purge     "")
                  (operation (sc-popup-menu arg "Choose Operation"
                                            *sc-system-operations-menu-items*)))
              (when operation
                (when (and (member operation '("-c" "-ci" "-cid" "cd"))	; elisp member uses `equal'
                           (x-beep-confirm "Do you want to purge the binary files?"))
                  (setf purge "-pu")))
              (install-images (format "-prod %s -co %s %s %s %s"
				      (sc-product-external-id product)
				      company-name
				      purge
				      operation
				      application-name)))))))))


;; ------------------------------------------------------------------------------
;;                                   PACKAGES
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SC-SET-PACKAGE
;;;Description
;;;	Sets the package of the current buffer.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{str} is a \emph{string}
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/09/28	A. Frazao	Created
;;;	06/05/19	A. Frazao	Use interactive
;;;-----------------------------------------------------------------------------
(defun sc-set-package (str)
  (interactive (list (read-string "Package: " fi:package)))
  (if *windows-emacs*
      (let ((buffer (current-buffer)))
	(cond ((string= (buffer-name buffer) allegro-common-lisp-buffer-name)
	       (insert (format "(in-package %S)" (upcase str)))
	       (fi:inferior-lisp-newline))
	      (t (let ((filename (buffer-file-name buffer)))
		   (if (or (string-match "\\.lisp$"  filename)
			   (string-match "\\.cl$"    filename))
		       (setq fi:package str))))))))


;;;-----------------------------------------------------------------------------
;;;REMOVE-CHARS
;;;Description
;;;	Removes chars from a string.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{string} is a string.
;;;		
;;;		\arg{char-list} is a list of characters.
;;;		
;;;	\return-types
;;;		string
;;;History
;;;	Date		Author		Description
;;;	04/07/09	Dario(Alex) 	Created
;;;-----------------------------------------------------------------------------
(defun remove-chars (string char-list)
  (let ((result ""))
    (dotimes (i (length string))
      (unless (member (elt string i) char-list)
	(setf result (concat result (string (elt string i))))))
    result))


;;;-----------------------------------------------------------------------------
;;;SC-SET-DEFINITION-PACKAGE
;;;Description
;;;	Sets the Lisp buffer package to the package of the definition found by
;;;	searching the "in-package" form that precedes the defintion.
;;;	Useful for compiling definitions in patch files.
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
;;;	04/07/09	Dario(Alex) 	Created
;;;-----------------------------------------------------------------------------
(defun sc-set-definition-package ()
  (interactive)
  (if *windows-emacs*
      (save-excursion
	(let ((package-start)
	      (found (re-search-backward "(.*in-package.*)" nil t)))
	  (when found
	    (goto-char found)
	    (forward-char)
	    (forward-sexp)
	    (setq package-start (point))
	    (forward-sexp)
	    (setq fi:package (remove-chars (buffer-substring package-start (point)) '(? ?\"))))))))


;;;-----------------------------------------------------------------------------
;;;*SC-PACKAGES-LIST*
;;;Description
;;;	List with packages ({(<title> <package name>*)}*)
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/09/28	A. Frazao	Created
;;;	03/07/28	A. Frazao	Re-organised
;;;	04/01/16	A. Frazao	Added SCHEDULER-ST and DISPATCH-SCHEDULER
;;;-----------------------------------------------------------------------------
(defvar *sc-packages-list* '(("Models"
			      "BASIC-MODELS"
			      "DATA-MODELS"
			      "DATA-MANAGER-MODELS"
			      "RESOURCES-MODELS"
			      "ROSTER-MODELS"
			      "SCHEDULER-MODELS" 
			      "SHORT-TERM-MODELS"
			      "TASK-DISTRIBUTOR-MODELS")
			     ("Scheduling"
			      "LABOUR-RULES"
			      "TASK-SCHEDULER"
			      "DUTY-ROSTER"
			      "SCHEDULER-ST"
			      "DISPATCH-SCHEDULER"
			      "POS-TRIP-GEN"
			      "TASK-ABSTRACTOR"
			      "TASK-DISTRIBUTOR"
			      "TASK-GEN"
			      "TASK-SEQ")
			     ("Data Management"
			      "DATA-MANAGER"
			      "DATA-ORGANIZATION" 
			      "INTERFACE"
			      "READERS"
			      "TDMGR")
			     ("Middleware"
			      "MIDDLEWARE")
			     ("GUI"
			      "COMMON-GRAPHICS"
			      "MAPS"
			      "WINGRAPHICS")
			     ("Multi-user"
			      "APPLMGR-CLIENT"
			      "PARTITION-SERVER-CLIENT"
			      "PARTITION-SERVER-CLIENT-COMMON"
			      "PERIODS-CLIENT"
			      "UPSERVER-CLIENT")
			     ("Siscog Util"
			      "CHAVE" 
			      "SEARCH"
			      "SG-ODBC"
			      "SIKE"
			      "SISCOG"
			      "TRADUCAO"
			      "UTIL")
			     ("Lisp"
			      "CLOS"
			      "COMMON-LISP"
			      "COMMON-LISP-USER"
			      "USER")
			     ("Allegro"
			      "ACL-SOCKET"
			      "COMMON-GRAPHICS-USER"
			      "DBI"
			      "EXCL"
			      "FOREIGN-FUNCTIONS"
			      "MULTIPROCESSING"
			      "PROFILER"
			      "WINDOWS")))


;;;-----------------------------------------------------------------------------
;;;SC-SET-PACKAGE-MENU
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/09/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-set-package-menu (arg)
  (let ((*current-x-arg* arg)
	(items nil))
    (dolist (submenu *sc-packages-list*)
      (if (cdr submenu)
	  (let ((sub-items nil))
	    (push (car submenu) sub-items)
	    (dolist (package (cdr submenu))
	      (push (sc-make-menu-item package package) sub-items))
	    (push (reverse sub-items) items))))
    (if items
	(let ((choice (sc-popup-menu arg "Choose package" (reverse items) t)))
	  (when choice
	    (sc-set-package choice))))))


;;;-----------------------------------------------------------------------------
;;;                               UTILITIES MENU
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SC-CREWS-UTILITIES-MENU-ITEMS
;;;Description
;;;	Defines the utilities menu
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	99/09/10	J. P. Varandas	Add option "Install Crews Image"
;;;	99/09/28	A. Frazao	Add options "Set package" and
;;;					"PC -> UNIX (Reopen)"
;;;	01/11/13	J. P. Varandas	Add option "Digest Modifications"
;;;	04/07/09	Dario		Added "Set definition package"
;;;	05/10/19	A. Frazao	Added "UNIX -> PC (Reopen)"
;;;	06/05/19	A. Frazao	Added "Set other package"
;;;	09/02/10	J. P. Varandas	"Install Crews Image" -> "Install Image" 
;;;-----------------------------------------------------------------------------
(defvar sc-product-utilities-menu-items
    (list (sc-make-menu-item "Set defined package"    'sc-set-package-menu)
	  (sc-make-menu-item "Set other package"      '(call-interactively 'sc-set-package))
	  (sc-make-menu-item "Set definition package" 'sc-set-definition-package)
	  ""
	  (sc-make-menu-item "Find in dictionaries"   'find-in-dic)
	  (sc-make-menu-item "Insert in dictionaries" 'insert-in-dic)
	  (sc-make-menu-item "Insert all keywords"    'insert-keywords)
	  (sc-make-menu-item "Reset ignored keywords" 'reset-keywords)
	  ""
	  (sc-make-menu-item "Insert comment tabs"    'insert-comment-tabs)
	  (sc-make-menu-item "Insert in system"       'insert-in-system)
	  (sc-make-menu-item "Find In Other Window"   'find-in-other-window)
	  ""
	  (sc-make-menu-item "Kill Line With Pattern" 'sc-kill-line-with-pattern)
	  (sc-make-menu-item "PC -> UNIX"             'change-pc2unix)
	  (sc-make-menu-item "PC -> UNIX (Reopen)"    'change-pc2unix-reopen)
	  (sc-make-menu-item "UNIX -> PC (Reopen)"    'change-unix2pc-reopen)
	  (sc-make-menu-item "Set all file as new"    'set-file-new)
	  ""
	  (sc-make-menu-item "Digest Modifications"   'digest-modifications)
	  ""
	  (sc-make-menu-item "Install Image"          'company-systems-menu)
	  ))



;;;-----------------------------------------------------------------------------
;;;                     Systems and sub-systems for modifications
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;GET-SYSTEM-VERSIONS
;;;Description
;;;	Returns the current versions of \arg{system} of type \arg{version-type}.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{string} with the name of the system.
;;;		
;;;		\arg{version-type} is a \emph{keyword} with the version type.
;;;		Possible values are \emph{:production} and \emph{:development}.
;;;		
;;;	\return-types
;;;		List of \elem{string} with the current versions.
;;;History
;;;	Date		Author		Description
;;;	01/05/02	A. Vasconcelos	Created
;;;	01/06/08	Toni		Added version 1.3.0 for the CREWS_WAGN
;;;					system.
;;;	01/07/04	J. P. Varandas	Added version 6.3.0 for the CREWS system.
;;;					Added version 3.3.0 for the CREWS_NS system.
;;;	01/09/27	A. Vasconcelos	Updated for CREWS_ML and CREWS_DSB.
;;;					Removed version 6.1.0 of CREWS.
;;;	01/10/16	Toni		Removed version 1.2.0 of CREWS_WAGN.
;;;					Removed version 3.1.4 of PMS.
;;;					Added version 3.2.0 of PMS.
;;;	02/01/13	Fausto		Updated for CREWS_STOG.
;;;	02/01/31	Pedro Matos	Updated for CREWS_VR.
;;;	02/05/21	Fausto		Updated for CREWS_DSB.
;;;	02/06/07	J. P. Varandas	Updated versions of CREWS, CREWS-NS
;;;	02/07/18	Carlos Ribeiro	Updated for CREWS_BRISA.
;;;	02/08/22	Dario		Corrected bug in cond last alternative:
;;;					T -> t (ELISP is case sensitive!)
;;;	02/12/09	A. Frazao	Updated for CREWS_SISCOG
;;;	02/07/09	Pedro Matos	Updated for VIP V1.1.1
;;;	03/07/28	A. Frazao	Uses *sc-system-versions*
;;;-----------------------------------------------------------------------------
(defun get-system-versions (system version-type)
  (let ((versions '("VDEV")))
    (if (eq version-type :production)
	(dolist (sc-system *sc-all-systems*)
	  (when (string-equal (sc-system-name sc-system) system)
	    (setf versions (sc-system-versions sc-system))
	    (return))))
    versions))


;;;-----------------------------------------------------------------------------
;;;GET-ALL-SUB-SYSTEMS
;;;Description
;;;	Returns all the applicable sub-systems.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{sc-system}. 
;;;		
;;;	\return-types
;;;		A list of \emph{string}, the sub-systems. If \arg{system} is
;;;		supplied, returns the sub-systems of \arg{system}. Otherwise,
;;;		returns all the existing sub-systems.
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;	03/09/18	A. Frazao	Consider also the application image name
;;;	09/02/04	J. P. Varandas	*sc-all-crews-applications* -> *sc-all-applications*
;;;					sc-crews-application-name -> sc-application-name
;;;					sc-crews-application-image-name -> sc-application-image-name
;;;-----------------------------------------------------------------------------
(defun get-all-sub-systems (&optional system)
  (let ((sub-systems nil)
	(applications (or (and system (sc-system-applications system))
			  *sc-all-applications*)))
    (dolist (appl applications)
      (setf sub-systems (cons (upcase (sc-application-name appl)) sub-systems))
      ;; For the sub-systems that have different name from the image name (typically due to '-win')
      ;; add also the image name (with '-win').
      (if (not (string-equal (sc-application-name appl)
			     (sc-application-image-name appl)))
	  (setf sub-systems (cons (upcase (sc-application-image-name appl)) sub-systems))))
    (reverse sub-systems)))


;;;-----------------------------------------------------------------------------
;;;GET-SYSTEM-OF
;;;Description
;;;	Determines the software or company to which the file identified by
;;;	\arg{filename} belongs, as well as the subsystems to which applies.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{filename} is a string. It is the full path name of a file
;;;		that is added, changed or deleted in the scope of a modification.
;;;		
;;;	\return-types
;;;		A list with 3 elements with the following meaning:
;;;		\begin{itemize,numbered}
;;;			\item a string with the name of the company or software
;;;				to which the file belongs
;;;			\item a list of strings where each string holds the
;;;				name of a subsystem to which the file applies
;;;		\end{itemize}
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/10/19	J. P. Varandas	Created
;;;	00/07/26	J. P. Varandas	Added WORK-RECORDER
;;;	01/03/02	Toni		Added support for the PMS system.
;;;					Doc updated
;;;	01/05/02	A. Vasconcelos	Added "PARTITION-SERVER"
;;;	01/09/27	A. Vasconcelos	Updated for CREWS_ML and CREWS_DSB.
;;;					Replaced patricia by toni.
;;;	02/01/13	Fausto		Updated for CREWS_STOG.
;;;	02/01/31	Pedro Matos	Updated for CREWS_VR.
;;;	02/05/21	Fausto		Updated for CREWS_DSB.
;;;	02/06/07	J. P. Varandas	Deal with data folders
;;;					Changed the destination of the project's code
;;;	02/07/18	Carlos Ribeiro	Updated for CREWS_BRISA.
;;;	02/08/22	Dario		Corrected bug in cond last alternative:
;;;					T -> t (ELISP is case sensitive!)
;;;					stg -> stog
;;;	02/12/09	A. Frazao	Updated for CREWS_SISCOG
;;;	03/07/28	A. Frazao	Use *sc-all-systems*
;;;	05/10/12	A. Frazao	Removed responsibles
;;;	08/11/03	A. Frazao	If there are no sub-systems, just use the system name
;;;	09/02/04	J. P. Varandas	sc-system-crews-name -> sc-system-company-name
;;;					'get-sub-system-of' has a different signature
;;;	09/09/23	P. Madeira	`system-name' -> `sc-system-name'
;;;-----------------------------------------------------------------------------
(defun get-system-of (filename)
  (let ((sc-system-name nil)
	(sub-systems nil))
    (block exit
      (dolist (sc-system *sc-all-systems*)
	(dolist (version-dir (sc-system-src-version-dirs sc-system))
	  (let ((dir (if (consp version-dir)
			 (eval version-dir)
			 version-dir)))
	    (cond ((and dir
			(string-match dir filename))
		   (setf sc-system-name (sc-system-name sc-system))
		   (setf sub-systems (get-sub-system-of sc-system (substring filename (match-end 0))))
		   (return-from exit))
		  ((string-match (format "%s\\-data" (sc-system-company-name sc-system)) filename)
		   (setf sc-system-name (sc-system-name sc-system))
		   (setf sub-systems (get-all-sub-systems sc-system))
		   (return-from exit)))
	    ))))
    (unless sc-system-name
      (setf sub-systems (get-all-sub-systems)))
    (unless sub-systems
      (setf sub-systems (list sc-system-name)))
    (list sc-system-name sub-systems)))


;;;-----------------------------------------------------------------------------
;;;GET-SUB-SYSTEM-OF
;;;Description
;;;	For a given filename it returns a list with the applicable systems
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{sc-system}
;;;		
;;;		\arg{filename} is a file specification.
;;;		
;;;	\return-types
;;;		\item A list of \elem{string} with the applicable systems.
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/10/19	J. P. Varandas	Created
;;;	01/05/02	A. Vasconcelos	Added "PARTITION-SERVER"
;;;	02/06/07	J. P. Varandas	Removed the X modules
;;;	03/07/28	A. Frazao	Use *sc-crews-all-file-subsystems*
;;;	05/10/12	A. Frazao	Removed responsible
;;;	09/02/04	J. P. Varandas	Added argument \arg{system}
;;;					Handle the diffent products
;;;-----------------------------------------------------------------------------
(defun get-sub-system-of (system filename)
  (if (string-match "^CREWS" (sc-system-name system))
      (let ((sub-systems nil))
	(dolist (file-subsystems *sc-all-file-subsystems*)
	  (when (string-match (sc-file-subsystems-pathname file-subsystems) filename)
	    (setf sub-systems (sc-file-subsystems-names file-subsystems))
	    (return)))
	sub-systems)
      (let ((applications nil))
	(dolist (appl (sc-system-applications system))
	  (setf applications (cons (upcase (sc-application-name appl)) applications)))
	(nreverse applications))))


;;;-----------------------------------------------------------------------------
;;;PROCESS-SYSTEM
;;;Description
;;;	Coordinates the process of finding the companies, systems and
;;;	responsable persons of a modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{results} is a list with system, sub-systems and responsibles.
;;;		Each element of the list is a list with the syntax
;;;		(<system crews name> <sub-systems> <responsibles>), where:
;;;		\begin{itemize,bulleted}
;;;		\item <system crews name> is an uppercase string of a system crews
;;;		      name.
;;;		\item <sub-systems> is a list of strings, each being an uppercase
;;;		      CREWS application name.
;;;		\item <responsibles> is a list of strings, each being an email
;;;		      address of a responsible to which the modificaton should be sent.
;;;		\end{itemize}
;;;		
;;;		\arg{mods} is a list of modification files (see \elem{make-mod-file}).
;;;		
;;;		\arg{patch} is a \emph{string}, the name of the modification patch
;;;		file, or \emph{nil} if there is no patch for the modification.
;;;		
;;;	\return-types
;;;		A list whose first element is a string with all the applicable
;;;		systems and the rest are strings, the email addresses to which
;;;		the modification should be sent to.
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/10/19	J. P. Varandas	Created
;;;	01/03/02	Toni		Added support for the PMS system.
;;;	01/09/27	A. Vasconcelos	Updated for CREWS_ML and CREWS_DSB.
;;;	02/01/13	Fausto		Updated for CREWS_STOG.
;;;	02/01/31	Pedro Matos	Updated for CREWS_VR.
;;;	02/05/21	Fausto		Updated for CREWS_DSB.
;;;	02/06/07	J. P. Varandas	Handle empty results.
;;;	02/07/18	Carlos Ribeiro	Updated for CREWS_BRISA.
;;;	02/12/09	A. Frazao	Updated for CREWS_SISCOG
;;;	03/07/28	A. Frazao	Use generic values.
;;;	03/07/28	A. Frazao	Corrected version systems
;;;	05/10/13	J. P. Varandas	Do not compute the responsible to send the modification
;;;-----------------------------------------------------------------------------
(defun process-system (results mods patch)
  (let ((headers nil)
	(vdev-systems "")
	(versions-systems ""))
    
    (if results
	(dolist (system *sc-all-systems*)
	  (when (find (sc-system-name system) results :key 'car :test 'string-equal)
	    (setf headers (cons (get-versions-header (sc-system-name system)) headers))))
	(setf headers (list (get-versions-header nil))))
    (setf headers (reverse headers))

    (dolist (header headers)
      (when mods
	(setf vdev-systems (format "%s %s" vdev-systems (first header))))
      (when (or patch
		(member* "PMS" results :key 'car :test 'string-equal))
	(setf versions-systems (format "%s %s" versions-systems (second header)))))
    
    (format "%s %s" 
	    (if (string-equal vdev-systems "") "" (subseq vdev-systems 1))
	    (if (string-equal versions-systems "") "" (subseq versions-systems 1)))))


;;;-----------------------------------------------------------------------------
;;;PROCESS-SUB-SYSTEM
;;;Description
;;;	Collects all the sub-systems envolved on the modification.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{results} is a list with system, sub-systems and responsibles.
;;;		Each element of the list is a list with the syntax
;;;		(<system crews name> <sub-systems> <responsibles>), where:
;;;		\begin{itemize,bulleted}
;;;		\item <system crews name> is an uppercase string of a system crews
;;;		      name.
;;;		\item <sub-systems> is a list of strings, each being an uppercase
;;;		      CREWS application name.
;;;		\item <responsibles> is a list of strings, each being an email
;;;		      address of a responsible to which the modificaton should be sent.
;;;		\end{itemize}
;;;		
;;;	\return-types
;;;		A list of strings, each being an uppercase CREWS application name.
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/10/19	J. P. Varandas	Created
;;;	02/06/07	J. P. Varandas	\arg{results} may be NIL
;;;	03/07/28	A. Frazao	Corrected order by applications
;;;	03/09/18	A. Frazao	Remove redundant sub-systems
;;;	09/02/04	J. P. Varandas	sc-crews-application-image-name -> sc-application-image-name
;;;					sc-find-crews-application -> sc-find-application
;;;					sc-crews-application-name -> sc-application-name
;;;-----------------------------------------------------------------------------
(defun process-sub-system (results)
  (if results
      (let ((sub-systems nil))
	(dolist (sub-system (get-all-sub-systems))
	  (dolist (result results)
	    (when (find sub-system (second result) :test 'string-equal)
	      (setq sub-systems (cons sub-system sub-systems))
	      (return))))
	;; For the sub-systems that have different name from the image name (typically due to '-win')
	;; check if they are duplicated. Keep the image name (with '-win').
	(dolist (sub-system sub-systems)
	  (let ((appl (sc-find-application (downcase sub-system) :key 'sc-application-image-name)))
	    (when appl
	      (setq sub-systems (remove* (upcase (sc-application-name appl)) sub-systems :test 'string-equal)))))
	(reverse sub-systems))
      (get-all-sub-systems)))


;;;-----------------------------------------------------------------------------
;;;MAKE-SYSTEM-VERSIONS
;;;Description
;;;	Returns a string resulting from the concatenation of \arg{system} with
;;;	the several system versions in \arg{versions}.
;;;	
;;;	\visibility
;;;	        SI
;;;		
;;;	\args
;;;		\arg{system} is a \elem{string} with the name of the system.
;;;		
;;;		\arg{versions} is a \elem{list} of \elem{string} with the versions
;;;		of \arg{system}.
;;;		
;;;	\return-types
;;;		A \elem{string} with the system versions.
;;;		
;;;History
;;;	Date		Author		Description
;;;	01/05/02	A. Vasconcelos	Created
;;;	03/07/28	A. Frazao	Corrected spacing
;;;-----------------------------------------------------------------------------
(defun make-system-versions (system versions)
  (when versions
    (let ((result (format "%s-%s" system (car versions))))
      (dolist (version (cdr versions))
	(setq result (concatenate 'string result (format " %s-%s" system version))))
      (upcase result))))


;;;-----------------------------------------------------------------------------
;;;GET-VERSIONS-HEADER
;;;Description
;;;	Defines the contents of the \emph{System} field of the header of the
;;;	modification mail file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{sc-system-name} is a string. It represents a system or a collection
;;;		of systems.
;;;		
;;;	\return-types
;;;		A list with 3 elements with the following meaning:
;;;		\begin{itemize,numbered}
;;;			\item a string with the contents of the \emph{System}
;;;				field for the current version of the system
;;;			\item a string with the contents of the \emph{System}
;;;				field for the previous version of the system
;;;			\item a list of strings where each string holds the
;;;				name of a company to which is necessary to send
;;;				a patch
;;;		\end{itemize}
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/10/19	J. P. Varandas	Created
;;;	01/03/02	Toni		Added support for the PMS system.
;;;					Doc updated
;;;	01/05/02	A. Vasconcelos	Reformulated in order to enable several
;;;					production and development versions of a system.
;;;	01/09/27	A. Vasconcelos	Updated for CREWS_ML and CREWS_DSB.
;;;	01/10/09	Fausto		Added "ml" "dsb" to list.
;;;	02/01/13	Fausto		Updated for CREWS_STOG.
;;;	02/01/31	Pedro Matos	Updated for CREWS_VR.
;;;	02/05/21	Fausto		Updated for CREWS_DSB.
;;;	02/06/07	J. P. Varandas	\arg{system} may be 'NULL'
;;;	02/07/18	Carlos Ribeiro	Updated for CREWS_BRISA.
;;;	02/12/09	A. Frazao	Updated for CREWS_SISCOG
;;;	03/07/28	A. Frazao	Uses *sc-all-systems*
;;;	03/07/28	A. Frazao	Use CREWS by default
;;;	09/09/23	P. Madeira	`system-name' -> `sc-system-name'
;;;-----------------------------------------------------------------------------
(defun get-versions-header (sc-system-name)
  (unless sc-system-name
    (setf sc-system-name "CREWS"))
  (let ((system (sc-find-system sc-system-name)))
    (list (make-system-versions sc-system-name (get-system-versions sc-system-name :development))
	  (make-system-versions sc-system-name (get-system-versions sc-system-name :production))
	  (when (sc-system-send-to system)
	    (list (sc-system-send-to system))))))
