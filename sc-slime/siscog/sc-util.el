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
;;;	This file contains some utilities for use with GNU Emacs.
;;; History
;;;	Date		Author		Description
;;;	91/09/01	A. Frazao	Created
;;;	99/01/12	A. Frazao	Added definitions
;;;					  SC-DELETE-DIRECTORY
;;;					  SC-COPY-DIRECTORY
;;;					  SC-MAKE-DIRECTORY
;;;					  SC-DELETE-FILE
;;;					  SC-COPY-FILE
;;;	99/02/26	A. Frazao	Changed definitions
;;;					  FILES-ITEMS
;;;					  SC-COPY-DIRECTORY
;;;					Added definitions
;;;					  SC-CREWS-FILE-P
;;;	99/02/27	A. Frazao	Added definitions
;;;					  FILES-WITH-TYPE
;;;					  FILES-WITH-NAME
;;;	99/03/23	Toni		Changed definitions
;;;					  SC-CREWS-FILE-P
;;;					  SC-COPY-DIRECTORY
;;;					Added definitions
;;;					  SC-C-AND-CPP-CREWS-MULTI-USER-FILE-P
;;;	99/08/25	Toni		Changed definitions
;;;					  SC-C-AND-CPP-CREWS-MULTI-USER-FILE-P
;;;					Added definitions
;;;					  SC-C-AND-CPP-CREWS-DATABASE-FILE-P
;;;					  SC-C-AND-CPP-CREWS-FILE-P
;;;	99/08/31	J. P. Varandas	Added definitions
;;;					  SC-MOVE-FILE
;;;					  ABSOLUTE-PATH-P
;;;					  SC-INSERT-COMMAS2
;;;					  SC-INDENT-REGION
;;;					Changed definitions
;;;					  SELECT-FILE-MENU
;;;	99/09/28	A. Frazao	Changed definitions
;;;					  REMOVE-CR
;;;	00/02/02	A. Frazao	Added definitions
;;;					  SC-DATE-FROM-TIMEVAL
;;;					  SC-CURRENT-DATE
;;;					  SC-DATE-FROM-TIMESTRING
;;;					  SC-DATE-DAY
;;;					  SC-DATE-MONTH
;;;					  SC-DATE-YEAR
;;;					  MAKE-SC-DATE
;;;					  MY-CURRENT-TIME-STRING
;;;					  CURRENT-YEAR
;;;					  CURRENT-DATE-STRING
;;;					  *SC-MONTHS*
;;;					  SC-DATE<
;;;	02/02/28	A. Frazao	Added definitions
;;;					  NEXT-SPACE
;;;					  NEXT-NON-SPACE
;;;	05/07/01	A. Frazao	Changed definitions
;;;					  SC-COPY-FILE
;;;	05/10/19	A. Frazao	Added definitions
;;;					  ADD-CR
;;;	08/02/22	Tom Weissmann	Added definitions
;;;					  SC-STRING-JOIN
;;;					  SC-Y-OR-N-P
;;;	08/07/08	A. Frazao	Changed definitions
;;;					  SELECT-FILE-MENU
;;;					  (SETQ SC-DIR-LIST)
;;;	08/10/09	Tom Weissmann	Changed definitions
;;;					  STRING-JOIN
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  (SETQ SC-DIR-LIST)
;;;					  FILES-ITEMS
;;;					  SC-COPY-DIRECTORY
;;;					  SC-RPLAN-FILE-P
;;;	09/09/23	P. Madeira	Added definitions
;;;					  SC-KILL-LINE
;;;					Changed definitions
;;;					  SC-COPY-FILE
;;;					  SC-MOVE-FILE
;;;					  SC-DELETE-FILE
;;;					  SC-MAKE-DIRECTORY
;;;					  F3-INSERT-TAB
;;;					  F4-INSERT-TABS
;;;	09/10/30	A. Frazao	Added definitions
;;;					  SC-DIRECTORIES
;;;-----------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------------
;;                                    DATE ADT
;; ------------------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;MAKE-SC-DATE
;;;Description
;;;	Creates and returns a date object
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun make-sc-date (year month day)
  (+ (* year 10000) (* month 100) day))

;;;-----------------------------------------------------------------------------
;;;SC-DATE-YEAR
;;;Description
;;;	Returns the year of a date
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-date-year (date)
  (floor date 10000))

;;;-----------------------------------------------------------------------------
;;;SC-DATE-MONTH
;;;Description
;;;	Returns the month of a date
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-date-month (date)
  (rem* (floor date 100) 100))

;;;-----------------------------------------------------------------------------
;;;SC-DATE-DAY
;;;Description
;;;	Returns the day of a date
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-date-day (date)
  (rem* (rem* date 10000) 100))

;;;-----------------------------------------------------------------------------
;;;SC-DATE<
;;;Description
;;;	Returns TRUE if DATE1 is lower then DATE2
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-date< (date1 date2)
  (< date1 date2))

;;;-----------------------------------------------------------------------------
;;;*SC-MONTHS*
;;;Description
;;;	Variable with the list of months to extract dates from strings
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defvar *sc-months* '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;;;-----------------------------------------------------------------------------
;;;SC-DATE-FROM-TIMESTRING
;;;Description
;;;	Creates a date object from an internal time string (from current-time-string)
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-date-from-timestring (string)
  (if (string-match "^[^ ]+ \\([^ ]+\\)[ ]+\\([0-9]+\\) \\([0-9:]+\\) \\([0-9][0-9][0-9][0-9]\\)"
		    string)
      (let ((year (car (read-from-string (substring string (match-beginning 4) (match-end 4)))))
	    (month (+ (position (substring string (match-beginning 1) (match-end 1)) *sc-months* :test 'equal) 1))
	    (day (car (read-from-string (substring string (match-beginning 2) (match-end 2))))))
	(make-sc-date year month day))))
  
;;;-----------------------------------------------------------------------------
;;;SC-DATE-FROM-TIMEVAL
;;;Description
;;;	Returns the date defined in a time value
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-date-from-timeval (timeval)
  (sc-date-from-timestring (current-time-string timeval)))

;;;-----------------------------------------------------------------------------
;;;SC-CURRENT-DATE
;;;Description
;;;	Returns the current date
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-current-date ()
  (sc-date-from-timestring (current-time-string)))

;;;-----------------------------------------------------------------------------
;;;CURRENT-DATE-STRING
;;;Description
;;;	Returns a string with the format YY/MM/DD that represents the current
;;;	date.
;;;History
;;;	Date		Author		Description
;;;	00/02/02	A. Frazao	Moved from sc-mod.el
;;;					Uses DATE adt
;;;-----------------------------------------------------------------------------
(defun current-date-string ()
  (let ((date (sc-current-date)))
    (format "%02d/%02d/%02d" (rem* (sc-date-year date) 100) (sc-date-month date) (sc-date-day date))))

;;;-----------------------------------------------------------------------------
;;;CURRENT-YEAR
;;;Description
;;;	Returns the current year
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	00/02/02	A. Frazao	Moved from sc-mod.el
;;;					Uses DATE adt
;;;-----------------------------------------------------------------------------
(defun current-year ()
  (sc-date-year (sc-current-date)))

;;;-----------------------------------------------------------------------------
;;;MY-CURRENT-TIME-STRING
;;;Description
;;;	Devolve uma string do tipo "96/07/01 19:14" com a data e hora corrente
;;;History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Created
;;;	00/02/02	A. Frazao	Moved from sc-mod.el
;;;-----------------------------------------------------------------------------
(defun my-current-time-string ()
  (let ((date (current-time-string)))
    (if (string-match "^[^ ]+ \\([^ ]+\\)[ ]+\\([0-9]+\\) \\([0-9:]+\\) [0-9][0-9]\\([0-9][0-9]\\)"
		      date)
	(let ((year  (substring date (match-beginning 4) (match-end 4)))
	      (month (+ (position (substring date (match-beginning 1) (match-end 1)) *sc-months* :test 'equal) 1))
	      (day   (car (read-from-string (substring date (match-beginning 2) (match-end 2)))))
	      (time  (substring date (match-beginning 3) (+ (match-beginning 3) 5))))
	  (format "%s/%02d/%02d\t%s" year month day time)))))


;; ------------------------------------------------------------------------------------------
;;                                    Emacs Lisp utilities
;; ------------------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SC-KILL-LINE
;;;Description
;;;	Delete a line but do not put it in kill ring.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{arg} is a number or nil
;;;		
;;;	\return-types
;;;		NIL
;;;History
;;;	Date		Author		Description
;;;	09/09/23	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defun sc-kill-line (&optional arg)
  (let ((kill-whole-line nil) ; Behaviour expected by sc-mod
	(kill-ring '()) ; Rebind the kill ring to avoid annoying effects
	(kill-ring-yank-pointer '())) ; Same for kill ring yank pointer
    (kill-line arg)))

;;;-----------------------------------------------------------------------------
;;;SC-RPLAN-FILE-P
;;;Description
;;;	Test if \arg{name} is a relevant file.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string}, a file name.
;;;		
;;;		\arg{directory-name} is a \emph{string}, the name of \arg{name}'s
;;;		parent directory. It is ignored.
;;;		
;;;	\return-types
;;;		\emph{NIL} unless \arg{name} is an relevant file.
;;;
;;;History
;;;	Date		Author		Description
;;;	99/02/26	A. Frazao	Created
;;;	99/03/23	Toni		Added a second optional parameter to
;;;					receive the immediately enclosing
;;;					directory name, which is not used when
;;;					this definition of the function is
;;;					called.
;;;	09/02/10	J. P. Varandas	Changed function name: sc-crews-file-p -> sc-rplan-file-p
;;;					Simplified (Tom Weissmann).
;;;-----------------------------------------------------------------------------
(defun sc-rplan-file-p (name &optional directory-name)
  (not (or (string-match "^[%.]" name)
           (string-match "[~#]$" name)
           (member (file-name-extension name)
                   '("o"
                     "sbin"
                     "fasl"
                     "elc"
                     "elc-e"
                     "elc-x")))))

;;;-----------------------------------------------------------------------------
;;;SC-C-AND-CPP-CREWS-FILE-P
;;;Description
;;;	Test if \arg{name} is a correct C or C++ Crews
;;;	related file.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string}, a file name.
;;;		
;;;		\arg{directory-name} is a \emph{string}, \arg{name}'s parent
;;;		directory.
;;;		
;;;	\return-types
;;;		\emph{NIL} unless \arg{name} is a correct C or C++ Crews
;;;		related file.
;;;
;;;	\remarks
;;;	 	 This function must not call function 'sc-crews-file-p'.
;;;		 This is because this function is called as if it was the mentioned
;;;		 one. Thus calling 'sc-crews-file-p' results in an endless recursion.
;;;
;;;History
;;;	Date		Author		Description
;;;	99/08/25	Toni		Created
;;;-----------------------------------------------------------------------------
(defun sc-c-and-cpp-crews-file-p (name directory-name)
  (and (not (member (aref name 0) '(37 46)))
       (not (char-equal (elt name 0) 46))
       (not (string-match "~$" name))
       (not (string-match "#$" name))
       (not (string-match "\\.o$" name))
       (not (string-match "\\.so$" name))
       (not (string-match "\\.sbin$" name))
       (not (string-match "\\.fasl$" name))
       (not (string-match "\\.elc$" name))
       (not (string-match "\\.elc-e$" name))
       (not (string-match "\\.elc-x$" name))
       (not (string-match "\\.sql$" name))
       (not (string-match "\\.lisp$" name))
       (not (string-match "\\.cl$" name))))

;;;-----------------------------------------------------------------------------
;;;SC-C-AND-CPP-CREWS-MULTI-USER-FILE-P
;;;Description
;;;	Tests if \arg{name} is a correct C or C++ Crews
;;;	Multi-User related file.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string}, a file name.
;;;		
;;;		\arg{directory-name} is a \emph{string}, the name of \arg{name}s
;;;		parent directory.
;;;		
;;;	\return-types
;;;		\emph{NIL} unless \arg{name} is a correct C or C++ Crews
;;;	Multi-User related file.
;;;
;;;	\remarks
;;;	  This function must not call function 'sc-crews-file-p'.
;;;	  This is because this function is called as if it was the mentioned
;;;	  one. Thus calling 'sc-crews-file-p' results in an endless recursion.
;;;
;;;History
;;;	Date		Author		Description
;;;	99/03/23	Toni		Created
;;;	99/08/25	Toni		Most of the work was delegated to
;;;					function 'sc-c-and-cpp-crews-file-p'.
;;;-----------------------------------------------------------------------------
(defun sc-c-and-cpp-crews-multi-user-file-p (name directory-name)
  (or (string-match "/win_services" directory-name)
      (sc-c-and-cpp-crews-file-p name directory-name)))

;;;-----------------------------------------------------------------------------
;;;SC-C-AND-CPP-CREWS-DATABASE-FILE-P
;;;Description
;;;	Test if \arg{name} is a correct C or C++ Crews
;;;	Database related file.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{name} is a \emph{string}, a file name.
;;;		
;;;		\arg{directory-name} is a \emph{string}, the name of \arg{name}s
;;;		parent directory.
;;;		
;;;	\return-types
;;;		\emph{NIL} unless \arg{name} is a correct C or C++ Crews
;;;		Database related file.
;;;
;;;	\remarks
;;;	  This function must not call function 'sc-crews-file-p'.
;;;	  This is because this function is called as if it was the mentioned
;;;	  one. Thus calling 'sc-crews-file-p' results in an endless recursion.
;;;
;;;History
;;;	Date		Author		Description
;;;	99/08/25	Toni		Created
;;;-----------------------------------------------------------------------------
(defun sc-c-and-cpp-crews-database-file-p (name directory-name)
  (or (string-match "/win_database" directory-name)
      (sc-c-and-cpp-crews-file-p name directory-name)))

;;;-----------------------------------------------------------------------------
;;;FILES-WITH-NAME
;;;Description
;;;	Returns a list of files in DIR that starts with NAME
;;;History
;;;	Date		Author		Description
;;;	99/02/27	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun files-with-name (dir name)
  (directory-files dir t (format "^%s\\." name)))

;;;-----------------------------------------------------------------------------
;;;FILES-WITH-TYPE
;;;Description
;;;	Returns a list of files in DIR that ends with TYPE
;;;History
;;;	Date		Author		Description
;;;	99/02/27	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun files-with-type (dir type)
  (directory-files dir t (format "\\.%s$" type)))

;;;-----------------------------------------------------------------------------
;;;SC-DIRECTORIES
;;;Description
;;;	Returns the list of sub-directories of \arg{dir}
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a \emph{string}, the source directory.
;;;		
;;;	\return-types
;;;		A list of \emph{string}
;;;
;;;History
;;;	Date		Author		Description
;;;	09/10/30	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-directories (dir)
  (let ((directories nil))
    (dolist (name (directory-files dir))
      (when (sc-rplan-file-p name dir)
	(let ((file (format "%s/%s" dir name)))
	  (when (file-directory-p file)
	    (push file directories)))))
    directories))


;;;-----------------------------------------------------------------------------
;;;SC-COPY-DIRECTORY
;;;Description
;;;	Recursively copy a directory, filtering files with `sc-rplan-file-p'.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{from} is a \emph{string}, the source directory.
;;;		
;;;		\arg{to} is a \emph{string}, the destination directory.
;;;		
;;;	\return-types
;;;		void
;;;
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	99/02/26	A. Frazao	Calls SC-CREWS-FILE-P
;;;	99/03/23	Toni		Passed a second argument 'from' to the
;;;					call of 'sc-crews-file-p'.
;;;	09/02/10	J. P. Varandas	sc-crews-file-p -> sc-rplan-file-p
;;;-----------------------------------------------------------------------------
(defun sc-copy-directory (from to)
  (if (not (file-directory-p to))
      (make-directory to t))
  (dolist (name (directory-files from))
    (if (sc-rplan-file-p name from)
	(let ((orig (format "%s/%s" from name))
	      (dest (format "%s/%s" to name)))
	  (cond ((file-directory-p orig)
		 (sc-copy-directory orig dest))
		((file-exists-p orig)
		 (when (file-exists-p dest)
		   (delete-file dest))
		 (copy-file orig dest)))))))

;;;-----------------------------------------------------------------------------
;;;SC-DELETE-DIRECTORY
;;;Description
;;;	Apaga uma directoria recursivamente
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-delete-directory (dir)
  (dolist (name (directory-files dir))
    (let ((path (format "%s/%s" dir name)))
      (cond ((file-directory-p path)
	     (if (and (not (string= name "."))
		      (not (string= name "..")))
		 (sc-delete-directory path)))
	    ((file-exists-p path)
	     (delete-file path)))))
  (delete-directory dir))

;;;-----------------------------------------------------------------------------
;;;SC-COPY-FILE
;;;Description
;;;	Copies a file.
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{from} is a \emph{string}.
;;;		
;;;		\arg{to} is a \emph{string}.
;;;		
;;;	\return-types
;;;		void
;;;		
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	05/07/01	A. Frazao	Use copy-file
;;;	09/09/23	P. Madeira	Use built-in `copy-file'.
;;;					Keep last-modified time.
;;;-----------------------------------------------------------------------------
(defun sc-copy-file (from to)
  (condition-case err
      (copy-file from to t t)
    (error nil)))

;;;-----------------------------------------------------------------------------
;;;SC-MOVE-FILE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	09/09/23	P. Madeira	Use built-in `rename-file'
;;;-----------------------------------------------------------------------------
(defun sc-move-file (from to)
  (condition-case err
      (rename-file from to t)
    (error nil)))

;;;-----------------------------------------------------------------------------
;;;SC-DELETE-FILE
;;;Description
;;;	Apaga um ficheiro (depende do sistema operativo)
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	09/09/23	P. Madeira	Use built-in `delete-file'
;;;-----------------------------------------------------------------------------
(defun sc-delete-file (file)
  (condition-case err
      (delete-file file)
    (error nil)))

;;;-----------------------------------------------------------------------------
;;;SC-MAKE-DIRECTORY
;;;Description
;;;	Cria uma directoria (depende do sistema operativo)
;;;History
;;;	Date		Author		Description
;;;	99/01/12	A. Frazao	Created
;;;	09/09/23	P. Madeira	Use built-in `make-directory'
;;;-----------------------------------------------------------------------------
(defun sc-make-directory (dir)
  (condition-case err
      (make-directory dir t)
    (error nil)))

;;;-----------------------------------------------------------------------------
;;;NEXT-NON-SPACE
;;;Description
;;;	Moves to the next character that is NOT a space, tab or newline.
;;;History
;;;	Date		Author		Description
;;;	02/02/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun next-non-space ()
  (while (and (char-after (point))
	      (member (char-after (point)) '(9 32 10)))
    (forward-char)))

;;;-----------------------------------------------------------------------------
;;;NEXT-SPACE
;;;Description
;;;	Moves to the next character that is a space, tab or newline.
;;;History
;;;	Date		Author		Description
;;;	02/02/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun next-space ()
  (while (and (char-after (point))
	      (not (member (char-after (point)) '(9 32 10))))
    (forward-char)))

(defun sc-string-left-trim (char string)
  (let ((i 0)
	(m (length string)))
    (while (and (< i m) (char-equal (elt string i) char))
      (setq i (1+ i)))
    (substring string i m)))

(defun sc-string-right-trim (char string)
  (let ((i (1- (length string))))
    (while (and (>= i 0) (char-equal (elt string i) char))
      (setq i (1- i)))
    (substring string 0 (1+ i))))

(defun sc-string-trim (char string)
  (sc-string-left-trim char (sc-string-right-trim char string)))


(defun sc-position-r (el seq &optional test)
  (if (null test)
      (setq test 'eql))
  (let ((i (1- (length seq))))
    (while (and (>= i 0)
		(null (funcall test el (elt seq i))))
      (setq i (1- i)))
    (if (>= i 0)
	i)))

(defun sc-string-to-list-aux (string char)
  (let ((pos (position char string)))
    (if pos
	(cons (substring string 0 pos) (sc-string-to-list-aux (sc-string-left-trim char (substring string pos)) char))
	(if (string-equal string "")
	    nil
	    (list string)))))

(defun sc-string-to-list (string &optional char)
  (if (null char)
      (setq char (if (member 'xemacs features)
		     (int-char 32)
		     32)))
  (sc-string-to-list-aux (sc-string-trim char string) char))

(defun sc-command (command)
  (setq command (sc-string-to-list command))
  (let ((buffer (current-buffer))
	string)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (let ((start (point)))
      (setq command (append (list (car command) nil t nil) (cdr command)))
      (apply 'call-process command)
      (setq string (buffer-substring start (point)))
      (kill-region start (point)))
    (switch-to-buffer buffer)
    string))

;; ------------------------------------------------------------------------------------------
;;                                 INSERTING COMMAS
;; ------------------------------------------------------------------------------------------

(defun sc-insert-commas ()
  (interactive)
  (let ((start (min (point) (mark t)))
	(end   (max (point) (mark t))))
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (< (point) end)
	(if (not (= (char-after (point)) 10))
	    (progn
	      (insert-char 59 1)
	      (setq end (+ end 1))))
	(forward-line)))))

;;;-----------------------------------------------------------------------------
;;;SC-INSERT-COMMAS2
;;;Description
;;;	Inserts 3 commas if the first character of the first line is not a comma
;;;	OR clears all the commas at the beginning of the region lines
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun sc-insert-commas2 ()
  (interactive)
  (let ((start (min (point) (mark t)))
	(end   (max (point) (mark t))))
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (if (= (char-after (point)) 59)
	  (progn
	    (while (< (point) end)
	      (while (= (char-after (point)) 59)
		(delete-char 1)
		(setq end (- end 1)))
	      (forward-line)))
	(while (< (point) end)
	  (if (not (= (char-after (point)) 10))
	      (progn
		(insert-char 59 1)
		(insert-char 59 1)
		(insert-char 59 1)
		(setq end (+ end 3))))
	  (forward-line))))))

(defun sc-delete-commas ()
  (interactive)
  (let ((start (min (point) (mark t)))
	(end   (max (point) (mark t))))
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (< (point) end)
	(if (= (char-after (point)) 59)
	    (delete-char 1 t))
	(forward-line)))))

;; ------------------------------------------------------------------------------
;;                                   INSERTING TABS
;; ------------------------------------------------------------------------------

;;;	09/09/23	P. Madeira	`insert-tab' -> `f3-insert-tab'.
;;;					Use `insert-tab' with `indent-tabs-mode' set to `t'.
(defun f3-insert-tab (&optional arg)
  (interactive)
  (let ((indent-tabs-mode t))
    (insert-tab arg)))

;;;	09/09/23	P. Madeira	`insert-tabs' -> `f4-insert-tabs'.
;;;					Use `f3-insert-tab' with count of 5.
(defun f4-insert-tabs ()
  (interactive)
  (f3-insert-tab 5))

(defun move-to-next-tab ()
  (interactive)
  (let ((point (+ (point) 1))
	(max (point-max))
	(beep t))
    (while (< point max)
      (if (= (char-after point) 9)
	  (progn
	    (goto-char point)
	    (setq point max)
	    (setq beep nil))
	  (setq point (+ point 1))))
    (if beep (beep))))

;; ------------------------------------------------------------------------------
;;                                  REMOVE CARRIAGE-RETURNS
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;REMOVE-CR
;;;Description
;;;	Removes all carriage returns
;;;History
;;;	Date		Author		Description
;;;	99/09/28	A. Frazao	Does not use replace string
;;;-----------------------------------------------------------------------------
(defun remove-cr ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (char-after (point))
      (if (= (char-after (point)) 13)
	  (delete-char 1)
	  (forward-char 1)))))

;;;-----------------------------------------------------------------------------
;;;ADD-CR
;;;Description
;;;	Adds carriage returns to buffer
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
;;;	05/10/19	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun add-cr ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((start (point)))
      (while (char-after (point))
	(if (= (char-after (point)) 10)
	    (progn
	      (if (not (= (point) start))
		  (progn
		    (backward-char 1)
		    (if (not (= (char-after (point)) 13))
			(progn
			  (forward-char 1)
			  (insert 13))
			(forward-char 1)))
		  (progn
		    (insert 13)
		    (forward-char 1)))))
	(if (char-after (point))
	    (forward-char 1))))))

;; ------------------------------------------------------------------------------
;;                                   INDENTING DEFINITIONS
;; ------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;SC-INDENT-REGION
;;;Description
;;;	Indents a definition completely
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun sc-indent-region ()
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (point) (mark) nil)))

;; ------------------------------------------------------------------------------
;;                                   SELECTING FILES
;; ------------------------------------------------------------------------------

(defvar sc-dir-list nil)

;;;-----------------------------------------------------------------------------
;;;(SETQ SC-DIR-LIST)
;;;Description
;;;	Sets the directories to edit files
;;;History
;;;	Date		Author		Description
;;;	08/07/08	A. Frazao	Changed Siscog Files
;;;	09/02/10	J. P. Varandas	Added Fleet and On-Time
;;;-----------------------------------------------------------------------------
(setq sc-dir-list
  (append (list (sc-make-menu-item "Edit Siscog File"      '(getenv "SISCOG_DIR"))
                (sc-make-menu-item "Edit Crews Data File"  '(getenv "CREWS_DATA_DIR"))
                (sc-make-menu-item "Edit Fleet Data File"  '(getenv "FLEET_DATA_DIR"))
		(sc-make-menu-item "Edit On-Time Data File" '(getenv "ONTIME_DATA_DIR"))
		(sc-make-menu-item "Edit Home File"    (getenv "HOME")))
	  (if *windows-emacs*
	      (list (sc-make-menu-item "Edit Siscog Original File"  "x:")
		    (sc-make-menu-item "Edit Emacs File"     (getenv "SISCOG_EMACS_DIR"))
		    (sc-make-menu-item "Emacs Lisp (NT)"     "c:/Program Files/emacs-19.34/lisp"))
	      (list (sc-make-menu-item "Edit Emacs File"     (getenv "SISCOG_EMACS_DIR"))))))

(defun item< (item1 item2)
  (string< (sc-menu-item-name item1) (sc-menu-item-name item2)))

(defun directories-items (dir)
  (let ((items nil)
	(names (directory-files dir)))
    (while names
      (if (or (not (char-equal (elt (car names) 0) 46))
	      (equal (car names) ".")
	      (equal (car names) ".."))
	  (let ((new-dir (format "%s/%s" dir (car names))))
	    (cond ((equal (car names) ".")
		   (setq items (cons (sc-make-menu-item (format "%s" (car names)) (list new-dir 2)) items)))
		  ((file-directory-p new-dir)
		   (setq items (cons (sc-make-menu-item (format "%s >" (car names)) (list new-dir 1)) items))))))
      (setq names (cdr names)))
    (sort items 'item<)))

;;;-----------------------------------------------------------------------------
;;;FILES-ITEMS
;;;Description
;;;	Create a list of menu items corresponding to the files in \arg{dir},
;;;	filtered by `sc-rplan-file-p'.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{dir} is a \emph{string}, a directory.
;;;		
;;;	\return-types
;;;		A \emph{list} of menu items.
;;;
;;;History
;;;	Date		Author		Description
;;;	99/02/26	A. Frazao	Calls SC-CREWS-FILE-P
;;;	09/02/10	J. P. Varandas	sc-crews-file-p -> sc-rplan-file-p
;;;-----------------------------------------------------------------------------
(defun files-items (dir)
  (let ((items nil)
	(names (directory-files dir)))
    (while names
      (if (sc-rplan-file-p (car names))
	  (let ((file (format "%s/%s" dir (car names))))
	    (if (not (file-directory-p file))
		(setq items (cons (sc-make-menu-item (car names) (list file 0)) items)))))
      (setq names (cdr names)))
    (sort items 'item<)))

(defun process-data-item (result arg)
  (let ((path (car result))
	(type (cadr result)))
    (case type
      (0 (switch-to-buffer (find-file-noselect path)))
      (1 (edit-dir-file path arg))
      (2 (dired path)))))

(defun edit-dir-file (path arg)
  (let ((items (append (directories-items path) (files-items path))))
    (if items
	(let ((result (sc-popup-menu arg "Select" items nil 40)))
	  (if result
	      (process-data-item result arg)))
	(beep))))

(defun edit-file (arg dir)
  (if (file-directory-p dir)
      (edit-dir-file dir arg)
      (beep)))

;;;-----------------------------------------------------------------------------
;;;ABSOLUTE-PATH-P
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun absolute-path-p (path)
  (and (> (length path) 0)
       (or (position 58 path)
	   (char-equal (elt path 0) 47)
	   (char-equal (elt path 0) 92))))

;;;-----------------------------------------------------------------------------
;;;SELECT-FILE-MENU
;;;Description
;;;	Selects a file from a menu
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{arg} is event arg.
;;;		
;;;	\return-types
;;;		void
;;;
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Use of function 'absolute-path-p'
;;;	08/07/08	A. Frazao	CREWS_DIR -> SISCOG_DIR
;;;-----------------------------------------------------------------------------
(defun select-file-menu (arg)
  (interactive "e")
  (let ((choice (sc-popup-menu arg "Select" sc-dir-list)))
    (if choice
	(progn
	  (if (consp choice)
	      (setf choice (eval choice)))
	  (if (absolute-path-p choice)
	      (edit-file arg (format "%s" choice))
	      (edit-file arg (format "%s/%s" (getenv "SISCOG_DIR") choice)))))))


;;;-----------------------------------------------------------------------------
;;;SC-Y-OR-N-P
;;;Description
;;;	Ask the user a yes or no question, using a dialog box. Return t if
;;;	the answer is yes.
;;;	\arg{prompt} is the string to display to ask the question.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{prompt} is a \emph{string} to display to ask the
;;;		 question.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	08/02/22	Tom Weissmann	Created
;;;-----------------------------------------------------------------------------
(defun sc-y-or-n-p (prompt)
  (let ((use-dialog-box t)
	(last-nonmenu-event nil))
    (y-or-n-p prompt)))


;;;-----------------------------------------------------------------------------
;;;ENSURE-LIST
;;;Description
;;;	If \arg{obj} is a list, return it unchanged, otherwise return a list
;;;     containing it.
;;;     
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{obj} is an \emph{object}.
;;;		
;;;	\return-types
;;;		\emph{list}.
;;;History
;;;	Date		Author		Description
;;;	08/02/22	Tom Weissmann	Created
;;;-----------------------------------------------------------------------------
(defun ensure-list (obj)
  (if (listp obj) obj (list obj)))


;;;-----------------------------------------------------------------------------
;;;STRING-JOIN
;;;Description
;;;	Concatenate \arg{strings} into a \emph{string}, separated by \arg{sep}.
;;;	If any of \arg{strings} is a \emph{list} it is joined recursively.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{sep} is a \emph{string}.
;;;		
;;;		\arg{strings} is a \emph{list} of \emph{string}s.
;;;		
;;;	\return-types
;;;		\emph{string}
;;;
;;;	\example
;;;		(string-join  ", " "one" "two" "three")
;;;		-> "one, two, three"
;;;
;;;		(string-join ", " '("one" "two") "three")
;;;		-> "one, two, three"
;;;
;;;History
;;;	Date		Author		Description
;;;	08/10/09	Tom Weissmann	Created (POA 12960.0)
;;;-----------------------------------------------------------------------------
(defun string-join (sep &rest strings)
  "Concatenate STRINGS into a string, separated by SEP.

If any of STRINGS is a list it is joined recursively."
  (mapconcat (lambda (bit)
	       (if (listp bit)
		   (apply 'string-join sep bit)
		 bit))
	     strings sep))

