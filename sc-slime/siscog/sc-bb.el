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
;;;	
;;; History
;;;	Date		Author		Description
;;;	96/07/01	Joao Filipe	Created
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  ASK-BIG-BROTHER
;;;-----------------------------------------------------------------------------

(defun read-bb-elem ()
  (let ((start (point))
	(end nil)
	(car (char-after (point))))
    (while (and car
		(not (member car '(9 10))))
      (forward-char)
      (setq end (point))
      (setq car (char-after (point))))
    (when end
      (forward-char)
      (buffer-substring start end))))

(defun string<= (str1 str2)
  (or (string= str1 str2)
      (string< str1 str2)))

(defun read-bb-line ()
  (let ((date (read-bb-elem)))
    (if date
	(let ((time (read-bb-elem))
	      (type (read-bb-elem))
	      (mod  (read-bb-elem))
	      (elem nil)
	      (filename nil))
	  (when (member type '("C" "D" "M"))
	    (setq elem (read-bb-elem))
	    (setq filename (read-bb-elem)))
	  (list date time type mod elem filename)))))

(defun bb-date (l)
  (nth 0 l))
(defun bb-time (l)
  (nth 1 l))
(defun bb-type (l)
  (nth 2 l))
(defun bb-mod (l)
  (nth 3 l))
(defun bb-elem (l)
  (nth 4 l))
(defun bb-filename (l)
  (nth 5 l))

(defun bb-date<= (d1 d2)
  (or (null d1)
      (null d2)
      (string<= d1 d2)))

(defun bb-line< (l1 l2)
  (or (string< (bb-date l1) (bb-date l2))
      (and (string= (bb-date l1) (bb-date l2))
	   (or (string< (bb-mod l1)  (bb-mod l2))
	       (and (string= (bb-mod l1)  (bb-mod l2))
		    (or (and (bb-filename l1) (bb-filename l2) (string< (bb-filename l1) (bb-filename l2)))
			(and (or (null (bb-filename l1))
				 (null (bb-filename l2))
				 (string= (bb-filename l1) (bb-filename l2)))
			     (or (string< (bb-time l1) (bb-time l2))
				 (and (string= (bb-time l1) (bb-time l2))
				      (or (string= (bb-type l1) "NEW_M")
					  (not (string= (bb-type l1) "END_M"))
					  (not (string= (bb-type l1) "DEL_M"))))))))))))

(defun read-bb-lines (start-date end-date)
  (let ((lines nil)
	line)
    (loop
        (setq line (read-bb-line))
      (when (null line)
	(return))
      (when (and (bb-date<= start-date (bb-date line))
		 (bb-date<= (bb-date line) end-date))
	(setq lines (push line lines))))
    (sort lines 'bb-line<)))


(defun print-bb-line (line)
  (cond ((string= (bb-type line) "NEW_M")
	 (insert 9 9 (bb-time line) " Criacao" 10))
	((string= (bb-type line) "END_M")
	 (insert 9 9 (bb-time line) " Mail file" 10))
	((string= (bb-type line) "DEL_M")
	 (insert 9 9 (bb-time line) " Apagada" 10))
	((string= (bb-type line) "C")
	 (insert 9 9 9 (bb-time line) " NEW: " (bb-elem line) 10))
	((string= (bb-type line) "M")
	 (insert 9 9 9 (bb-time line) " MOD: " (bb-elem line) 10))
	((string= (bb-type line) "D")
	 (insert 9 9 9 (bb-time line) " DEL: " (bb-elem line) 10))))

;;;-----------------------------------------------------------------------------
;;;ASK-BIG-BROTHER
;;;Description
;;;	Display the contents of the big brother file
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
;;;	09/02/10	J. P. Varandas	*crews-big-brother-file* -> *actions-log-file*
;;;-----------------------------------------------------------------------------
(defun ask-big-brother ()
  (let ((start-date (read-default-string "Enter Start Date" (current-date-string) nil))
	(end-date (read-default-string "Enter End Date" (current-date-string) nil))
	(lines nil))
    (when (and (file-exists-p *actions-log-file*)
	       (file-readable-p *actions-log-file*))
      (let* ((old-buf (current-buffer))
	     (name (file-name-nondirectory *actions-log-file*))
	     (buf (get-buffer name)))
	(when buf
	  (kill-buffer buf))
	(setq buf (find-file-noselect *actions-log-file*))
	(set-buffer buf)
	(beginning-of-buffer)
	(setf lines (read-bb-lines start-date end-date))
	(set-buffer old-buf)

	(setq buf (get-buffer-create "*BIG-BROTHER*"))
	(set-buffer buf)
	(erase-buffer)
	(let ((prev-line nil)
	      (dif-day nil))
	  (dolist (line lines)
	    (if (or (null prev-line)
		    (not (string= (bb-date prev-line) (bb-date line))))
		(progn
		  (insert 10 "DIA " (bb-date line) 10)
		  (setq dif-day t))
		(setq dif-day nil))
	    (when (or dif-day
		      (not (string= (bb-mod prev-line) (bb-mod line))))
	      (insert 10 9 "Modificacao: " (bb-mod line) 10))
	    (when (and (bb-filename line)
		       (or dif-day
			   (not (string= (bb-filename prev-line) (bb-filename line)))))
	      (insert 10 9 9 (bb-filename line) 10))
	    (print-bb-line line)
	    (setq prev-line line)))
	(switch-to-buffer-other-window buf)
	(pop-to-buffer old-buf)))))
