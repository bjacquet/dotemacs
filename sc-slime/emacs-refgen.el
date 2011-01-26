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
;;;	Interface com o emacs do gerador do manual de referencia.
;;; History
;;;	Date		Author		Description
;;;	96/09/16	A. Vasconcelos	Added definitions
;;;					  BEGINNING-OF-DEFINITION
;;;	96/09/23	A. Vasconcelos	Added definitions
;;;					  *REFGEN-LISP-FILE*
;;;					Changed definitions
;;;					  GET-DEFINITION-ID2
;;;	96/12/19	A. Frazao	Added definitions
;;;					  INFORM-DEFLR-FILE
;;;					Changed definitions
;;;					  GET-DEFINITION-DESCRIPTION
;;;					  GET-DEFINITION-ID2
;;;	97/03/27	A. Vasconcelos	Changed definitions
;;;					  GET-DEFINITION-DESCRIPTION
;;;	97/03/27	A. Vasconcelos	Changed definitions
;;;					  CREATE-LISP-FILE
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;INSTALACAO:

;;;Para instalar, actualizar o valor das seguintes variaveis, compilar este ficheiro com "Byte Compile" e carrega-lo no emacs.

;;;A variavel *CREWS-DOCUMENTATION-DIR* contem a directoria onde se guardam os documentos produzidos pela ferramenta.

;;;A variavel *REFGEN-LISP-FILE* contem a especificacao do ficheiro Lisp que gera os manuais de referencia

;;;-----------------------------------------------------------------------------
;;;UTILIZACAO:

;;;Correr uma das applicacoes do CREWS que contenha o codigo a documentar
;;;Abrir, num buffer, o ficheiro Lisp a documentar, e seleccionar esse buffer.
;;;Carregar na tecla F10
;;;O sistema pede um nome para o ficheiro Lisp intermedio.
;;;O sistema pergunta se quer compilar o ficheiro Lisp intermedio e gerar o manual de referencia.
;;;O documento com o manual de referencia (extensao "rtf") e guardado na directoria *CREWS-DOCUMENTATION-DIR*

;;;-----------------------------------------------------------------------------


(defvar *crews-documentation-dir* "/home/vasco/siscog/documentacao")

;;;-----------------------------------------------------------------------------
;;;*REFGEN-LISP-FILE*
;;;Description
;;;	Path do ficheiro Lisp para geracao do manual de referencia
;;;History
;;;	Date		Author		Description
;;;	96/09/23	A. Vasconcelos	Created
;;;-----------------------------------------------------------------------------
(defvar *refgen-lisp-file* "/home/vasco/refgen")


(defvar *crews-documentation* "")

(defvar *documentation-buffer* nil)

(defvar *the-current-buffer* nil)

(defun beginning-of-definition ()
  (end-of-defun)
  (beginning-of-defun))

(defun inform-function-file (obj-name string doc date)
  (when *documentation-buffer*
    (set-buffer *documentation-buffer*)
    (end-of-buffer)
    (insert 9 "(user::function-documentation" 32 34 (downcase obj-name) 34 32 "'" string 32 34 doc 34 32 34 date 34 32 "stream)" 10)
    (set-buffer *the-current-buffer*)
    t))

(defun inform-macro-file (obj-name string doc date)
  (when *documentation-buffer*
    (set-buffer *documentation-buffer*)
    (end-of-buffer)
    (insert 9 "(user::macro-documentation" 32 34 (downcase obj-name) 34 32 "'" string 32 34 doc 34 32 34 date 34 32 "stream)" 10)
    (set-buffer *the-current-buffer*)
    t))

;;;-----------------------------------------------------------------------------
;;;INFORM-DEFLR-FILE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/12/19	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun inform-deflr-file (obj-name string doc date)
  (when *documentation-buffer*
    (set-buffer *documentation-buffer*)
    (end-of-buffer)
    (insert 9 "(user::deflr-documentation" 32 34 (downcase obj-name) 34 32 "'" string 32 34 doc 34 32 34 date 34 32 "stream)" 10)
    (set-buffer *the-current-buffer*)
    t))

(defun inform-my-file2 (function type obj-name string doc class date)
  (when *documentation-buffer*
    (set-buffer *documentation-buffer*)
    (end-of-buffer)
    (insert 9 "(user::" function 32 34 type 34 32 34 (downcase obj-name) 34 32 "'" string 32 34 doc 34 32 "'" class 32 34 date 34 32 "stream)" 10)
    (set-buffer *the-current-buffer*)
    t))

(defun inform-clos-class-file (obj-name string doc date)
  (when *documentation-buffer*
    (set-buffer *documentation-buffer*)
    (end-of-buffer)
    (insert 9 "(user::clos-class-documentation" 32 "'" (downcase obj-name) 32 34 doc 34 32 34 date 34 32 "stream)" 10)
    (set-buffer *the-current-buffer*)
    t))

(defun inform-sike-class-file (obj-name string doc date)
  (when *documentation-buffer*
    (set-buffer *documentation-buffer*)
    (end-of-buffer)
    (insert 9 "(user::sike-class-documentation" 32 "'" (downcase obj-name) 32 34 doc 34 32 34 date 34 32 "stream)" 10)
    (set-buffer *the-current-buffer*)
    t))

(defun inform-method-file (obj-name string doc date)
  (when *documentation-buffer*
    (set-buffer *documentation-buffer*)
    (end-of-buffer)
    (insert 9 "(user::method-documentation" 32 34 (downcase obj-name) 34 32 "'" string 32 34 doc 34 32 34 date 34 32 "stream)" 10)
    (set-buffer *the-current-buffer*)
    t))

(defun inform-var-file (type obj-name value doc date)
  (when *documentation-buffer*
    (set-buffer *documentation-buffer*)
    (end-of-buffer)
    (insert 9 "(user::variable-documentation" 32 34 type 34 32 34 (downcase obj-name) 34 32 34 value 34 32 34 doc 34 32 34 date 34 32 "stream)" 10)
    (set-buffer *the-current-buffer*)
    t))

(defun get-next-method-class2 ()
  (next-non-space)
  (cond ((or (= (char-after (point)) 41))
	 nil)
	(t (let (start end)
	     (setq start (point))
	     (forward-sexp)
	     (setq end (point))
	     (upcase (buffer-substring start end))))))

(defun get-method-classes2 ()
  (let ((str (get-next-method-class2))
	class)
    (while (setq class (get-next-method-class2))
      (setq str (format "%s %s" str class)))
    str))

(defun get-method-id2 ()
  (let ((id t))
    (next-non-space)
    (if (= (char-after (point)) 40)
	(progn
	  (forward-char 1)
	  (setf id (format "(%s)" (or (get-method-classes2) "")))
	  (next-non-space)
	  (forward-char 1)
	  id)
	(progn
	  (get-next-name)
	  (next-non-space)
	  (forward-char 1)
	  (setf id (format "(%s)" (or (get-method-classes2) "")))
	  (next-non-space)
	  (forward-char 1)
	  id))))

(defun append-docs (doc1 doc2)
  (cond ((= (length doc1) 0) doc2)
	((= (length doc2) 0) doc1)
	(t (format "%s %s" doc1 doc2))))

;;;-----------------------------------------------------------------------------
;;;GET-DEFINITION-ID2
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/09/23	A. Vasconcelos	Added notfound
;;;	96/12/19	A. Frazao	Added DEF.LR and DEF.AUX
;;;-----------------------------------------------------------------------------
(defun get-definition-id2 (doc date &optional last-point)
  (let (start end def current-point notfound)
    (and (ignore-errors
	   (end-of-defun)
	   (setq end (point))
	   (beginning-of-defun)
	   (setq start (point))
	   (setq current-point start))
	 (or (null last-point)
	     (not (= last-point start)))
	 (ignore-errors
	   (while (< current-point end)
	     (while (not (= (char-after (point)) 40))
	       (forward-char 1))
	     (forward-char 1)
	     (setq current-point (point))
	     (if (< current-point end)
		 (let ((def (get-next-name)))
		   (if def
		       (let ((name (get-next-name)))
			 (cond ((equal def "DEF.CONSTRUCTOR")
				(inform-my-file2 "make-function-documentation" "Function" (format "MAKE.%s" name) "(NAME &REST ARGS)" doc name date))
			       ((equal def "DEF.EXTERNAL.CONSTRUCTOR")
				(inform-my-file2 "make-function-documentation" "Function" (format "MAKE.%s" name) "(&REST ARGS)" doc name date))
			       ((equal def "DEF.ALL.INSTS")
				(inform-my-file2 "all-function-documentation" "Function" (format "ALL.%sS" name) "()" doc name date))
			       ((equal def "DEF.FIND.INST")
				(inform-my-file2 "find-function-documentation" "Function" (format "FIND.%s" name) "(name)" doc name date))
			       ((equal def "DEF.GET.INST")
				(inform-my-file2 "find-function-documentation" "Function" (format "GET.%s" name) "(name)" doc name date))
			       ((equal def "DEFUN")
				(inform-function-file name (get-method-id2) (append-docs doc (get-next-comment)) date))
			       ((equal def "DEF.LR")
				(inform-deflr-file name (get-method-id2) (append-docs doc (get-next-comment)) date))
			       ((equal def "DEF.AUX")
				(inform-method-file name (get-method-id2) (append-docs doc (get-next-comment)) date))
			       ((equal def "DEFMACRO")
				(inform-macro-file name (get-method-id2) (append-docs doc (get-next-comment)) date))
			       ((equal def "DEFMETHOD")
				(inform-method-file name (get-method-id2) (append-docs doc (get-next-comment)) date))
			       ((equal def "DEFCLASS")
				(inform-clos-class-file name (get-method-id2) doc date))
			       ((equal def "DEF.CLASS")
				(inform-sike-class-file name (get-method-id2) doc date))
			       ((equal def "DEFVAR")
				(inform-var-file "Variable" name (get-next-name) (append-docs doc (get-next-comment)) date))
			       ((equal def "DEFCONSTANT")
				(inform-var-file "Constant" name (get-next-name) (append-docs doc (get-next-comment)) date))
			       ((equal def "DEFPARAMETER")
				(inform-var-file "Parameter" name (get-next-name) (append-docs doc (get-next-comment)) date))
			       ((equal def "D@")
				(inform-var-file "Parameter" name (get-next-name)
						 (append-docs "This a special parameter. %s %s" (append-docs doc (get-next-comment)))
						 date))
			       (t
				(setq notfound t))
			       )
			 (if (not notfound)
			     (setq current-point end)))))))))
    (or start 0)))

;;;-----------------------------------------------------------------------------
;;;GET-DEFINITION-DESCRIPTION
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	96/12/19	A. Frazao	Insert a newline after \\line
;;;	97/03/27	A. Vasconcelos	Deals with forms imediately before the
;;;					definition itself, sucha as proclaims
;;;-----------------------------------------------------------------------------
(defun get-definition-description ()
  (let ((str nil))
    (beginning-of-definition)
    (previous-line 1)
    (beginning-of-line)
    (when (not (looking-at "^;;;---"))
      ;; Maybe its a proclaim (or similar)
      (previous-line 1)
      (beginning-of-line)
      (when (not (looking-at "^;;;---"))
	;; Still no start of header. It doesnt have an header.
	;; Reposition into the starting point
	(next-line 2)
	(beginning-of-line)))
    (if (looking-at "^;;;---")
	(let ((end nil))
	  (while (not end)
	    (previous-line 1)
	    (beginning-of-line)
	    (if (looking-at "^;;;")
		(when (looking-at "^;;;[ \t]*Description")
		  (next-line 1)
		  (beginning-of-line)
		  (while (and (not (looking-at "^;;;[ \t]*History"))
			      (looking-at "\\(^;;;[ \t]+\\)"))
		    (end-of-line)
		    (if str
			(setf str (format "%s\\\\line%c%s" str 10 (buffer-substring (match-end 1) (point))))
			(setf str (buffer-substring (match-end 1) (point))))
		    (next-line 1)
		    (beginning-of-line))
		  (setf end t))
		(setf end t))))
	(let ((current (point)))
	  (backward-sexp)
	  (forward-sexp)
	  (next-line 1)
	  (beginning-of-line)
	  (while (< (point) current)
	    (if (looking-at "\\(^[;]+\\)")
		(let ((new-str nil))
		  (end-of-line)
		  (setf new-str (buffer-substring (match-end 1) (point)))
		  (when (string-match "\\([; \t]+$\\)" new-str)
		    (setf new-str (substring new-str 0 (match-beginning 1))))
		  (when (> (length new-str) 0)
		    (if str
			(setf str (format "%s\\\\line%c%s" str 10 new-str))
			(setf str new-str)))))
	    (next-line 1)
	    (beginning-of-line))))
    (or str "")))

(defun get-definition-date ()
  (let ((str nil))
    (beginning-of-definition)
    (previous-line 1)
    (beginning-of-line)
    (when (looking-at "^;;;")
      (let ((end nil))
	(while (not end)
	  (previous-line 1)
	  (beginning-of-line)
	  (if (looking-at "^;;;")
	      (when (looking-at "^;;;[ \t]+\\([0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\)[ \t]+[^ \t]+")
		(setf str (buffer-substring (match-beginning 1) (match-end 1)))
		(setf end t))
	      (setf end t)))))
    (or str "")))

(defun get-documentation ()
  (interactive)
  (let ((description (save-excursion (get-definition-description)))
	(date (save-excursion (get-definition-date))))
    (when (= (length date) 0)
      (save-excursion (setf date (get-buffer-date))))
    (save-excursion (get-definition-id2 description date))))

(defun get-next-comment ()
  (next-non-space)
  (if (= (char-after (point)) 34)
      (let ((start (point)))
	(forward-sexp)
	(buffer-substring (+ start 1) (- (point) 1)))
      ""))

(defun get-buffer-defs ()
  (let ((end nil)
	(current-point nil)
	(last-point nil)
	(buffer-date (save-excursion (get-buffer-date))))
    (end-of-buffer)
    (setq end (point))
    (beginning-of-buffer)
    (setq current-point (point))
    (while (< current-point end)
      (forward-sexp)
      (save-excursion (backward-char 3)
		      (let ((description (save-excursion (get-definition-description)))
			    (date (save-excursion (get-definition-date))))
			(when (= (length date) 0)
			  (setf date buffer-date))
			(save-excursion (setf last-point (get-definition-id2 description date last-point)))))
      (setq current-point (point)))))

(defun get-package ()
  (save-excursion
    (let ((start nil)
	  (end nil)
	  (found nil))
      (beginning-of-buffer)
      (while (not found)
	(if (looking-at "(in-package")
	    (progn
	      (beginning-of-line)
	      (setq start (point))
	      (end-of-line)
	      (setq end (point))
	      (setq found (buffer-substring start end)))
	    (next-line 1)))
      found)))

(defun get-buffer-date ()
  (let ((date nil))
    (beginning-of-buffer)
    (let ((found nil))
      (while (not found)
	(forward-line)
	(beginning-of-line)
	(if (looking-at "^;;;")
	    (when (looking-at "^;;;[ \t]+\\([0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\)[ \t]+[^ \t]+")
	      (setf date (buffer-substring (match-beginning 1) (match-end 1)))
	      (setf found t))
	    (setf found t))))
    (or date "")))

;;;-----------------------------------------------------------------------------
;;;CREATE-LISP-FILE
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	97/03/27	A. Vasconcelos	Replaced file extension from ".doc" to ".rtf"
;;;-----------------------------------------------------------------------------
(defun create-lisp-file (lisp-file package)
  (setq *crews-documentation* (format "%s/%s.lisp" *crews-documentation-dir* lisp-file))
  (when *crews-documentation*
    (setq *the-current-buffer* (current-buffer))
    (let* ((name (file-name-nondirectory *crews-documentation*))
	   (buf (get-buffer name)))
      (when buf
	(kill-buffer buf))
      (setq *documentation-buffer* (find-file-noselect *crews-documentation*))
      (set-buffer *documentation-buffer*)
      (erase-buffer)
	
      (insert package 10 10 "(with-open-file (stream " 34 *crews-documentation-dir* "/" lisp-file ".rtf" 34 " :direction :output :if-exists :supersede :if-does-not-exist :create)" 10 9 "(user::create-word-document stream)" 10 9 "(user::word-introduction stream)" 10)
	
      (set-buffer *the-current-buffer*)
      t)))

(defun close-lisp-file ()
  (when *documentation-buffer*
    (set-buffer *documentation-buffer*)
    (end-of-buffer)
    (insert 9 "(user::end-word-document stream))" 10)
    (save-buffer *documentation-buffer*)
    (kill-buffer *documentation-buffer*)
    (set-buffer *the-current-buffer*)
    t))

(defun switch-to-lisp2 (choice)
  (interactive "r\nP")
  (cl-goto-lisp-buffer)
  (end-of-buffer)
  (comint-send-string (get-buffer-process (current-buffer)) (concat choice "\n")))

(defun create-doc-file ()
  (interactive)
  (let ((old-buf (current-buffer))
	(lisp-file (read-string "Insert the documentation file name: "))
	(package (get-package)))
    (create-lisp-file lisp-file package)
    (save-excursion (get-buffer-defs))
    (close-lisp-file)
    (beep)
    (when (y-or-n-p "Compile it? ")
      (switch-to-lisp2 (format "(siscog::compile-load-if \"%s\")" *refgen-lisp-file*))
      (switch-to-lisp2 (format "(load \"%s/%s.lisp\" :if-source-only :load)" *crews-documentation-dir* lisp-file)))
    (set-buffer old-buf)
    ))

(define-key global-map [f10] 'create-doc-file)
