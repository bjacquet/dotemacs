(in-package :lucid)

(defmacro user::compile-def (thing)
  (if (and (listp thing) (eq (car thing) 'clos:defmethod))
      `(clos-sys:compile-method ,thing)
      `(compile ,thing)))

(defmacro dostring ((var string &optional ret) &body body)
  (let ((aux (gensym)))
    `(let ((,var nil))
       (dotimes (,aux (length ,string) ,ret)
	 (setq ,var (char ,string ,aux))
	 ,@body))))

(defun is-special-char? (char)
  (member char '(#\? #\*)))

(defun has-special-chars? (str)
  (dostring (char str)
    (when (is-special-char? char)
      (return t))))

(defun str2list (str)
  (let ((list nil))
    (dostring (char str (nreverse list))
      (when (is-special-char? char)
	(push #\\ list))
      (push char list))))

(defun treat-special-chars (str)
  (let* ((list (str2list str))
	 (n 0)
	 (str2 (make-string (length list))))
    (dolist (char list str2)
      (setf (char str2 n) char)
      (incf n))))

(defun symbol-name2 (sym)
  (let ((str (symbol-name sym)))
    (if (has-special-chars? str)
	(treat-special-chars str)
	str)))

(defun treat-name (name)
  (if (listp name)
      (mapcar #'symbol-name2 name)
      (symbol-name2 name)))

;;; Tem que fazer uma copia da source, porque o get-source-file deve fazer algum tipo de optimizacao
;;; guardando os ultimos resultados => quando se chamava a segunda vez o get-cl-source-file para uma mesma funcao,
;;; se a lista fosse destruida aqui, rebentava...
(defun treat-source (source)
  (let ((source2 (copy-tree source)))
    (setf (cadar source2) (treat-name (cadar source2)))
    source2))
  
(defun treat-source-list (sources)
  (mapcar #'treat-source sources))


(defun get-cl-source-file (symbol)
  (let ((sources nil))
    (dolist (sym (find-all-symbols symbol))
      (dolist (source (get-source-file sym nil t))
	(when (atom (car source))
	  (setq source (cons (list (car source) symbol) (cdr source))))
	(setf sources (cons source sources)))
      (setf sources (append (get-source-file `(setf ,sym) nil t) sources)))
    (treat-source-list sources)
    ))


(defun list-all-my-packages ()
  (let ((packages nil))
    (dolist (pack (list-all-packages))
      (unless (member (package-name pack)
		      '("CLOS" "CLOS-SYSTEM"
			"KEYWORD"
			"LISP"
			"LOOP" 
			;;"LUCID"
			;;"LUCID-COMMON-LISP" -> Por causa dos initialize-instance :after ...
			"LUCID-RUNTIME-SUPPORT"
			"LWALKER"
			"MONITOR"
			"PRETTY-PRINTER"
			"REORGANIZER"
			"SYSTEM"
			"XLIB")
		      :test #'string=)
	(push pack packages)))
    packages))

(defun num-symbs (package)
  (let ((n 0))
    (do-symbols (s package)
      (when (eq package (symbol-package s))
	(incf n)))
    n))

(defmacro do-all-my-symbols ((symbol &optional ret) &body body)
  `(dolist (pack (list-all-my-packages) ,ret)
     (do-symbols (,symbol pack)
       (when (eq pack (symbol-package ,symbol))
	 ,@body))))

(defmacro do-function-body ((symbol function &optional ret) &body body)
  `(dotimes (i (- (system::procedure-length ,function) 4) ,ret)
     (let ((,symbol (system::procedure-ref ,function (+ i 4))))
       ,@body)))

(defun is-symbol-or-setf (symbol elem)
  (or (eq symbol elem)
      (eq (lucid::get-setf-name-symbol symbol) elem)))


(defun has-symbol-aux (symbol elem)
  (declare (special *visited*))
  (push elem *visited*)
  (if (system::procedurep elem)
      (do-function-body (elem-symbol elem nil)
        (when (and (not (member elem-symbol *visited*))
		   (has-symbol-aux symbol elem-symbol))
	  (return t)))
      (is-symbol-or-setf symbol elem)))

(defun has-symbol (symbol elem)
  (let ((*visited* nil))
    (declare (special *visited*))
    (has-symbol-aux symbol elem)))

(defun get-source-file2 (symbol)
  (when (string= (package-name (symbol-package symbol)) "SETF")
    (setq symbol (system::procedure-name (symbol-function symbol))))
  (get-source-file symbol nil t))

(defun get-cl-callers (calling-symbol)
  (let ((sources nil))
    (do-all-my-symbols (symbol)
      (when (and (fboundp symbol)
		 (not (special-form-p symbol))
		 (null (macro-function symbol)))
	(let ((function (symbol-function symbol)))

	  ;;; Esta em trace ???
	  (let ((name (system::procedure-name function)))
	    (when (and (listp name)
		       (eq (first name) :advice)
		       (eq (third name) :trace))
	      (setq function (system::procedure-ref function 14))))
	    
	  (if (clos::generic-function-p function)
	      ;; Its a generic function. So find the methods
	      (dolist (method (clos::generic-function-methods function))
		(let ((function2 (clos::method-function method)))
		  (do-function-body (func-symbol function2)
		    (when (has-symbol calling-symbol func-symbol)
		      (dolist (source (get-source-file2 symbol))
			;; Agora ha que filtrar os metodos com os argumentos correctos.
			(when (equal (car source) (system::procedure-name function2)) 
			  (push (copy-tree source) sources)
			  (return)))
		      (return)))))
	      ;; Its a function
	      (do-function-body (func-symbol function)
	        (when (has-symbol calling-symbol func-symbol)
		  ;; Pode ser +q1 porque pode haver funcoes com o mesmo nome em diferentes packages.
		  (let ((new-sources (copy-tree (get-source-file2 symbol))))
		    (dolist (source new-sources)
		      (setf (car source) (list (car source) symbol))
		      )
		    (setf sources (nconc new-sources sources)))
		  (return)))))))
    (treat-source-list sources)))
