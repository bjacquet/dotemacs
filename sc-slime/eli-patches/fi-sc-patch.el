;;;-----------------------------------------------------------------------------
;;; Description
;;;	ELI re-definitions to correct bugs and keep correct behaviour through ACL versions.
;;;	
;;; History
;;;	Date		Author		Description
;;;	10/01/26	P. Madeira	Re-definitions of file: c:/Program Files/acl80/eli/fi-lep.el
;;;					  FI::CL-FILE-POSITION-TO-POINT
;;;					Re-definitions of file: eli81/fi-lep.el
;;;					  FI::SHOW-FOUND-DEFINITION
;;;					  CORRECT-METHOD-SPECIALIZER
;;;					  FI:TRACE-DEFINER
;;;					Re-definitions of file: eli81/fi-util.el
;;;					  FI::PACKAGE
;;;					  FI::START-LISP-INTERFACE-WINDOWS
;;;	10/08/10	P. Madeira	Re-definitions of file: eli81/fi-keys.el
;;;					  FI:CHECK-UNBALANCED-PARENTHESES-WHEN-SAVING
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;; File: c:/Program Files/acl80/eli/fi-lep.el
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;FI::CL-FILE-POSITION-TO-POINT
;;;Description
;;;	Re-definition of ACL 8.0's ELI to calculate the correct position of a
;;;	definition in the current buffer; for ACL versions >= 8.1, it simply
;;;	adds 1.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{real-file-position} is a \emph{number}.
;;;		
;;;	\return-types
;;;		A \emph{number}.
;;;		
;;;	\remarks
;;;		In versions of ACL < 8.1, the Windows/DOS files' newlines were
;;;		counted entirely as 2 characters (CR + LF). EMACS only counts as
;;;		1, therefore the need for this function.
;;;History
;;;	Date		Author		Description
;;;	10/01/26	P. Madeira	Count lines differently for ACL >= 8.1
;;;-----------------------------------------------------------------------------
(defun fi::cl-file-position-to-point (real-file-position)
  ;; This function is only called on Windows.

  ;; REAL-FILE-POSITION is the cl:file-position with the file in binary
  ;; mode (CR and LF terminates each line).  The job of this function is to
  ;; find the point associated with this file position.
  ;;
  ;; We start from the beginning of the file and go forward line by line,
  ;; counting by adding an extra character for the LF that isn't in the
  ;; buffer.
  
  (if (and fi::lisp-version
	   (or (> (car fi::lisp-version) 8)
	       (and (= (car fi::lisp-version) 8)
		    (>= (caadr fi::lisp-version) 1))))
      (1+ real-file-position)
      (save-excursion
	(let ((file-position (+ real-file-position
				;; add two because the file-position is just
				;; before the form:
				2))
	      chars-on-line)
	  (goto-char (point-min))
	  (block done
	    (while (progn
		     (setq chars-on-line
			   (- (save-excursion (end-of-line) (point))
			      (point)))
		     (setq file-position (- file-position chars-on-line
					    ;; one for CR and LF:
					    2))
		     t)
	      (cond
		((> file-position 0)
		 ;; keep going
		 (goto-char (+ (point) chars-on-line 1)))
		(t (return-from done)))))
	  (point)))))


;;;-----------------------------------------------------------------------------
;;; File: eli81/fi-lep.el
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;FI::SHOW-FOUND-DEFINITION
;;;Description
;;;	Re-definition that calls `fi::cl-file-position-to-point' to know the
;;;	exact position where to jump to.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{thing} is a \emph{form}.
;;;		
;;;		\arg{pathname} is a \emph{string}.
;;;		
;;;		\arg{point} is a \emph{number}.
;;;		
;;;		\arg{n-more} is a \emph{number} or nil.
;;;		
;;;		\arg{other-window-p} is a \emph{generalized boolean}.
;;;		
;;;		\arg{pop-stack} is a \emph{list}.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	10/01/26	P. Madeira	Use `fi::cl-file-position-to-point'
;;;-----------------------------------------------------------------------------
(defun fi::show-found-definition (thing pathname point n-more
				  &optional other-window-p pop-stack)
  (if pathname
      (if (equal pathname "top-level")
	  (message
	   "%s was defined somewhere at the top-level, %d more definitions"
	   thing n-more)
	(let ((mess "")
	      (xb nil)
	      (pathname (fi::ensure-translated-pathname pathname)))
	  (when fi:filename-frobber-hook
	    (setq pathname (funcall fi:filename-frobber-hook pathname)))
	  (ring-insert lep::show-def-marker-ring (point-marker))
	  (setq xb (get-file-buffer pathname))
	  (if other-window-p
	      (find-file-other-window pathname)
	    (find-file pathname))
	  (if xb (set-mark (point)))
	  (if (null point)
	      (progn
		(setq mess
		  (fi::double-char-in-string
		   ?%
		   (format "The definition of %s is somewhere in this file! "
			   thing)))
		(beginning-of-buffer))
	    (progn
	      (goto-char
	       (if (on-ms-windows)
		   (fi::cl-file-position-to-point point)
		   (1+ point)))
	      (if (not xb) (set-mark (point)))))
	  (cond ((eq n-more 0)
		 (if (lep::meta-dot-from-fspec)
		     (message (concat mess "%ss of %s")
			      (lep::meta-dot-what) (lep::meta-dot-from-fspec))
		   (message (concat mess "No more %ss of %s")
			    (lep::meta-dot-what) thing)))
		(n-more
 		 (message (concat mess "%d more %ss of %s")
			  n-more
			  (lep::meta-dot-what)
			  (or (lep::meta-dot-from-fspec) thing))))
	  (when pop-stack (fi::pop-metadot-session))))
    (message "cannot find file for %s" thing)))


;;;-----------------------------------------------------------------------------
;;; File: eli81/fi-lep.el
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;CORRECT-METHOD-SPECIALIZER
;;;Description
;;;	Removes quotes from eql specializers.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{specializer-form} is a \emph{list}: a specialized argument.
;;;		
;;;	\return-types
;;;		A \emph{list}: a specialized argument.
;;;History
;;;	Date		Author		Description
;;;	10/01/26	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defun correct-method-specializer (specializer-form)
  (cond ((and (consp specializer-form) (eql 'eql (car specializer-form))
	      (consp (cadr specializer-form)) (eql 'quote (caadr specializer-form)))
	 (list 'eql (cadadr specializer-form)))
	(t specializer-form)))


;;;-----------------------------------------------------------------------------
;;; File: eli81/fi-lep.el
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;FI:TRACE-DEFINER
;;;Description
;;;	Re-definition that resolves a bug, unquoting eql specializers in methods.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{current-prefix-arg} is an \emph{object}.
;;;		
;;;	\return-types
;;;		void.
;;;History
;;;	Date		Author		Description
;;;	10/01/26	P. Madeira	Remove quote from EQL specializers
;;;-----------------------------------------------------------------------------
(defun fi:trace-definer (current-prefix-arg)
  "Dynamically toggle, in the Common Lisp environment, tracing for the
function defined by the top-level form around the cursor position.  The
form can be a defun, defgeneric, defmethod, define-compiler-macro, or
deftype.  The defmethod case is most useful, as the function spec for
the particular method is extracted from the qualifiers and specializers.
If tracing is already turned on, then it will be turned off.  With a
prefix arg, cause the debugger to be invoked via a call to BREAK when
the function is called.  fi:package is used to determine from which
Common Lisp package the operation is done.  In a subprocess buffer, the
package is tracked automatically.  In source buffer, the package is
parsed at file visit time."
  (interactive "P")
  (save-excursion
    (let (definer spec name qualifiers specializers)
      (forward-char 1)
      (beginning-of-defun)
      (unless (looking-at "(def")
	(error "Can't parse a top-level defining form"))
      (forward-char 1)			;open paren
      (setq definer (fi::substr-sexp))
      (setq name (fi::substr-sexp))
      (cond ((fi::string-equal-nocase definer "defmethod")
	     (loop as subform = (read-from-string (fi::substr-sexp))
		   as next = (car subform)
		   while (symbolp next)
		   collect next into quals
		   finally do (setq qualifiers (apply 'concat (mapcar 'symbol-name quals)))
		   (setq specializers
		     (loop for spec in next
			   until (member spec '(&optional &rest &key &aux &allow-other-keys))
			   collect (if (atom spec)
				       't
				       (correct-method-specializer (cadr spec))))))
	     (setq spec
	       (concat "(method " name " "
		       qualifiers " "
		       (format "%S" specializers)
		       " )")))
	    ((or (fi::string-equal-nocase definer "defun")
		 (fi::string-equal-nocase definer "defmacro")
		 (fi::string-equal-nocase definer "defgeneric"))
	     (setq spec name))
	    ((fi::string-equal-nocase definer "deftype")
	     (setq spec (format "(excl::deftype-expander %s)" name)))
	    ((fi::string-equal-nocase definer "define-compiler-macro")
	     (setq spec (format "(:property %s excl::.compiler-macro.)" name)))
	    (t (error "Can't trace a %s" definer)))
      (fi::make-request
	  (lep::toggle-trace :fspec (fi::defontify-string spec) :break current-prefix-arg)
	;; Normal continuation
	(() (what tracep)
	 (message (if tracep "%s is now traced" "%s is now untraced")
		  what))
	;; Error continuation
	((spec) (error)
	 (fi::show-error-text "Cannot (un)trace %s: %s" spec error))))))


;;;-----------------------------------------------------------------------------
;;; File: eli81/fi-util.el
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;FI::PACKAGE
;;;Description
;;;	Re-definition that uses a predefined fi:package when no package
;;;	specifier is found in the current buffer.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{string}.
;;;History
;;;	Date		Author		Description
;;;	10/01/26	P. Madeira	Resort to a preset package if no
;;;					in-package is found.
;;;-----------------------------------------------------------------------------
(defun fi::package ()
  (cond
   ((not fi::multiple-in-packages) fi:package)
   (t
    (or
     ;; look back from the point for the correct package name
     (save-excursion (fi::parse-package-from-buffer t t t))
     fi:package))))


;;;-----------------------------------------------------------------------------
;;; File: eli81/fi-util.el
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;FI::START-LISP-INTERFACE-WINDOWS
;;;Description
;;;	Re-definition that uses the "-e" command-line specifier, the only entry
;;;	point available in SISCOG images, calling
;;;	excl::new-start-emacs-lisp-interface.
;;;	
;;;	It is assumed that we're all using ACL versions >= 7.0.
;;;	Right now, we're all using ACL versions >= 8.0.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{use-background-streams} is a \emph{generalized-boolean}.
;;;		
;;;		\arg{lisp-image-name} is a \emph{string}.
;;;		
;;;	\return-types
;;;		A \emph{list} of \emph{string}.
;;;History
;;;	Date		Author		Description
;;;	10/01/26	P. Madeira	Allow one ACL for each EMACS
;;;-----------------------------------------------------------------------------
(defun fi::start-lisp-interface-windows (use-background-streams
					 &optional lisp-image-name)
  (let ((args (list "-e"
                    (format "(excl::new-start-emacs-lisp-interface :background-streams %s)"
                            use-background-streams))))

    (when lisp-image-name
      (when (string-match "^~" lisp-image-name)
	(setq lisp-image-name (expand-file-name lisp-image-name)))
      (setq args
	(append (list "-I"
		      (if (cygwinp)
			  (cygwin-to-windows-pathname lisp-image-name)
			lisp-image-name))
		args)))
    
    args))


;;;-----------------------------------------------------------------------------
;;; File: eli81/fi-keys.el
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;FI:CHECK-UNBALANCED-PARENTHESES-WHEN-SAVING
;;;Description
;;;	Re-definition to workaround an inadvertent buffer coding system change
;;;	when parentheses are not balanced and answering no to the "save file
;;;	anyway" question.
;;;	
;;;	It is used as a hook in `write-file-hooks'.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{boolean}.
;;;		
;;;	\remarks
;;;		The original definition of this function reports that the file
;;;		has been written when we answer no to the "save file anyway"
;;;		question, but it does not set `last-coding-system-used', which
;;;		`basic-save-buffer' is expecting to reflect the coding system
;;;		actually used to write the file, so it can update the buffer.
;;;		
;;;		Typing C-g is a workaround to this problem, because it
;;;		interrupts the saving procedure. This re-definition always
;;;		interrupts the saving procedure by re-signaling the error if
;;;		you answer no.
;;;History
;;;	Date		Author		Description
;;;	10/08/10	P. Madeira	Re-signal error on "no"
;;;-----------------------------------------------------------------------------
(defun fi:check-unbalanced-parentheses-when-saving ()
  (if (and fi:check-unbalanced-parentheses-when-saving
	   (memq major-mode '(fi:common-lisp-mode fi:emacs-lisp-mode
			      fi:franz-lisp-mode)))
      (if (eq 'warn fi:check-unbalanced-parentheses-when-saving)
	  (save-excursion
	    (condition-case nil
		(progn (fi:find-unbalanced-parenthesis) nil)
	      (error
	       (message "Warning: parens are not balanced in this buffer.")
	       (ding)
	       (sit-for 2)
	       ;; so the file is written:
	       nil)))
	(condition-case err
	    (progn (fi:find-unbalanced-parenthesis) nil)
	  (error
	   ;; save file if user types "yes":
	   (let ((answer (y-or-n-p "Parens are not balanced.  Save file anyway? ")))
	     (if answer
		 ;; so the file is written:
		 nil
	       ;; Rethrow the error to interrupt saving procedure
	       (signal (car err) (cdr err)))))))))

