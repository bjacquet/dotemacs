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
;;;	97/05/15	A. Frazao	Added definitions
;;;					  MOUSE-SET-FONT-IF
;;;	99/08/31	J. P. Varandas	Changed definitions
;;;					  X-SC-COPY-KILL-YANK
;;;					  X-SC-SET-POINT
;;;	99/09/20	A. Frazao	Changed map for setting the M-; and M-: keys
;;;	99/09/28	A. Frazao	Removed
;;;					  (global-set-key [f5] 'remove-cr)
;;;	01/07/04	J. P. Varandas	Changed definitions
;;;					  SPLIT-MENU-ITEMS
;;;	03/07/28	A. Frazao	Added definitions
;;;					  SC-EXECUTE-MENU-ITEM
;;;	03/11/14	A. Frazao	Changed definitions
;;;					  SPLIT-WINDOW-VERTICALLY
;;;	04/04/27	A. Frazao	Deleted definitions
;;;					  MOUSE-SET-FONT-IF
;;;					Changed definitions
;;;					  MENU-BAR-FONTS-MENU
;;;					Added definitions
;;;					  SET-FONT-DEFAULT
;;;					  SET-FONT-SELECT
;;;	06/08/24	J. P. Varandas	Deleted definitions related with Solaris
;;;	08/10/13	Tom Weissmann	Replaced deprecated `standard-display-european' with
;;;					`set-language-environment'.
;;;	09/02/02	Tom Weissmann	Added definitions
;;;					  'EMACS-STARTUP-HOOK
;;;					  SC-STARTUP-HOOK
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  [M-DOWN-MOUSE-1] ('crews-applications-menu -> 'product-applications-menu)
;;;	09/02/10	J. P. Varandas	Changed definitions
;;;					  [S-DOWN-MOUSE-1] ('select-crews-mod-action -> 'select-modif-request-action)
;;;	09/02/13	A. Frazao	Do not change [C-DOWN-MOUSE-3]
;;;	09/03/13	J. P. Varandas	Replaced `set-language-environment' with 
;;;					`standard-display-european'.
;;;	09/09/23	P. Madeira	Added require
;;;					  'cl
;;;					Added definitions
;;;					  WINDOWS-TRANSFORM-ITEMS-1
;;;					  WINDOWS-TRANSFORM-ITEMS
;;;					Changed definitions
;;;					  SC-POPUP-MENU
;;;					  `standard-display-european' -> `set-language-environment'
;;;					  GLOBAL-SET-KEY [F3]
;;;					  GLOBAL-SET-KEY [F4]
;;;					  X-SC-COPY-KILL-YANK
;;;					  SPLIT-WINDOW-VERTICALLY
;;;					Commented mouse-1 key bindings
;;;					Commented iso-accents mode code
;;;	09/09/23	P. Filipe	Changed definitions
;;;					  SET-FONT-SELECT
;;;	10/01/27	Rui Patrocinio	Changed character encoding to be
;;;					"Latin 1" in all emacs versions
;;;	82/06/20	P. Madeira	Added definitions
;;;					  BRING-TO-FRONT
;;;-----------------------------------------------------------------------------

(require 'cl)

(defun sc-make-menu-item (name values)
  (cons name values))

(defun sc-menu-item-name (item)
  (car item))

(defun sc-menu-item-value (item)
  (cdr item))

;;;-----------------------------------------------------------------------------
;;;SC-EXECUTE-MENU-ITEM
;;;Description
;;;	Executes a menu item.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{arg} is an Emacs event.
;;;		
;;;		\arg{value} is a \emph{symbol} or a \emph{list}.
;;;		If it is a symbol, it must be the name of a function. The function
;;;		can have one or no arguments. If it has one argument it is called
;;;		with the value of \arg{arg}.
;;;		If it is a list, the first element must be a symbol that names a
;;;		function. The remaining elements are the arguments to be supplied
;;;		to the function. However, if the second element of the list is the
;;;		symbol \emph{arg}, the value of \arg{arg} is supplied instead.
;;;		
;;;	\return-types
;;;		The value of the evaluation.
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-execute-menu-item (arg choice)
  (if (consp choice)
      (if (and (cdr choice)
	       (eq (cadr choice) 'arg))
	  (apply (car choice) arg (cddr choice))
	  (eval choice))
      (let ((def (symbol-function choice)))
	;; Handle symbols aliased to other symbols.
	(setq def (indirect-function def))
	;; If definition is a macro, find the function inside it.
	(if (eq (car-safe def) 'macro)
	    (setq def (cdr def)))
	(let ((arglist (cond ((byte-code-function-p def)
			      (car (append def nil)))
			     ((eq (car-safe def) 'lambda)
			      (nth 1 def))
			     (t t))))
	  (if (and (listp arglist)
		   (car arglist))
	      (funcall choice arg)
	      (funcall choice))))))

;;;-----------------------------------------------------------------------------
;;;SPLIT-MENU-ITEMS
;;;Description
;;;	It splits the list of items in groups of items with maximum length of 
;;;	\arg{max} and present those as a submenu
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{title} is a \elem{string}.
;;;		
;;;		\arg{items} is a \elem{menu-item}.
;;;		
;;;		\arg{max} is a \elem{fixnum}.
;;;		
;;;	\return-types
;;;		A list of lists of menu-items.
;;;History
;;;	Date		Author		Description
;;;	01/07/04	J. P. Varandas	For each group of items it presents the 
;;;					first and last names of that group instead 
;;;					of a number of items
;;;-----------------------------------------------------------------------------
(defun split-menu-items (title items max)
  (if (> (length items) max)
      (let ((new-items nil)
	    (i 0)
	    (n 1)
	    (list nil))
	(dolist (item items)
	  (if (= i max)
	      (progn
		(setq new-items (cons (cons (format "%s - %s" (car (rac list)) (car (car list))) (nreverse list)) new-items))
		(setq list nil)
		(setq i 0)
		(setq n (+ n 1))))
	  (setq list (cons item list))
	  (setq i (+ i 1)))
	(if list
	    (setq new-items (cons (cons (format "%s - %s" (car (rac list)) (car (car list))) (nreverse list)) new-items)))
	(nreverse new-items))
      items))


;;;-----------------------------------------------------------------------------
;;;WINDOWS-TRANSFORM-ITEMS-1
;;;Description
;;;	Double ampersand for Windows Emacs 21, implementation.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{item} is anything.
;;;		
;;;	\return-types
;;;		If \arg{item} is a \emph{string}, the same string with every
;;;		ampersand doubled.
;;;		If \arg{item} is a \emph{cons}, it recurses.
;;;		Otherwise, the \arg{item} itself.
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/09/23	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defun windows-transform-items-1 (item)
  (cond ((stringp item)
	 (replace-regexp-in-string "&" "&&" item t))
	((consp item)
	 (cons (windows-transform-items (car item))
	       (if (consp (cdr item))
		   (mapcar #'(lambda (sub-item)
			       (windows-transform-items sub-item))
			   (cdr item))
		   (windows-transform-items (cdr item)))))
	(t item)))


;;;-----------------------------------------------------------------------------
;;;WINDOWS-TRANSFORM-ITEMS
;;;Description
;;;	Double ampersand for Windows Emacs 21, entry point.
;;;
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{items} is a \emph{list} or \emph{string}.
;;;		
;;;	\return-types
;;;		If \arg{item} is a \emph{string}, the same string with every
;;;		ampersand doubled. Otherwise, the \arg{item} itself.
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/09/23	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defun windows-transform-items (item)
  (if (and item (<= emacs-major-version 21) (eq system-type 'windows-nt))
      (windows-transform-items-1 item)
      item))


;;;-----------------------------------------------------------------------------
;;;SC-POPUP-MENU
;;;Description
;;;	Menu items are (<string> . <value>)
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Created
;;;	09/09/23	P. Madeira	Use `windows-transform-items'
;;;-----------------------------------------------------------------------------
(defun sc-popup-menu (arg title items &optional multiple split)
  (if (and split
	   (> (length items) split))
      (setq items (split-menu-items title items split)
            multiple t))
  (if multiple
      (setq items (cons title items))
      (setq items (list title (cons title items))))
  (x-popup-menu arg (windows-transform-items items)))

;;;-----------------------------------------------------------------------------
;;;SC-DIALOG
;;;Description
;;;	Buttons are (<string> . <value>) or nil
;;;History
;;;	Date		Author		Description
;;;	98/11/04	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun sc-dialog (arg title strings buttons)
  (let ((items nil))
    (dolist (str strings)
      (push (sc-make-menu-item str 'sc-none) items))
    (dolist (button buttons)
      (push (sc-make-menu-item (car button) (cdr button)) items))
    (setq items (nreverse items))
    (loop
     (let ((result (x-popup-menu arg (list title (cons title items)))))
       (if (not (eq result 'sc-none))
	   (return result))))))

;; ------------------------------------------------------------------------------------------
;;                               Marking regions and S-expressions
;; ------------------------------------------------------------------------------------------
(defun sc-mark-sexp (arg)
  "Set mark ARG sexps from point."
  (interactive "p")
  (cond ((char-equal (char-after (point)) 41)
	 (goto-char (1+ (point)))
	 (backward-sexp arg)
	 (save-excursion
	   (forward-sexp arg)
	   (point)))
	((char-equal (char-after (point)) 40)
	 (save-excursion
	   (forward-sexp arg)
	   (point)))
	((char-equal (char-after (1- (point))) 41)
	 (backward-sexp arg)
	 (save-excursion
	   (forward-sexp arg)
	   (point)))
	(t (if (not (find (char-after (1- (point))) '(32 40 41) :test '=))
	       (backward-sexp arg))
	   (save-excursion
	     (forward-sexp arg)
	     (point)))))

(defun x-sc-mark-sexp (click)
  "Moves point to current mouse position. Then marks the S-Expression.
Finaly, blinks at the end of the marked region."
  (interactive "e")
  (mouse-set-point click)
  (let ((mark (sc-mark-sexp nil)))
    (push-mark mark nil t)))

(defvar *last-copy-kill-yank* nil)

;;;-----------------------------------------------------------------------------
;;;X-SC-COPY-KILL-YANK
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas  Sets the 'transient-mark-mode' to nil
;;;	09/09/23	P. Madeira	Sets the `deactivate-mark' to nil instead (EMACS 23)
;;;-----------------------------------------------------------------------------
(defun x-sc-copy-kill-yank (click)
  "Several actions can be performed. See comments in the code."
  (interactive "e")
  (if mark-active
      (cond ((equal *last-copy-kill-yank* (list (current-buffer) (point) (mark)))
	     ;; If it is the second time the text is deleted from the buffer
	     (delete-region (point) (mark))
	     (setq *last-copy-kill-yank* nil))
	    (t 
	     ;; If it is the first time the marked text is saved to the kill ring
	     (copy-region-as-kill (point) (mark))
	     (setq *last-copy-kill-yank* (list (current-buffer) (point) (mark)))
	     (setq deactivate-mark nil)))
      ;; Otherwise yank the text in the kill ring
      (yank)))

(defun x-sc-set-mark (click)
  "If mouse position is different from current point sets a mark at the mouse position.
Blinks at the marked position for 1 second."
  (interactive "e")
  (let ((pos (posn-point (event-start click))))
    (push-mark pos nil t)
    (or (and transient-mark-mode
	     (eq (framep (selected-frame)) 'x))
	(let ((point-save (point)))
	  (goto-char pos)
	  (sit-for 1)
	  (goto-char point-save)))))

;;;-----------------------------------------------------------------------------
;;;X-SC-SET-POINT
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Set the function '(interactive "e")'
;;;-----------------------------------------------------------------------------
(defun x-sc-set-point (arg)
  "Sets the point at the current mouse position.
Checks if the new current position is over a parentheses. If so, blink
the corresponding opening or closing parentheses."
  ;; Set point at the mouse position
  (interactive "e")
  (mouse-set-point arg)
  (let ((point-save (point)) ;; Save current point
	(char (char-after (point)))) ;; Get char under point
    ;; If char is a parentheses
    (if (and char (or (= char 40) (= char 41)))
	(progn
	  (cond ((= char 40) ;; Open parentheses
		 (forward-sexp)) ;; Move to corresponding close parentheses
		((= char 41) ;; Close parentheses
		 (goto-char (+ point-save 1)) ;; Move one position
		 (backward-sexp))) ;; Move backward to corresponding open parentheses
	  ;; Blink for 1 second
	  (sit-for 1)
	  ;; Get back to the new point
	  (goto-char point-save)
	  (mark t)))))

(defun x-sc-mouse-drag-region (start-event)
  (interactive "e")
  (setq *last-copy-kill-yank* nil)
  (mouse-drag-region start-event))

(defun x-sc-ignore (event)
  (interactive "e")
  nil)


;;; -----------------------------------------------------------------
;;; misc-extensions.el

;; Note: you must have set your load-path (see first command in this
;; file) for this to work!
(load "misc-extensions")

;; Set indentation for Common Lisp
(setq lisp-indent-function 'common-lisp-indent-function)

;; Some useful functions which delete excess whitespace
(global-set-key "\C-x " 'delete-forward-whitespace) 
(rebind-keys-which-call 'just-one-space 'my-just-one-space) ;typically M-spc

;; C-M-d parallel to C-d, M-d for consistency:
(global-set-key "\M-\C-d" 'kill-sexp)

;; C-M-s repositions window with point or defun at top
(global-set-key "\M-\C-s" 'reposition-point-at-top)
(define-key lisp-mode-map "\M-\C-s" 'reposition-defun-at-top)
(define-key emacs-lisp-mode-map "\M-\C-s" 'reposition-defun-at-top)

(define-key lisp-mode-map "\e(" 'grow-list-forward)
(define-key emacs-lisp-mode-map "\e(" 'grow-list-forward)

;; Grep for symbol nearest mouse in *.lisp files.
(define-key lisp-mode-map "\M-\C-g" 'cl-grep-for-symbol)

;; Switch bindings so that standard carriage return indents the new line:
(define-key lisp-mode-map "\C-m" 'newline-and-indent)
(define-key lisp-mode-map "\n" 'newline)     ;this is control-<cr> on Suns
(define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
(define-key emacs-lisp-mode-map "\n" 'newline)

;; The next 4 expressions fix comment paragraph filling in lisp:
(rebind-keys-which-call 'fill-paragraph 'lisp-fill-paragraph lisp-mode-map)
(rebind-keys-which-call 'fill-paragraph 'lisp-fill-paragraph emacs-lisp-mode-map)

(defun set-common-lisp-paragraph-regexps ()
  ;; Para ligar FONT-LOCK-MODE
  ;; (font-lock-mode 1)
  (setq paragraph-start
	(concat "^[ \t]*[^ ; \t]\\|^[ \t]*$\\|" (or paragraph-start "")))
  (setq paragraph-separate paragraph-start))

(add-hook 'lisp-mode-hook 'set-common-lisp-paragraph-regexps)

(add-hook 'emacs-lisp-mode-hook 'set-common-lisp-paragraph-regexps)


;;;;; Modify this function: When window is split, switch to a new buffer
;;;;; in the new window!
(defvar standard-split-window-vertically
  (symbol-function 'split-window-vertically))

;;;-----------------------------------------------------------------------------
;;;SPLIT-WINDOW-VERTICALLY
;;;Description
;;;	When spliting, switch the other window to other buffer.
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/11/14	A. Frazao	arg is optional
;;;	09/09/23	P. Madeira	Changed code to work in EMACS 23
;;;-----------------------------------------------------------------------------
(defun split-window-vertically (&optional arg)
  "Split current window into two windows, one above the other.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally."
  (interactive "P")
  (prog1
      (funcall standard-split-window-vertically arg)
    (other-window 1)
    (switch-to-buffer (other-buffer))))


;;;-----------------------------------------------------------------------------
;;;BRING-TO-FRONT
;;;Description
;;;	Brings the current Emacs frame to front.
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
;;;	82/06/20	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defun bring-to-front ()
  (select-frame-set-input-focus (selected-frame)))


;;;;; Modify definition in files.el to expand all symlinks in filename.
;;;(defvar standard-find-file-noselect (symbol-function 'find-file-noselect))

;; ------------------------------------------------------------------------------
;;                                KEYS AND OTHER
;; ------------------------------------------------------------------------------

;; We use latin 1, aka iso-8559-1
;; (if (>= emacs-major-version 22)
;;     ;; Use this definition for EMACS >= 22
;;      (set-language-environment "Latin-1")
;;     ;; Use this definition for EMACS 21 to avoid bad characters in modification mails
;;     (standard-display-european 1))

;; Calling 'standard-display-european' is causing some problems with functionalities
;; that read all the buffer (e.g. compile buffer) in Emacs 21.3.
;; Using 'set-language-environment' corrects that problem.
;; This problem was found only after using ELI 8.1 in Emacs 21.3.
;; In any case, as stated in emacs documentation, 'standard-display-european' is obsolete.
(set-language-environment "Latin-1")


(define-key global-map [down-mouse-1] 'x-sc-mouse-drag-region)

;; (global-set-key [mouse-1]        'x-sc-set-point)
;; (global-set-key [double-mouse-1] 'x-sc-set-point)
;; (global-set-key [triple-mouse-1] 'x-sc-set-point)
;; (global-set-key [S-mouse-1]      'x-sc-set-mark)
(global-set-key [S-mouse-1]      'x-sc-ignore)

(global-set-key [mouse-2]        'x-sc-mark-sexp)
(global-set-key [double-mouse-2] 'x-sc-mark-sexp)
(global-set-key [triple-mouse-2] 'x-sc-mark-sexp)


(global-set-key [S-mouse-2]      'x-sc-copy-kill-yank)
(global-set-key [mouse-3]        'x-sc-copy-kill-yank)
(global-set-key [double-mouse-3] 'x-sc-copy-kill-yank)
(global-set-key [triple-mouse-3] 'x-sc-copy-kill-yank)

(global-set-key [C-S-mouse-1] 'buffer-menu)

;;;(global-set-key [C-down-mouse-3] 'x-sc-ignore)
(global-set-key [M-mouse-1]      'x-sc-ignore)
(global-set-key [M-drag-mouse-1] 'x-sc-ignore)
(global-set-key [M-down-mouse-1] 'x-sc-ignore)
(global-set-key [M-mouse-3]      'x-sc-ignore)
(global-set-key [M-mouse-2]      'x-sc-ignore)

(define-key global-map "\M-;" 'sc-insert-commas)
(define-key global-map "\M-:" 'sc-delete-commas)

(global-set-key [f2] 'move-to-next-tab)
;;;	09/09/23	P. Madeira	Use `f3-insert-tab'
(global-set-key [f3] 'f3-insert-tab)
;;;	09/09/23	P. Madeira	Use `f4-insert-tabs'
(global-set-key [f4] 'f4-insert-tabs)

(global-set-key [C-S-down-mouse-3] 'buffer-menu)
(global-set-key [C-S-down-mouse-1] 'select-file-menu)
(global-set-key [M-down-mouse-1] 'product-applications-menu)
(global-set-key [S-down-mouse-1] 'select-modif-request-action)

(setq menu-bar-final-items '(help modes fonts))

(defvar menu-bar-modes-menu (make-sparse-keymap "Modes"))
(define-key global-map [menu-bar modes] (cons "Modes" menu-bar-modes-menu))

(define-key  menu-bar-modes-menu [fundamental-mode] '("Fundamental" . fundamental-mode))
(define-key  menu-bar-modes-menu [font-lock-mode] '("Font Lock" . font-lock-mode))
;;; The accents emulation mode is no longer necessary
;;;(define-key  menu-bar-modes-menu [iso-accents-mode] '("Iso Accents" . iso-accents-mode))
(define-key  menu-bar-modes-menu [emacs-lisp-mode] '("Emacs Lisp" . emacs-lisp-mode))
(define-key  menu-bar-modes-menu [text-mode] '("Text" . text-mode))
(define-key  menu-bar-modes-menu [cl-shell-mode] '("CL Shell" . cl-shell-mode))
(define-key  menu-bar-modes-menu [lisp-mode] '("Lisp" . lisp-mode))

(defvar menu-bar-fonts-menu (make-sparse-keymap "Fonts"))
(define-key global-map [menu-bar fonts] (cons "Fonts" menu-bar-fonts-menu))

;;;-----------------------------------------------------------------------------
;;;MENU-BAR-FONTS-MENU
;;;Description
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/04/27	A. Frazao	Added "Default" and "Select"
;;;-----------------------------------------------------------------------------
(define-key  menu-bar-fonts-menu [font5]   '("Old fixed"      . set-font5))
(define-key  menu-bar-fonts-menu [font4]   '("Lucida Bold 14" . set-font4))
(define-key  menu-bar-fonts-menu [font3]   '("Lucida Bold 12" . set-font3))
(define-key  menu-bar-fonts-menu [font2]   '("Lucida 12"      . set-font2))
(define-key  menu-bar-fonts-menu [font1]   '("Courier 12"     . set-font1))
(define-key  menu-bar-fonts-menu [font-default] '("Default"        . set-font-default))
(define-key  menu-bar-fonts-menu [font-select]  '("Select"         . set-font-select))


;;;-----------------------------------------------------------------------------
;;;SET-FONT-SELECT
;;;Description
;;;	Selects and sets a new font
;;;		
;;;History
;;;	Date		Author		Description
;;;	04/04/27	A. Frazao	Created
;;;	09/09/23	P. Filipe	Replaced `set-default-font' with `set-frame-font'
;;;-----------------------------------------------------------------------------
(defun set-font-select ()
  (interactive)
  (let ((font (w32-select-font))) ; deprecated, but x-select-font does not exist in EMACS 21
    (set-frame-font font)
    (message font)))

;;;-----------------------------------------------------------------------------
;;;SET-FONT-DEFAULT
;;;Description
;;;	Sets the default font
;;;	
;;;History
;;;	Date		Author		Description
;;;	04/04/27	A. Frazao	Created
;;;-----------------------------------------------------------------------------
(defun set-font-default ()
  (interactive)
  (mouse-set-font "-*-Courier New-normal-r-*-*-13-97-96-96-c-*-iso8859-1"))

(defun set-font1 () (interactive) (mouse-set-font "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*"))
(defun set-font2 () (interactive) (mouse-set-font "-*-Lucida Sans Typewriter-Medium-R-*-*-*-120-72-72-*-*-iso8859-1"))
(defun set-font3 () (interactive) (mouse-set-font "-*-Lucida Sans Typewriter-Bold-R-*-*-*-120-72-72-*-*-iso8859-1"))
(defun set-font4 () (interactive) (mouse-set-font "-*-Lucida Sans Typewriter-Bold-R-*-*-*-140-72-72-*-*-iso8859-1"))
(defun set-font5 () (interactive) (mouse-set-font "-misc-fixed-medium-r-normal--0-120-75-75-c-0-iso8859-1"))


;; ------------------------------------------------------------------------------
;;                                   PORTUGUESE CHARACTERS
;; ------------------------------------------------------------------------------

;;; This emulation is no longer necessary
;;(load "iso-acc")

;;(iso-accents-mode)


;;; Startup

;;;-----------------------------------------------------------------------------
;;;SC-STARTUP-HOOK
;;;Description
;;;	Hook run when Emacs has finished startup. SC-Emacs functions should add 
;;;	to this hook, not `emacs-startup-hook'
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/02	Tom Weissmann	Created
;;;-----------------------------------------------------------------------------
(defvar sc-startup-hook nil)

;;;-----------------------------------------------------------------------------
;;;'EMACS-STARTUP-HOOK
;;;Description
;;;	Adds 'sc-startup-hook' to the emacs statup hook
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	09/02/02	Tom Weissmann	Created
;;;-----------------------------------------------------------------------------
(add-hook 'emacs-startup-hook (lambda ()
                                (run-hooks 'sc-startup-hook)))
