;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2010, SISCOG - Sistemas Cognitivos, SA
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
;;;                 SISCOG - Sistemas Cognitivos, SA
;;;                      Campo Grande 378, 3º
;;;                        1700-097 LISBOA
;;;                           PORTUGAL
;;;
;;;-----------------------------------------------------------------------------
;;; Description
;;;	
;;; History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;	42/42/42	L. B. Oliveira	Added definitions
;;;					  SLDB-SETUP
;;;					  SLIME-OPEN-INSPECTOR
;;;					Changed definitions
;;;					  SC-SLIME-START-ACL-PROCESS
;;;-----------------------------------------------------------------------------

(eval-when-compile
  (require 'cl))

(require 'slime)
(slime-setup '(slime-fancy slime-banner slime-indentation))

(sc-load "fi-sc-manual")

(pushnew '("\\.bil$" . lisp-mode) auto-mode-alist :test 'equal)
(pushnew '("\\.bml$" . lisp-mode) auto-mode-alist :test 'equal)
(pushnew '("\\.cl$"  . lisp-mode) auto-mode-alist :test 'equal)
(pushnew '("\\.lpr$" . lisp-mode) auto-mode-alist :test 'equal)

;;; Indentation settings
(define-cl-indent '(make-instance 1))
(define-cl-indent '(make-window 1))


;; ------------------------------------------------------------------------------
;;                               ALLEGRO COMMON LISP
;; ------------------------------------------------------------------------------
(setq allegro-common-lisp-host            (system-name))
(setq allegro-common-lisp-directory       (getenv "HOME"))
;;(setq allegro-common-lisp-buffer-name     "*Allegro CL*")
(setq allegro-common-lisp-image-arguments nil)
(setq allegro-common-lisp-image-name      nil) ; EXE
(setq allegro-common-lisp-image-file      nil)


;;;-----------------------------------------------------------------------------
;;;SLIME-EOL-CONVERSION-FIXUP
;;;Description
;;;	ACL >= 8.1 doesn't need end-of-line correction.
;;;		
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defadvice slime-eol-conversion-fixup (around sc-slime-eol-conversion-fixup-for-acl>=8.1 activate)
  (if (and (not (boundp 'notes)) ; compilation notes
	   (string-equal "allegro" (slime-lisp-implementation-name (slime-connection)))
	   (>= (string-to-number (slime-lisp-implementation-version (slime-connection))) 8.1))
      (setq ad-return-value 0)
      ad-do-it))


;;;-----------------------------------------------------------------------------
;;;SC-SLIME-START-ACL-PROCESS
;;;Description
;;;	Start swank by setting some env-vars that are used in swank-init.lisp.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{image} is a \emph{string} representing a file name.
;;;		
;;;		\arg{args} is a \emph{list} of \emph{string}.
;;;		
;;;	\return-types
;;;		A \emph{process} or nil.
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;	42/42/42	L. B. Oliveira	Massage start-process invocation.
;;;-----------------------------------------------------------------------------
(defun sc-slime-start-acl-process (image args)
  (let* (;; Avoid Emacs argument quoting, it's very erratic
	 (w32-quote-process-args nil)
	 ;; Set the needed env-vars
	 (process-environment (nconc (list (concat "SLIME_LOADER" "=" (expand-file-name slime-backend slime-path))
					   (concat "SLIME_SWANK_PORT_FILE" "=" (slime-swank-port-file))
					   (concat "SLIME_NET_CODING_SYSTEM" "=" (slime-coding-system-cl-name slime-net-coding-system)))
				     process-environment))
	 ;; Actually start ACL
	 (acl-process (apply 'start-process
			     "common-lisp"
			     (generate-new-buffer-name " common-lisp")
			     allegro-common-lisp-image-name
                             ;; HERE BE DRAGONS.  Allegro's argument parsing
                             ;; seems to be buggy. Adding a random characters
                             ;; that don't change semantics (like quotes and
                             ;; spaces) to one of the arguments usually fixes
                             ;; any crashes that come up.
                             (list* (concat "-I \"" image "\"")
                                    "-e (load(concatenate'string(sys:getenv\"SISCOG_EMACS_DIR\")\"/siscog/swank-init\"))"
                                    args))))
    (with-current-buffer (process-buffer acl-process)
      (let ((lisp-name (make-symbol
			(concat (file-name-sans-extension image)
				"." (or (file-name-extension image) "dxl")))))
	;; KLUDGE: but a very useful one, to distinguish each connection based
	;;         on the image name and to store the used arguments.
	(make-local-variable 'slime-inferior-lisp-args)
	(setq slime-inferior-lisp-args
	      (nconc (list :name lisp-name)
		     slime-inferior-lisp-args))))
    acl-process))


;;;-----------------------------------------------------------------------------
;;;RUN-ALLEGRO-LISP-IMAGE
;;;Description
;;;	Runs ACL with the given image and connects to it.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{image} is a \emph{string} representing a file name.
;;;		
;;;		\arg{args} is a \emph{list} of \emph{string}.
;;;		
;;;	\return-types
;;;		A \emph{process} or nil.
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defun run-allegro-lisp-image (image args)
  (let* ((acl-process (sc-slime-start-acl-process (if image
						      (format "%s.dxl" image)
						      allegro-common-lisp-image-file)
						  args))
	 (new-image-running nil)
	 (original-banner-function slime-repl-banner-function)
	 (slime-repl-banner-function `(lambda ()
					(funcall original-banner-function)
					(setq new-image-running t))))
    (slime-read-port-and-connect acl-process nil)
    (while (and (eq (process-status acl-process) 'run)
		(not new-image-running))
      (accept-process-output nil 1))
    acl-process))


;;;-----------------------------------------------------------------------------
;;;RUN-COMMON-LISP
;;;Description
;;;	Runs ACL with the default image and connects to it.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{process} or nil.
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defun run-common-lisp ()
  (interactive)
  (run-allegro-lisp-image allegro-common-lisp-image-file
			  allegro-common-lisp-image-arguments))


;;;-----------------------------------------------------------------------------
;;;GOTO-ALLEGRO-LISP-BUFFER
;;;Description
;;;	Switches to the current connection's REPL buffer.
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
(defun goto-allegro-lisp-buffer ()
  "Makes the *lisp* buffer the current buffer, running lisp if necessary"
  (interactive)
  (call-interactively 'slime-switch-to-output-buffer))


(define-key slime-inspector-mode-map (kbd "<mouse-4>") 'slime-inspector-pop)
(define-key slime-inspector-mode-map (kbd "<mouse-5>") 'slime-inspector-next)


;;; For find definition support in swank-init.lisp
(setq slime-ed-use-dedicated-frame nil)

;;;-----------------------------------------------------------------------------
;;;SLIME-ED
;;;Description
;;;	Bring Emacs frame to front after editing command.
;;;	
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defadvice slime-ed (after slime-ed-focus-frame activate)
  (bring-to-front))


;;;-----------------------------------------------------------------------------
;;;SLIME-OPEN-INSPECTOR
;;;Description
;;;	Raises the Emacs frame when the inspector is invoked.
;;;		
;;;History
;;;	Date		Author		Description
;;;	42/42/42	L. B. Oliveira	Created
;;;-----------------------------------------------------------------------------
(defadvice slime-open-inspector (after slime-raise-inspector activate)
  (raise-frame))


;;;-----------------------------------------------------------------------------
;;;SLDB-SETUP
;;;Description
;;;	Raises the Emacs frame when the debugger is invoked.
;;;		
;;;History
;;;	Date		Author		Description
;;;	42/42/42	L. B. Oliveira	Created
;;;-----------------------------------------------------------------------------
(defadvice sldb-setup (after slime-raise-debugger activate)
  (raise-frame))


;;;-----------------------------------------------------------------------------
;;;REPL-NEWLINE
;;;Description
;;;	Simulates the user action that evaluates a form in the REPL.
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
(defun repl-newline ()
  (slime-repl-return))


;;;-----------------------------------------------------------------------------
;;;LISP-CONNECTION-CLEANUP
;;;Description
;;;	Generates the necessary Common-Lisp form to cleanup the connection's
;;;	side effects from the running image as much as possible.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{tree} representing a LISP form.
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defun lisp-connection-cleanup ()
  `(progn
     (loop
       while swank::*connections*
       do (swank::close-connection (first swank::*connections*) nil nil))
     (setf *debugger-hook* nil)
     (dolist (package (list-all-packages))
       (let ((mismatch (mismatch (package-name package) "swank" :test 'char-equal)))
	 (when (or (null mismatch) (= mismatch 5))
	   (delete-package package))))))


;;;-----------------------------------------------------------------------------
;;;LISP-PROCESS
;;;Description
;;;	Returns the current Lisp process.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{process}.
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defun lisp-process ()
  (slime-process))
