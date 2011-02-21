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
;;;-----------------------------------------------------------------------------

(cl:in-package #:cl-user)

;;; HACK. To be fixed in SLIME. Images with SWANK have this variable pointing to
;;; the original home dir which won't exist if we try to run them on a different
;;; machine.
(when (find-package "SWANK-LOADER")
  (makunbound (find-symbol "*FASL-DIRECTORY*" "SWANK-LOADER")))

;;; All env-vars are set when starting a lisp in slime-init.el
(load (sys:getenv "SLIME_LOADER"))

;;; This might be fixed in SC-EMACS proper, someday.
(setf lep::*connection* nil)

(defparameter *sc-emacs-bypass-hotspot* nil
  "Boolean switch to toggle bypassing of hotspot objects.")

(defparameter *sc-emacs-inspect-in-emacs* nil
  "Boolean switch to toggle the SLIME inspector.")

(swank-loader:init
 :delete t         ; delete any existing SWANK packages
 :reload t         ; reload SWANK, even if the SWANK package already exists
 :load-contribs t) ; load all contribs

(def-fwrapper sc-emacs-inspect-wrapper (object)
  "Wrapper around CL:INSPECT that will inspect in Emacs if such a
connection is established."
  (if (or (null *sc-emacs-inspect-in-emacs*)
	  (null swank::*connections*))
      (call-next-fwrapper)
      (let ((swank::*buffer-package* (find-package :cl))
	    (swank::*buffer-readtable* *readtable*))
	(swank:inspect-in-emacs
	 (if (and *sc-emacs-bypass-hotspot*
		  (typep object (find-symbol "HOTSPOT" "WINGRAPHICS")))
	     (funcall (find-symbol "OBJECT" "MAPS") object)
	     object)))))

(fwrap 'cl:inspect 'sc-emacs-inspect-wrapper 'sc-emacs-inspect-wrapper)

;;; When using ELI, ACL knows if it should open files in EMACS
;;; Here, we teach it to do so also when using SLIME
(excl:def-fwrapper show-source-code-in-slime (definition)
  (when (typep definition 'ide.find-definitions:definition)
    (if (and (ide:open-files-in-gnu-emacs (cg:configuration ide:*ide-system*))
	     (swank::default-connection))
	(let* ((fspec (ide.trace::function-spec-from-definition definition))
	       (full-name (ide.find-definitions::full-name definition))
	       (def (or fspec full-name)))
	  (if def
	      (let ((*package* (find-package '#:cl-user)))
		(swank:ed-in-emacs def))
	      (cg:message-box (format nil "Cannot find definition ~s."
				      definition)
			      "Swank")))
	(excl:call-next-fwrapper))))

(excl:fwrap 'ide.find-definitions::show-source-code
	    :show-source-code 'show-source-code-in-slime)

;;; Some images have funky sys:*temporary-directory*

(def-fwrapper make-temp-file-name-wrapper (&optional prefix directory)
  (if (null directory)
      (sys:make-temp-file-name (or prefix "") (sys:getenv "TEMP"))
      (call-next-fwrapper)))

(fwrap 'system:make-temp-file-name
       :make-temp-file-name-wrapper 'make-temp-file-name-wrapper)

;;; ACL has performance issues if this is not set
(setf swank:*use-dedicated-output-stream* t)

(swank:start-server (sys:getenv "SLIME_SWANK_PORT_FILE")
		    :coding-system (sys:getenv "SLIME_NET_CODING_SYSTEM")
		    :dont-close t)
