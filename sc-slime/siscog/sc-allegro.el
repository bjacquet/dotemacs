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
;;;	98/11/26	A. Frazao	Added some comments
;;;	99/07/23	A. Frazao	Added definitions
;;;					  (put 'if 'fi:lisp-indent-hook 3)
;;;	99/08/27	A. Frazao	Changed definitions
;;;					  RUN-ALLEGRO-LISP-COMMAND
;;;	99/09/28	A. Frazao	Added definitions
;;;					  SC-FIND-DEFINITION
;;;					  SC-FIND-STRING-IN-FILE
;;;					  SC-EVAL-IN-LISP-ASYNCHRONOUS
;;;	00/01/31	J. P. Varandas	c:/Program Files/acl50 -> c:/Program Files/acl501
;;;	00/02/18	J. P. Varandas	Added definitions
;;;					  SET-INTERNATIONAL-SIMPLE-LISP
;;;					  SET-SIMPLE-LISP
;;;					  SET-INTERNATIONAL-ALLEGRO
;;;					  SET-NORMAL-ALLEGRO
;;;	00/02/24	A. Frazao	Added definitions
;;;					  FI::INSTALL-MODE-MENUS
;;;	05/11/07	A. Frazao	Changed definitions
;;;					  SET-NORMAL-ALLEGRO
;;;	06/08/24	J. P. Varandas	Added definitions for Emacs 21
;;;					  FI::ADD-COMMON-LISP-POPUP-MENU-ITEMS
;;;					Removed for Emacs 21 the hook 'fi:show-run-status' from 'fi:start-lisp-interface-hook'
;;;					Changed definitions
;;;					  RUN-ALLEGRO-LISP-COMMAND
;;;					  SET-NORMAL-ALLEGRO
;;;	07/12/28	J. P. Varandas	Set 'allegro-common-lisp-directory' to (getenv "HOME")
;;;	08/10/13	Tom Weissmann	Adjust `fi:start-lisp-interface-arguments' for
;;;					ELI versions >= 3. 
;;;	09/02/02	J. P. Varandas	Deleted definitions
;;;					  SET-NORMAL-ALLEGRO
;;;					  SET-INTERNATIONAL-ALLEGRO
;;;					  SET-SIMPLE-LISP
;;;					  SET-INTERNATIONAL-SIMPLE-LISP
;;;					  SC-FIND-STRING-IN-FILE
;;;					  SC-FIND-DEFINITION
;;;					  SC-FIND-STRING-IN-FILE
;;;					  SC-FIND-DEFINITION
;;;					  SC-EVAL-IN-LISP-ASYNCHRONOUS
;;;	09/02/13	J. P. Varandas	Deleted definitions
;;;					  FI::INITIALIZE-POPUP-MENU-MAP
;;;					Added definitions
;;;					  FI::ADD-COMMON-LISP-POPUP-MENU-ITEMS
;;;     10/01/26	Rui Patrocinio  Added ELI patches
;;;	82/06/20	P. Madeira	Moved ELI definitions to sc-allegro-eli.el.
;;;	10/11/18	P. Madeira	Load fi-sc-manual
;;;-----------------------------------------------------------------------------

;; ------------------------------------------------------------------------------
;;                               ALLEGRO COMMON LISP
;; ------------------------------------------------------------------------------
(setq allegro-common-lisp-host            (system-name))
(setq allegro-common-lisp-directory       (getenv "HOME"))
(setq allegro-common-lisp-buffer-name     "*Allegro CL*")
(setq allegro-common-lisp-image-arguments nil)
(setq allegro-common-lisp-image-name      nil) ; EXE
(setq allegro-common-lisp-image-file      nil)

(cond ((and (boundp '*use-slime*) *use-slime*)
       (sc-load "sc-allegro-slime"))
      (t
       (sc-load "sc-allegro-eli")))

;;;-----------------------------------------------------------------------------
;;;RUN-ALLEGRO-LISP-COMMAND
;;;Description
;;;	
;;;History
;;;	Date		Author		Description
;;;	99/08/27	A. Frazao	Run asyncronous in windows (with &)
;;;	06/08/24	J. P. Varandas	Removed obsolete code for Solaris
;;;-----------------------------------------------------------------------------
(defun run-allegro-lisp-command (image args)
  (let* ((dir (file-name-directory allegro-common-lisp-image-name))
	 (program (file-name-nondirectory allegro-common-lisp-image-name))
	 (image-file (if image
			 (format "%s.dxl" image)
			 allegro-common-lisp-image-file))
	 (str ""))
    (dolist (arg args)
      (setq str (format "%s %s" str arg)))
    (cd dir)
    (cond (*windows-emacs*
	   (shell-command (format "%s -I '%s' %s &" program image-file str)))
	  (*linux-emacs*
	   (shell-command (format "xterm -e %s -I '%s' %s" allegro-common-lisp-image-name image-file str)))
	  )))


;; Keybindings
(global-set-key "\M-\C-a" 'goto-allegro-lisp-buffer)

;; Recent versions of ELI do not work with SISCOG images because they rely on command line
;; arguments -f and -L to control starting the Lisp listener: SISCOG images eat these
;; arguments, so the Lisp listener never starts, and Emacs can't connect.
;;
;; Creating the arguments to control the Lisp listener is itself controlled by the
;; variable `fi:start-lisp-interface-arguments', whose value is a function.
;; We do not have to roll our own function because on unix ELI uses the -e argument (which
;; passes a lisp form to evaluate), and this argument is unaffected by SISCOG images.
;;
;; See http://www.franz.com/support/documentation/8.1/doc/startup.htm#command-line-args-1
;; or a more recent version, for information on command-line arguments

;; This is currently necessary on Emacs >= v22, which needs ELI version >= v3
;; (let ((eli-major-version (string-to-number fi:emacs-lisp-interface-version)))
;;   (when (>= eli-major-version 3)
;;     (setq fi:start-lisp-interface-arguments 'fi::start-lisp-interface-unix)))

