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
;;;	99/08/31	J. P. Varandas	Added definitions
;;;					  SETF AUTO-MODE-ALIST
;;;	99/09/28	A. Frazao	Loads sc-win
;;;	99/12/20	cpla    	Loads sc-doc.
;;;	01/03/02	Toni		Added auto-loading of files that
;;;					support PHP and VBScript editing modes.
;;;	01/09/18	Fausto		Added nt5.0
;;;	02/02/28	A. Frazao	Added loading of sc-grepfc
;;;	02/06/07	J. P. Varandas	Added loading of idoc-measure
;;;	03/07/02	A. Frazao	Added loading of online-doc
;;;					Changed definitions
;;;					  (SETQ LOAD-PATH)
;;;	03/07/28	A. Frazao	Added definitions
;;;					  "SC-CREWS-UTILITIES"
;;;					  "SC-USER-PARAM"
;;;					  "SC-MODELS"
;;;					  "SC-CREWS-GLOBAL-PARAM"
;;;					  "SC-RUN-CREWS-IMAGES"
;;;					  "SC-INSTALL-CREWS-IMAGES"
;;;					  "SC-UPDATE-CREWS-CODE"
;;;					  "SC-MAIN-UTILITIES-MENU"
;;;					Deleted definitions
;;;					  "SC-MOD-PARAM"
;;;	05/08/05	Fernando	Changed definitions
;;;					  "SC-MAIL"
;;;					  (SETQ LOAD-PATH)
;;;	06/04/06	J. P. Varandas	Changed definitions
;;;					  Changed the settings of the global variables for Emacs version nt5.1
;;;	06/04/11	J. P. Varandas	Loads the Emacs module "EDIFF" and redefines 
;;;					the function EDIFF-CONVERT-STANDARD-FILENAME 
;;;					afterwards
;;;					Changed definitions
;;;					  SC-LOAD
;;;					  (SETF AUTO-MODE-ALIST)
;;;					Added definitions
;;;					  EDIFF-CONVERT-STANDARD-FILENAME
;;;	06/08/24	J. P. Varandas	Deleted definitions
;;;					  *COMPILE-EXTENSION*
;;;					  *SOLARIS-XEMACS*
;;;					  *SOLARIS-EMACS*
;;;					Added definitions
;;;					  *WINDOWS-EMACS21*
;;;					Comment definitions
;;;					  EDIFF-CONVERT-STANDARD-FILENAME
;;;					Changed definitions
;;;					  BYTE-COMPILE-DEST-FILE
;;;	07/12/28	J. P. Varandas	Added nt6.0
;;;	08/07/08	A. Frazao	Added definitions
;;;					  "SC-INSTALL-CREWS-IMAGES-OLD"
;;;	08/10/13	Tom Weissmann	Changed definitions
;;;					  'LOAD-PATH
;;;	09/02/10	J. P. Varandas	Changed modules names
;;;					  sc-crews-global-param -> sc-global-param
;;;					  sc-crews-utilities -> sc-product-utilities
;;;					  sc-run-crews-images -> sc-run-images
;;;					  sc-install-crews-images -> sc-install-images
;;;					Added load of module
;;;					  sc-install-product-images
;;;	10/01/26	Rui Patrocinio	Changed definitions
;;;					  'LOAD-PATH
;;;	82/06/20	P. Madeira	Changed definitions
;;;					  'LOAD-PATH
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; 
;;; 
;;; THE FILE IS DIVIDED IN THE FOLLOWING SECTIONS (IN THE APPEARING ORDER):
;;;	- LOADING GENERAL STUFF
;;;	- GLOBAL VARIABLES
;;;	- SUPPORT OF ADDITIONAL MODES AND FILE EXTENSIONS
;;;	- UTILITIES
;;;	- LOADING 'CHANGE REQUEST MANAGER' SPECIFIC FILES
;;;	- CHANGING CHARACTERS SYNTAX FOR EMACS INTERPRETATION
;;; 
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; 
;;; LOADING GENERAL STUFF
;;; 
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

(load "cl")

(load "cl-macs")

(load "cl-extra")

;;;(load "ediff")

;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; 
;;; GLOBAL VARIABLES
;;; 
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

(defvar *linux-emacs* nil)
(defvar *windows-emacs* nil)

(if (eq 'windows-nt system-type)
    (setq *windows-emacs* t)
    (setq *linux-emacs* t))


;;;-----------------------------------------------------------------------------
;;;*WINDOWS-EMACS21*
;;;Description
;;;	Indicates if SC-EMACS is running with Emacs version 21.3
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	06/08/24	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defvar *windows-emacs21* nil)

(if (search "GNU Emacs 21.3" (emacs-version))
    (setf *windows-emacs21* t))

    
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; 
;;; SUPPORT OF ADDITIONAL MODES AND FILE EXTENSIONS
;;; 
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;(SETF AUTO-MODE-ALIST)
;;;Description
;;;	Adds file extentions to be deal by lisp-mode
;;;History
;;;	Date		Author		Description
;;;	99/08/31	J. P. Varandas	Created
;;;	06/04/11	J. P. Varandas	Added bml file type
;;;-----------------------------------------------------------------------------
(setf auto-mode-alist (cons '("\\.bil\\'" . lisp-mode)
			    (cons '("\\.bml\\'" . lisp-mode)
				  (cons '("\\.lpr\\'" . lisp-mode) 
					auto-mode-alist))))

;;------------------------------------------------------------------------------
;; Loads the PHP editing mode.
;;------------------------------------------------------------------------------

(autoload 'php-mode (format "%s/web/%s" (getenv "SISCOG_EMACS_DIR") "php-mode.el") "PHP mode." t)
(setq auto-mode-alist (append '(("\\.\\(php\\|php3\\|inc\\)$" . php-mode))
			      auto-mode-alist))

;;------------------------------------------------------------------------------
;; Loads the VBScript editing mode.
;;------------------------------------------------------------------------------

(autoload 'visual-basic-mode (format "%s/web/%s" (getenv "SISCOG_EMACS_DIR") "visual-basic-mode.el")
	  "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|asp\\)$" .
				 visual-basic-mode)) auto-mode-alist))

;; Commented since I don't understand the logic and the need of this.
;;(autoload 'vbp-mode (format "%s/web/%s" (getenv "SISCOG_EMACS_DIR") "visual-basic-mode.el") "VBP mode." t)
;;(setq auto-mode-alist (append '(("\\.\\(vbg\\|vbg\\)$" . vbp-mode))
;;			      auto-mode-alist))

(autoload 'vbp-mode (format "%s/web/%s" (getenv "SISCOG_EMACS_DIR") "vbp-mode.el") "VBP mode." t)
(setq auto-mode-alist (append '(("\\.vbp$" . vbp-mode)) auto-mode-alist))

;; Commented since I haven't VB installed.
;;(setq visual-basic-ide-pathname "d:/Program Files/DevStudio/VB/VB5.EXE")        ;; Installation dependent


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; 
;;; UTILITIES
;;; 
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;'LOAD-PATH
;;;Description
;;;	List of pathnames where system shall search for the files to load.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/02	A. Frazao	Added cl-hyperspec
;;;	05/08/05	Fernando	Added sc-mail, the folder with the mail tool adopted and adapted by SISCOG
;;;	06/08/24	J. P. Varandas	Removed the load-path for ELI-X and add for ELI21-WIN
;;;					If '*linked-to-server-dir*' is set then load sc-emacs from server
;;;	08/10/13	Tom Weissmann	Removed nested `cons' to aid readability.
;;;					Load latest eli code for Emacs 22.
;;;	10/01/26	Rui Patrocinio	Added eli81 and eli-patches dir (POA 16535.0)
;;;	82/06/20	P. Madeira	Added slime
;;;-----------------------------------------------------------------------------
(let* ((env-dir    (getenv "SISCOG_EMACS_DIR"))
       (server-dir (or (and (boundp '*linked-to-server-dir*) *linked-to-server-dir*)
                             env-dir)))
  (flet ((add-dir (dir root)
           (add-to-list 'load-path (expand-file-name dir root))))
    ;; add load dirs in reverse order
    ;; duplicates are deleted by `add-to-list'
    (add-dir "web"            server-dir)
    (add-dir "cl-hyperspec"   server-dir)
    (add-dir "cl-shell"       server-dir)
    (add-dir "siscog"         env-dir) ; fallback
    (add-dir "siscog"         server-dir)
    (add-dir ""               env-dir)
    ;; load eli code
    (assert (>= emacs-major-version 21) nil "Please use an Emacs version >= 21!")
    (cond ((and (boundp '*use-slime*) *use-slime*)
	   (add-dir "slime" env-dir)
	   (add-dir "slime/contrib" env-dir))
	  (t
	   (add-dir "eli81" server-dir)))
    (add-dir "eli-patches" server-dir)))


;;;-----------------------------------------------------------------------------
;;;BYTE-COMPILE-DEST-FILE
;;;Description
;;;	Convert an Emacs Lisp source file name to a compiled file name.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{filename} is a \emph{string}
;;;		
;;;	\return-types
;;;		A \emph{string}
;;;History
;;;	Date		Author		Description
;;;	06/08/24	J. P. Varandas	Removed the use of variable '*compile-extension*'
;;;-----------------------------------------------------------------------------
(defun byte-compile-dest-file (filename)
  (setq filename (file-name-sans-versions filename))
  (cond ((eq system-type 'vax-vms)
	 (concat (substring filename 0 (string-match ";" filename)) "c"))
	((string-match "\\.el$" filename)
	 (concat (substring filename 0 (match-beginning 0)) ".elc"))
	(t (concat filename ".elc"))))


(defun byte-compile-orig-file (filename)
  "Convert an Emacs Lisp source file name to a compiled file name."
  (setq filename (file-name-sans-versions filename))
  (cond ((eq system-type 'vax-vms)
	 (substring filename 0 (string-match ";" filename)))
	((string-match "\\.el$" filename)
	 (concat (substring filename 0 (match-beginning 0)) ".el"))
	(t (concat filename ".el"))))


;;;-----------------------------------------------------------------------------
;;;SC-LOAD
;;;Description
;;;	Search for the given filename on the list of directories \ref{load-path} 
;;;	and loads the found file.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{filename} is a \elem{string} with the name of the file to load.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	06/04/11	J. P. Varandas	As soon as the file is found exits from the function.
;;;-----------------------------------------------------------------------------
(defun sc-load (filename)
  (interactive)
  (let ((found nil))
    (block exit
      (dolist (dir load-path)
	(let ((file (concat dir "/" filename)))
	  (let ((compiled (byte-compile-dest-file file))
		(source (byte-compile-orig-file file)))
	    (dolist (f (list compiled source file))
	      (when (and (file-exists-p f)
			 (not (file-directory-p f)))
		(load f)
		(setq found t)
		(return-from exit)))))))
    (unless found
      (error "sc-load: Cannot find file %s" filename))))


;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; 
;;; LOADING 'CHANGE REQUEST MANAGER' SPECIFIC FILES
;;; 
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;Load modules
;;;		
;;;History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Added
;;;					  "SC-MAIN-UTILITIES-MENU"
;;;					  "SC-UPDATE-CREWS-CODE"
;;;					  "SC-INSTALL-CREWS-IMAGES"
;;;					  "SC-RUN-CREWS-IMAGES"
;;;					  "SC-CREWS-GLOBAL-PARAM"
;;;					  "SC-CREWS-UTILITIES"
;;;					  "SC-MODELS"
;;;					  "SC-USER-PARAM"
;;;					Deleted
;;;					  "SC-MOD-PARAM"
;;;					  "SC-CREWS"
;;;	05/08/05	Fernando	Added
;;;					  "SC-MAIL"
;;;	08/07/08	A. Frazao	Added
;;;					  sc-install-crews-images-old
;;;	09/02/10	J. P. Varandas	Changed names
;;;					  sc-crews-global-param -> sc-global-param
;;;					  sc-crews-utilities -> sc-product-utilities
;;;					  sc-run-crews-images -> sc-run-images
;;;					  sc-install-crews-images -> sc-install-images
;;;					Added
;;;					  sc-install-product-images
;;;-----------------------------------------------------------------------------
(sc-load "sc-allegro")
(sc-load "sc-emacs")
(sc-load "sc-win")
(load "cc-mode")

(sc-load "sc-models")
(sc-load "sc-global-param")
(sc-load "sc-util")
(sc-load "sc-product-utilities")
(sc-load "sc-mod")
(sc-load "sc-doc")
(sc-load "sc-update-crews-code")
(sc-load "sc-run-images")
(sc-load "sc-install-images")
(sc-load "sc-install-crews-images-old")
(sc-load "sc-install-product-images")
(sc-load "sc-main-utilities-menu")
(sc-load "sc-grepfc")
(sc-load "idoc-measure")
(sc-load "online-doc")
(sc-load "sc-mail")
;;(sc-load "sc-user-param")
;;(sc-load "sc-bb")

;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 
;;; 
;;; CHANGING CHARACTERS SYNTAX FOR EMACS INTERPRETATION
;;; 
;;; 
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

;; Isto destina-se a permitir seleccionar simbolos com '.' no meio quando se carrega
;; o botao do meio do rato
(modify-syntax-entry ?#  "_   " emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?.  "w   " emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?#  "_   " lisp-mode-syntax-table)
(modify-syntax-entry ?.  "w   " lisp-mode-syntax-table)

(put 'eval-expression 'disabled nil)

