;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2003, SISCOG - Sistemas Cognitivos Lda.
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
;;;                      Campo Grande 378, 3º
;;;                        1700-097 LISBOA
;;;                           PORTUGAL
;;;
;;;-----------------------------------------------------------------------------
;;; Description
;;;	Allows browsing the common lisp documentation
;;;	Depends on the contents of the varoable *lisp-doc-home*.
;;;	After loading this file, the function set-lisp-doc-home should be called
;;;	with the directory where the documentation is placed, e.g.
;;;	(set-lisp-doc-home "i:")
;;;
;;;	Adds a menu "Lisp doc" in the menu bar for different sources of documentation
;;;
;;;	Defines the following keys that, when pressed over a symbol, looks for the page where it is described:
;;;	f1 - Uses XAnalysis Common Lisp HyperSpec
;;;	meta-f1 - Uses Common Lisp The Language, 2nd Edition
;;;	
;;; History
;;;	Date		Author		Description
;;;	03/07/02	A. Frazao	Created
;;;-----------------------------------------------------------------------------

(defvar *lisp-doc-home*)

(sc-load "cltl2")
(sc-load "hyperspec")

(defun set-lisp-doc-home (doc-home)
  ;; Should map lisp on lisboa nt2
  (setf *lisp-doc-home* doc-home)
  (setf cltl2-root-url (concat doc-home "/cltl/"))
  (setf common-lisp-hyperspec-root (concat doc-home "/HyperSpec/"))
  (setf common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
  )

;;(setq menu-bar-final-items '(help modes fonts))

(unless (member 'lisp-doc menu-bar-final-items)
  (setf menu-bar-final-items (append menu-bar-final-items '(lisp-doc))))

(defvar menu-bar-lisp-doc-menu (make-sparse-keymap "Lisp Doc"))

(define-key global-map [menu-bar lisp-doc] (cons "Lisp Doc" menu-bar-lisp-doc-menu))

(define-key  menu-bar-lisp-doc-menu [font1] '("CLtL2"           . go-to-cltl2-main-page))
(define-key  menu-bar-lisp-doc-menu [font2] '("Hyperspec"       . go-to-hyperspec-main-page))
(define-key  menu-bar-lisp-doc-menu [font4] '("Acl 5.0.1 Ansi"  . go-to-acl501-ansicl-page))
(define-key  menu-bar-lisp-doc-menu [font3] '("Acl 5.0.1 CG"    . go-to-acl501-cg-page))
(define-key  menu-bar-lisp-doc-menu [font5] '("Acl 5.0.1 Intro" . go-to-acl501-main-page))

(defun go-to-acl501-main-page ()
  (interactive)
  (browse-url (format "%s/AllegroCL5.0.1/doc/cl/introduction.htm" *lisp-doc-home*)))

(defun go-to-acl501-ansicl-page ()
  (interactive)
  (browse-url (format "%s/AllegroCL5.0.1/ansicl/ansicl.htm" *lisp-doc-home*)))

(defun go-to-acl501-cg-page ()
  (interactive)
  (browse-url (format "%s/AllegroCL5.0.1/doc/cg/index.htm" *lisp-doc-home*)))

;; Go to hyperspec main page
(defun go-to-hyperspec-main-page ()
  (interactive)
  (browse-url (format "%sFront/index.htm" common-lisp-hyperspec-root)))

;; Go to Common Lisp the language main page
(defun go-to-cltl2-main-page ()
  (interactive)
  (browse-url (format "%scltl2.html" cltl2-root-url)))

;; Go to hyperspec definition
(defun go-to-hyperspec-definition ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (if thing
	(common-lisp-hyperspec thing)
	(progn
	  (beep)
	  (message "No symbol selected")))))

;; Go to cltl2 main page
(defun go-to-cltl2-definition ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (if thing
	(cltl2-lookup thing)
	(progn
	  (beep)
	  (message "No symbol selected")))))

;; Go to hyperspec definition
(global-set-key [f1] 'go-to-hyperspec-definition)

;; Go to cltl2 definition
(global-set-key [(meta f1)] 'go-to-cltl2-definition)
