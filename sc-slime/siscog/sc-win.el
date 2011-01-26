;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 1999, SISCOG - Sistemas Cognitivos Lda.
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
;;;	Implementa algumas extensões do Emacs do Windows
;;;	1. Capacidade de impressão
;;;	2. Redefinição de função do Emacs associada ao C-mouse-left em que o
;;;	menu não ordena os buffers.
;;; History
;;;	Date		Author		Description
;;;	99/09/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------

(require 'ps-print)
(setq ps-lpr-command "print")
(setq ps-printer-name "//LISBOA_NT2/HPLJ8100PS")
(setq ps-paper-type 'a4)
(ps-extend-face '(font-lock-keyword-face "black" nil bold) 'MERGE)
(ps-extend-face '(font-lock-comment-face "dim gray" nil italic) 'MERGE)

;; Keyboard Combinations

;; mouse.el (não ordena os buffers)
(defun mouse-buffer-menu-alist (buffers)
  (let (tail
	(maxlen 0)
	head)
    ;;    (setq buffers
    ;;	  (sort buffers
    ;;		(function (lambda (elt1 elt2)
    ;;			    (string< (buffer-name elt1) (buffer-name elt2))))))
    (setq tail buffers)
    (while tail
      (or (eq ?\ (aref (buffer-name (car tail)) 0))
	  (setq maxlen
	    (max maxlen
		 (length (buffer-name (car tail))))))
      (setq tail (cdr tail)))
    (setq tail buffers)
    (while tail
      (let ((elt (car tail)))
	(if (/= (aref (buffer-name elt) 0) ?\ )
	    (setq head
	      (cons
	       (cons
		(format
		 (format "%%%ds  %%s%%s  %%s" maxlen)
		 (buffer-name elt)
		 (if (buffer-modified-p elt) "*" " ")
		 (save-excursion
		   (set-buffer elt)
		   (if buffer-read-only "%" " "))
		 (or (buffer-file-name elt) 
		     (save-excursion
		       (set-buffer elt)
		       (if list-buffers-directory
			   (expand-file-name
			    list-buffers-directory)))
		     ""))
		elt)
	       head))))
      (setq tail (cdr tail)))
    ;; Compensate for the reversal that the above loop does.
    (nreverse head)))
