;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2000, SISCOG - Sistemas Cognitivos Lda.
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
;;;	Parameters of Documentation module.
;;;
;;;     Parameters should be defined before Documentation module is loaded
;;;	(sc-idoc.el).
;;; History
;;;	Date		Author		Description
;;;	00/03/21	cpla    	Created.
;;;	00/04/14	cpla    	Parameter definition modified to allow
;;;					previous assignment for customization.
;;;-----------------------------------------------------------------------------


;;;; sc-doc.el parameters

(defvar %insert-mandatory-attr-key% [f9]
  "Key to insert mandatory attributes.")


;;;; sc-doc-outline.el parameters

(defvar %beep-as-switch-visibility% nil
  "When True swicth visibility operation beeps.")


(defvar %switch-visibility-key% [f11]
  "Key to switch visibility property.")


(defvar %outline-block-key% [f8]
  "Key to outline current block.")


