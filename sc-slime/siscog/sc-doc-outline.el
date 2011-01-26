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
;;;	Defines the menu for outlining internal documentation.
;;; History
;;;	Date		Author		Description
;;;	99/11/11	cpla	        Created.
;;;	00/03/21	cpla    	Changed
;;;					SWITCH.VISIBILITY
;;;					MENU-ENABLE
;;;-----------------------------------------------------------------------------


;;;(defvar ovl-local-map (make-sparse-keymap))
;;;(define-key ovl-local-map [f6] 'switch.visibility)


;;;(put 'ovl_comment 'priority 101)
;;;(put 'ovl_comment 'evaporate t)
;;;(put 'ovl_comment 'local-map ovl-local-map)

(put 'ovl_description 'priority 100)
(put 'ovl_description 'evaporate t)

(put 'ovl_sub_description 'priority 99)
(put 'ovl_sub_description 'evaporate t)

(put 'ovl_history 'priority 100)
(put 'ovl_history 'evaporate t)


(defvar +categories+ '(;;ovl_comment
		       ovl_description ovl_sub_description ovl_history))


(defvar +alternative-rexp+ "\\|")


(defvar +default-visibility-state+ t)


(defun switch.value (v)
  (not v))


(defun boolean.to.on.off (b)
  (if b
      "On"
      "Off"))


(defun switch.visibility ()
  "Switches the invisible property of the inner overlay at point." 
  (interactive)
  (save-excursion
    (let ((ovl (select.inner.overlay (append (overlays-at (point))
					     (and (not (eolp))
						  (or (end-of-line) t)
						  (overlays-at (point)))
					     (overlays-at (1- (point)))))))
      (when ovl
	(overlay-put ovl 'invisible (switch.value (overlay-get ovl 'invisible)))
	(message (format "Visibility %s" (boolean.to.on.off (not (overlay-get ovl 'invisible)))))
	(when %beep-as-switch-visibility% (beep))))))


(defun inner.p (ovl1 ovl2)
  "Verifies if the first overlay is inner (PRIORITY is smaller) than the second overlay."
  (< (overlay-get ovl1 'priority)
     (overlay-get ovl2 'priority)))


(defun invisible.p (ovl)
  "Verifies if the overlay is invisible."
  (overlay-get ovl 'invisible))


(defun select.inner.overlay (ovls)
  "Selects the most inner overlay."
  (when ovls
    (setq ovls (sort ovls #'inner.p))
    (let ((ovl (pop ovls)))
      (dolist (ovl-aux ovls)
	  (when (invisible.p ovl-aux)
	    (setq ovl ovl-aux)))
      ovl)))



(defun select.by.category (ovls category)
  "Returns the first occurrence of an overlay of category CATEGORY."
  (dolist (ovl ovls)
    (when (eq (overlay-get ovl 'category) category)
      (return ovl))))


(defun delete.overlays (ovls)
  "Deletes the overlays OVLS concerned to doc outline."
  (dolist (ovl ovls)
    (when (member (overlay-get ovl 'category) +categories+)
      (message "Delete")
      (delete-overlay ovl))))


(defun delete.overlay (ovls category)
  "Deletes the overlays OVLS of category CATEGORY."
  (dolist (ovl ovls)
    (when (eq (overlay-get ovl 'category) category)
      (delete-overlay ovl))))


(defun start.of.comment.block ()
  "Moves the point to the beginning of the first line of the
  current comment block. Returns NIL if point is not at a comment block."
  (let ((comment (first (mode-fillers)))
	p)
    (save-excursion
     (beginning-of-line)
     (when (looking-at comment)
       (setq p (point))))
    (when p
      (goto-char p)
      (forward-line -1)
      (while (and (not (bobp))
		  (looking-at comment))
	     (forward-line -1))
      (when (not (bobp)) (forward-line 1))
      (point))))


(defun end.of.comment.block ()
  "Moves the point to the end of the last line of the
  current comment block. Returns NIL if point is not at a comment block."
  (let ((comment (first (mode-fillers)))
	p)
    (save-excursion
     (beginning-of-line)
     (when (looking-at comment)
       (setq p (point))))
    (when p
      (goto-char p)
      (forward-line 1)
      (while (and (not (eobp))
		  (looking-at comment))
	     (forward-line 1))
      (when (not (eobp)) (forward-line -1))
      (end-of-line)
      (point))))


(defun comment.line.p ()
  "Verifies if the current line is commented."
  (save-excursion
    (beginning-of-line)
    (when (looking-at (first (mode-fillers)))
       t)))


;;;; Definition of OUTLN

(defstruct outln
  header-rexp
  category
  end-body-fn
  outline-fn
  multiple
  leaf-p)


(defun comment.header.rexp ()
  "Defines the regular expression that characterises the start
  of a comment block."
  (let* ((mode-fillers (mode-fillers))
	 (comment (first mode-fillers))
	 (filler (second mode-fillers)))
    (concat filler "\n" comment)))


(defun comment.end.body.fn (end &optional outln)
  "Moves the point to the end of the comment body."
  (declare (ignore outln))
  (end.of.comment.block))


(defun comment.outline.fn (start end &optional outln)
  (declare (ignore outln))
  (let ((ovlay (make-overlay start end)))
    (overlay-put ovlay 'category 'ovl_comment)
    ))

        
(defun main.header.rexp ()
  "Defines the regular expression that characterises the start
  of the Description block."
  "Description$") 
  

(defun main.end.body.fn (end &optional outln)
  "Moves the point to the end of the Description body."
  (declare (ignore outln))
  (and end
       (re-search-forward (history.header.rexp) end t)
       (or (forward-line -1) t)
       (or (end-of-line) t)
       (point)))


(defun main.outline.fn (start end &optional outln)
  (declare (ignore outln))
  (let ((ovlay (make-overlay start end)))
    (overlay-put ovlay 'category 'ovl_description)
    (overlay-put ovlay 'invisible +default-visibility-state+)
    ))


(defun sub.header.rexp ()
  "Defines the regular expression that characterises the start
  of the attribute blocks."
  (when *all.attrs.keywords*
    (let ((rexp (first *all.attrs.keywords*)))
      (dolist (keyword (rest *all.attrs.keywords*))
	(setq rexp (concat rexp +alternative-rexp+ keyword)))
      rexp)))


(defun sub.end.body.fn (end &optional outln)
  "Moves the point to the end of the sub-description body."
  (and end
       (or (re-search-forward (funcall (outln-header-rexp outln)) end t)
	   (re-search-forward (history.header.rexp) end t))
       (or (forward-line -1) t)
       (or (end-of-line) t)
       (point)))


(defun sub.outline.fn (start end &optional outln)
  (declare (ignore outln))
  (let ((ovlay (make-overlay start end)))
    (overlay-put ovlay 'category 'ovl_sub_description)
    (overlay-put ovlay 'invisible +default-visibility-state+)
    ))


(defun history.header.rexp ()
  "Defines the regular expression that characterises the start
  of the History block."
  "History$")


(defun history.end.body.fn (end &optional outln)
  "Moves the point to the end of the History body."
  (declare (ignore outln))
  (let* ((mode-fillers (mode-fillers))
	 (filler (second mode-fillers)))
    (and end
	 (re-search-forward filler end t)
	 (or (forward-line -1) t)
	 (or (end-of-line) t)
	 (point))))


(defun history.outline.fn (start end &optional outln)
  (declare (ignore outln))
  (let ((ovlay (make-overlay start end)))
    (overlay-put ovlay 'category 'ovl_history)
    (overlay-put ovlay 'invisible +default-visibility-state+)
    ))



(defun all.outlns ()
  "Builds the list of all outlns. The order influences
  the result."
  (list
;;;   (make-outln :header-rexp #'comment.header.rexp
;;;   	       :end-body-fn #'comment.end.body.fn
;;;	       :outline-fn #'comment.outline.fn
;;;	       :category 'ovl_comment
;;;	       :multiple nil
;;;	       :leaf-p nil)
   (make-outln :header-rexp #'main.header.rexp
	       :end-body-fn #'main.end.body.fn
	       :outline-fn #'main.outline.fn
	       :category 'ovl_description
	       :multiple nil
	       :leaf-p nil)
   (make-outln :header-rexp #'sub.header.rexp
	       :end-body-fn #'sub.end.body.fn
	       :outline-fn #'sub.outline.fn
	       :category 'ovl_sub_description
	       :multiple t
	       :leaf-p t)
   (make-outln :header-rexp #'history.header.rexp
	       :end-body-fn #'history.end.body.fn
	       :outline-fn #'history.outline.fn
	       :category 'ovl_history
               :multiple nil
	       :leaf-p t)
   ))


(defvar *all-outlns* (all.outlns)
  "All defined outline forms.")


(defun get.all.outlns ()
  (or *all-outlns* 
      (setq *all-outlns* (all.outlns))))


(defun init.all.outlns ()
  (setq *all-outlns* (all.outlns)))


(defun outline.comment.block (start end)
  "Outlines a comment block starting at START and ending at END."
  (let ((ignore-inv line-move-ignore-invisible)
	(pos start)
	(outlns (get.all.outlns))
	m-outln
	new-pos)
    (setq line-move-ignore-invisible nil)
    (while (and (not (null outlns))
		(not (>= pos end)))
      (goto-char pos)
      (setq new-pos nil)   
      (setq m-outln (get.matching.outline outlns))
      (when m-outln
	(unless (outln-multiple m-outln)
	  (setq outlns (remove m-outln outlns)))
	(setq new-pos (save-excursion (apply.outline m-outln))))
      (if new-pos 
	(setq pos (1+ new-pos))
	(setq pos (1+ pos))))
    (setq line-move-ignore-invisible ignore-inv)
    end))


(defun get.matching.outline (outlns)
  "Selects an outline item with an header starting at the current point."
  (dolist (item outlns)
    (when (looking-at (funcall (outln-header-rexp item)))
      (return item))))


(defun apply.outline (outln)
  "Applies an outline to the current point."
  (let ((rexp (funcall (outln-header-rexp outln))))
    (when (looking-at rexp)
      (let ((start (re-search-forward rexp)))
	(let ((comment-end (save-excursion (end.of.comment.block))))
	  (let ((end (funcall (outln-end-body-fn outln) comment-end outln)))
	    (when (and start end)
	      (delete.overlay (overlays-at start) (outln-category outln))
	      (funcall (outln-outline-fn outln) start end outln)
	      (if (outln-leaf-p outln)
		  end
		  start))))))))
  

(defun previous.line.matches.p (rexp)
  "Verifies if the beginning of the previous line matches REXP."
  (save-excursion
    (beginning-of-line)
    (unless (bobp)
      (forward-line -1)
      (looking-at rexp))))


(defun next.comment.block ()
  "Moves point to the beginning of the next comment block
(if it exists)."
  (let* ((mode-fillers (mode-fillers))
	 (comment (first mode-fillers))
	 (filler (second mode-fillers))
	 (comment.rexp (concat "^" comment))
	 (filler.rexp (concat "^" filler))
         pos)
    (save-excursion
      (setq pos (re-search-forward filler.rexp nil t))
      (while (and pos
                  (previous.line.matches.p comment.rexp))
	 (setq pos (re-search-forward filler.rexp nil t)))
      (when pos 
         (beginning-of-line)
         (setq pos (point))))
    (when pos (goto-char pos))))


(defun doc.outline.buffer ()
  "Outlines the internal documentation of the current buffer."
  (interactive)
  (setq line-move-ignore-invisible t)
  (save-excursion
    (or (outlined.p) (turn.on.outline))
    (message "Outlining buffer...")
    (goto-char (point-min))
    (while (next.comment.block)
      (let ((start (point))
	    (end (end.of.comment.block)))
	(outline.comment.block start end)
	(goto-char end)))
    (message "Buffer outlined")))


(defun doc.outline.block ()
  "Outlines the current comment block."
  (interactive)
  (save-excursion
    (setq line-move-ignore-invisible t)
    (let ((start (start.of.comment.block)))
      (when start
	(or (outlined.p) (turn.on.outline))
	(message "Outlining block...")
	(outline.comment.block start (end.of.comment.block))
	(message "Block outlined")))))



(defun turn.on.outline ()
  "Turns on the outline mode."
  (interactive)
  (setf buffer-invisibility-spec '((t . t)))  ;; Gera lixo.
  (redraw-frame (selected-frame))
  (message "Outline On"))


(defun turn.off.outline ()
  "Turns off the outline mode."
  (interactive)
  (setf buffer-invisibility-spec '(INACTIVE))
  (redraw-frame (selected-frame))
  (message "Outline Off"))


(defun outlined.p ()
  "Verifies if buffer has been outlined."
  (consp buffer-invisibility-spec))


(defun outline.active.p ()
  "Verifies if outline is on."
  (and (consp buffer-invisibility-spec)
       (consp (first buffer-invisibility-spec))))


(defun overlays.in.line.p ()
  "Verifies if exists overlays at the current -line-."
  (save-excursion
    (when (or (overlays-at (point))
	      (or (end-of-line) nil)
	      (overlays-at (point))
	      (overlays-at (1- (point))))
      t)))


;;;; Definition of menu Doc


;;; Definition of Outline Map
(defvar outline-map)

(setq outline-map
  (make-sparse-keymap "Outline tool"))

(fset 'outline outline-map)

(define-key outline-map [Outline]
  '("Outline buffer" . doc.outline.buffer))

(define-key outline-map [separator1]
  '("--" . nil))

(define-key outline-map [switch.visibility]
  '("Switch visibility" . switch.visibility))

(define-key global-map %switch-visibility-key%
   'switch.visibility)

(put 'switch.visibility 'menu-enable '(and (outline.active.p) (overlays.in.line.p)))

(define-key outline-map [separator2]
  '("--" . nil))

(define-key outline-map [turn.off.outline]
  '("Turn off outline" . turn.off.outline))

(put 'turn.off.outline 'menu-enable '(and (outlined.p) (outline.active.p)))

(define-key outline-map [turn.on.outline]
  '("Turn on outline" . turn.on.outline))

(put 'turn.on.outline 'menu-enable '(and (outlined.p) (not (outline.active.p))))

(define-key outline-map [separator3]
  '("--" . nil))

(define-key outline-map [Outline.block]
  '("Outline header" . doc.outline.block))

(define-key global-map %outline-block-key%
   'doc.outline.block)

(put 'doc.outline.block 'menu-enable '(comment.line.p))



;;; Definition of Doc Map


(defvar doc-map)

(setq doc-map
  (make-sparse-keymap "Documentation tool"))

(fset 'doc doc-map)

(define-key doc-map [Outline]
  '("Outline..." . outline))

(define-key doc-map [separator1]
  '("--" . nil))


;;; Addition of Menu Doc to global-map.

(define-key-after (lookup-key global-map [menu-bar])
    [doc] '("Doc" . doc) 'edit)
















