;;; ---------------------------------------------------------------------
;;; Flyspell
;;;
(setq flyspell-mode-line-string nil)
(lexical-let ((idx 0)
	      (options (list 'flyspell-mode ; turns on
			     'flyspell-prog-mode
			     'flyspell-mode ; turns off
			     )))
  (setf (cdr (last options)) options)
  (defun flyspell-mode-cycle ()
    (interactive)
    (let ((option (elt options idx)))
      (setf idx (1+ idx))
      (message "%s" option)
      (funcall option))))
