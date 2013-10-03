;;; ---------------------------------------------------------------------
;;; Flyspell
;;;
(setq flyspell-mode-line-string nil)
(lexical-let ((idx 0)
	      (options (list 'flyspell-mode
			     'flyspell-prog-mode
			     'flyspell-mode-off)))
  (setf (cdr (last options)) options)
  (defun flyspell-mode-cycle ()
    (interactive)
    (let ((option (elt options idx)))
      (setf idx (1+ idx))
      (message "%s" option)
      (funcall option))))

(global-set-key [f7]   'flyspell-mode-cycle)
(global-set-key [S-f7] 'flyspell-buffer)
