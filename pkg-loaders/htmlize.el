;;; ---------------------------------------------------------------------
;;; Htmlize
;;;
(ensure-package 'htmlize)

(defun bj:export-buffer-to-html ()
  "Provided by LBO. Needs tweaking! :-)"
  (interactive)
  (let ((themes custom-enabled-themes))
    (mapc #'disable-theme themes)
    (unwind-protect
	(with-current-buffer (htmlize-buffer)
	  (let ((file (make-temp-file "htmlized-buffer-" nil ".html")))
	    (write-file file)
	    (browse-url file))
	  (kill-buffer))
      (mapc #'enable-theme themes))))
