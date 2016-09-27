;;; ---------------------------------------------------------------------
;;; Htmlize
;;;
(bj/ensure-package 'htmlize)

(defun bj/export-buffer-to-html ()
  "Provided by LBO."
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

(defun bj/export-region-to-html ()
  "Provided by LBO."
  (interactive)
  (let ((themes custom-enabled-themes)
        (transient-mark-mode-enabled transient-mark-mode))
    (mapc #'disable-theme themes)
    (transient-mark-mode -1)
    (redisplay)
    (unwind-protect
         (with-current-buffer (htmlize-region (region-beginning) (region-end))
           (let ((file (make-temp-file "htmlized-region-" nil ".html")))
             (write-file file)
             (browse-url file))
           (kill-buffer))
      (transient-mark-mode (if transient-mark-mode-enabled 1 -1))
      (mapc #'enable-theme themes))))
