;; -*- coding: utf-8; -*-
;;; defuns.el - Utilities

(setq debug-on-error t)


;; (defun title-set-title ()
;;   "Set title to current`s buffer \[buffer-file-name] name
;; or to \[buffer-name if it has no file"
;;   (let ((name (format "%s"
;;                       (cond
;;                        ((string-match
;;                          (getenv "HOME")
;;                          (or (buffer-file-name (current-buffer))
;;                              (buffer-name)))
;;                         (concat "~"
;;                                 (substring
;;                                  (buffer-file-name
;;                                   (current-buffer))
;;                                  (match-end 0))))
;;                        (t (or
;;                            (buffer-file-name (current-buffer))
;;                            (buffer-name)))))))
;;     (modify-frame-parameters (selected-frame)
;;                              (list (cons 'name name)))))
;; (add-hook 'post-command-hook
;; 	  'title-set-title)


(defun bj:trim-str (str)
  "Trims leading and tailing whitespace from `str'."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))


(defun bj:dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))


(defun bj:unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))


(defun bj:espacos () (interactive)
  "Eliminate whitespace at end of all lines in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t][ \t]*$" nil t)
      (delete-region (match-beginning 0) (point)))))


(defun bj:open-user-init-file ()
  (interactive)
  (find-file user-init-file))


(defun bj:kill-most-buffers (&optional keep-list)
  "Kill all buffers silently if unmodified, otherwise ask.
If keep-list has buffers don't kill them."
  (interactive)
  (setq list (buffer-list))
  (dolist (el keep-list)
    (setq list (delq el list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (and (not (string-equal name ""))
	   (not (string-equal name "*Messages*"))
	   (not (string-equal name "*Shell Command Output*"))
	   (not (string-equal name "*scratch*"))
	   (/= (aref name 0) ? )
	   (if (buffer-modified-p buffer)
	       (if (y-or-n-p
		    (format "Buffer %s has been edited. Kill? " name))
		   (kill-buffer buffer))
	     (kill-buffer buffer))))
    (setq list (cdr list))))


(defun bj:insert-todays-date (arg)
  "From http://emacswiki.org/emacs/InsertingTodaysDate"
  (interactive "P")
  (insert (if arg
	      (format-time-string "%B %e, %Y")
	    (format-time-string "%d/%m/%Y"))))


(defun bj:reading-time (arg)
  "Time to read the buffer or region."
  (interactive "P")
  (let* ((words.in.buffer      (if (use-region-p)
                                   (count-words (region-beginning) (region-end))
                                 (count-words (point-min) (point-max))))
         (words.per.minute     270)
         (words.per.second     (/ words.per.minute 60))
         (reading.time.seconds (/ words.in.buffer words.per.second))
         (reading.time.minutes (max (round (/ reading.time.seconds 60)) 1)))
    (if arg
        (insert (format "%d min read" reading.time.minutes))
      (save-excursion
        (message "%d minute%s"
                 reading.time.minutes
                 (if (= reading.time.minutes 1) "" "s"))))))


;;; defuns.el ends here
