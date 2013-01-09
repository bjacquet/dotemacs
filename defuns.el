;; -*- coding: utf-8; -*-
;;; defuns.el - Utilities

(setq debug-on-error t)


(defun title-set-title ()
  "Set title to current`s buffer \[buffer-file-name] name
or to \[buffer-name if it has no file"
  (let ((name (format "%s"
                      (cond
                       ((string-match
                         (getenv "HOME")
                         (or (buffer-file-name (current-buffer))
                             (buffer-name)))
                        (concat "~"
                                (substring
                                 (buffer-file-name
                                  (current-buffer))
                                 (match-end 0))))
                       (t (or
                           (buffer-file-name (current-buffer))
                           (buffer-name)))))))
    (modify-frame-parameters (selected-frame)
                             (list (cons `name name)))))
(add-hook `post-command-hook
	  (lambda ()
	    (title-set-title)))


(defun trim-str (str)
  "Trims leading and tailing whitespace from `str'."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))


(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))


(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))


(defun espacos () (interactive)
  "Eliminate whitespace at enfs of all lines in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t][ \t]*$" nil t)
      (delete-region (match-beginning 0) (point)))))

;;; defuns.el ends here
