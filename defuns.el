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
              (format-time-string "%d/%m/%Y")
              (format-time-string "%B %-d, %Y"))))


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


(defun bj:split-window-vertically ()
  "Split window and move cursor."
  (interactive)
  (split-window-vertically)
  (recenter)
  (other-window 1)
  (recenter))


(defun bj:split-window-horizontally ()
  "Split window and move cursor."
  (interactive)
  (split-window-horizontally)
  (other-window 1))


(defun bj:toggle-window-split ()
  "From https://www.emacswiki.org/emacs/ToggleWindowSplit
Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))
    (unless done
      (message "bj:toggle-window-split (part II)")
      (setq done nil)
      (dolist (dirs '((left . up) (up . left)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'left)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf)
                (other-window 1)))))))))


(defun bj:save-rot13 (arg)
  "Super-duper cryptic save."
  (interactive "P")
  (rot13-region (point-min) (point-max))
  (save-buffer)
  (if arg
      (kill-buffer)
    (rot13-region (point-min) (point-max))))


;;; defuns.el ends here
