(set-default-font "DejaVu Sans Mono-10")
(defvar *use-slime* t)


(defun kill-most-buffers (&optional keep-list)
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
	       (if (yes-or-no-p
		    (format "Buffer %s has been edited. Kill? " name))
		   (kill-buffer buffer))
	     (kill-buffer buffer))))
    (setq list (cdr list))))


(defun buffer-and-desktop ()
  (interactive)
  (setq buffer (current-buffer))
  (kill-most-buffers (list buffer))
  (delete-other-windows)
  (switch-to-buffer buffer)
  (desktop-menu))


(defun scratch-and-desktop ()
  (interactive)
  (kill-most-buffers)
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (desktop-menu))

(global-set-key [f6]   'buffer-and-desktop)
(global-set-key [S-f6] 'scratch-and-desktop)


;;; ---------------------------------------------------------------------
;;; SC-Emacs
;;;
(load (format "%s/custom/sc-before.el"
	      (getenv "SISCOG_EMACS_DIR_LOCAL")))

;; Tells SC-EMACS to use new SISCOG's ODBC names
(defvar *new-odbc-names* t)

;; Load SC-EMACS
(load (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))

;; Customise SC-EMACS
(load (format "%s/custom/sc-user-param.el"
	      (getenv "SISCOG_EMACS_DIR_LOCAL")))

;; Load other user specific customization.
(load (format "%s/custom/sc-after.el" (getenv "SISCOG_EMACS_DIR_LOCAL")))

;; (defvar *linked-to-server-dir*
;;   (if (file-exists-p "x:/siscog/sc-emacs")
;;       "x:/siscog/sc-emacs"))

(setf vc-handled-backends nil)


