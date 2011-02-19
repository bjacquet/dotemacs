;; -*- coding: utf-8; -*-
;;; .emacs - SISCOG specific to code
;;;
(transient-mark-mode t)
(set-default-font "DejaVu Sans Mono-11")
(defvar *use-slime* t)


(defun fancy-splash-head ()
 "Insert the head part of the splash screen into the current buffer."
 (let* ((image-file (format "%s/custom/siscog-symbol.xpm" (getenv "SISCOG_EMACS_DIR_LOCAL")))
        (img (create-image image-file))
        (image-width (and img (car (image-size img))))
        (window-width (window-width (selected-window))))
   (when img
     (when (> window-width image-width)
       (insert "\n\n")
       ;; Center the image in the window.
       (insert (propertize " " 'display
                           `(space :align-to (+ center (-0.5 . ,img)))))

       ;; Change the color of the XPM version of the splash image
       ;; so that it is visible with a dark frame background.
       (when (and (memq 'xpm img)
                  (eq (frame-parameter nil 'background-mode) 'dark))
         (setq img (append img '(:color-symbols (("#000000" . "gray30"))))))
       (insert-image img)
       (insert "\n\n")))))


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


(switch-to-buffer "*scratch*")
(goto-char (point-min))
(fancy-splash-head)
