;; -*- coding: utf-8; -*-
;;; .emacs - SISCOG specific
;;;


;;; ---------------------------------------------------------------------
;;; SC-Emacs
;;;
(defvar *use-slime* t)

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


(transient-mark-mode t)
(set-default-font "DejaVu Sans Mono-11")


;;; ---------------------------------------------------------------------
;;; Personal Stuff
;;;
(defun random-elem (list)
  (nth (random (length list)) list))

(defun fancy-splash-head ()
 "Insert the head part of the splash screen into the current buffer."
 (let* ((logo (random-elem (list "lisplogo-alien.xpm"
				 "siscog-symbol.xpm"
				 "lisplogo-flag.xpm"
				 "glider.xpm")))
	(image-file (format "%s/custom/%s"
			    (getenv "SISCOG_EMACS_DIR_LOCAL")
			    logo))
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


(global-set-key [f8]   'find-file-at-point)
(global-set-key [f9]   'last-closed-files)


(switch-to-buffer "*scratch*")
(goto-char (point-min))
(fancy-splash-head)
(goto-char (point-max))
