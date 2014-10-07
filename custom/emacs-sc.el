;; -*- coding: utf-8; -*-
;;; .emacs - SISCOG specific
;;;


(load-file (expand-file-name (concat emacs-dir "pkg-config.el")))
(load-pkg-loader "sc-emacs.el")
(load-pkg-loader "sc-slime.el")
(load-pkg-loader "sc-sly.el")


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


(switch-to-buffer "*scratch*")
(goto-char (point-min))
(fancy-splash-head)
(goto-char (point-max))
