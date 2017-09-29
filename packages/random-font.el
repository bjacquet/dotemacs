;;; ---------------------------------------------------------------------
;;; Random fonts
;;;

(let ((fonts (list "Lucida Console-13"))
      font)
  (cond
   ((equal system-type 'windows-nt)
    (setq fonts (append fonts (list "Consolas-14" "Consolas-12"))))
   ((equal system-type 'darwin)
    (setq fonts (list "DejaVu Sans Mono-14"
		      "NovaMono-14"
		      "Monaco-14"
		      "Share Tech Mono-15"
		      "Ubuntu Mono-16"))))
  (setq font (nth (random (length fonts)) fonts))
  (set-frame-font font)
  (message (format "Random font: %s" font)))
