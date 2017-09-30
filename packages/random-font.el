;;; ---------------------------------------------------------------------
;;; Random fonts
;;;

(let ((fonts (list "Lucida Console-13"))
      font)
  (cond
   (sc-org-p
    (setq fonts (append fonts (list "Consolas-14"))))
   (sc-dev-p
    (setq fonts (append fonts (list "Consolas-12"))))
   (mac-p
    (setq fonts (list "DejaVu Sans Mono-14"
                      "Melno Regular-14"
		      "Monaco-14"
		      "NovaMono-14"
		      "Share Tech Mono-15"
		      "Ubuntu Mono-16")))
   (t
    (setq fonts (list "NovaMono-12"
                      "Share Tech Mono-12"
		      "Ubuntu Mono-12"))))
  (setq font (nth (random (length fonts)) fonts))
  (set-frame-font font)
  (message (format "Random font: %s" font)))
