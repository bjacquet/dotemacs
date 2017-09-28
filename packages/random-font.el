;;; ---------------------------------------------------------------------
;;; Random fonts
;;;

(let ((fonts (list "DejaVu Sans Mono-14"))
      font)
  (cond
   (mac-p
    (setq fonts (append fonts (list "NovaMono-14"
                                    "Share Tech Mono-15"
                                    "Ubuntu Mono-16"
                                    "Monaco-14"))))
   ((eq window-system 'w2)
    (setq fonts (append fonts (list "Consolas-14")))))
  (setq font (nth (random (length fonts)) fonts))
  (set-frame-font font)
  (message (format "Random font: %s" font)))
