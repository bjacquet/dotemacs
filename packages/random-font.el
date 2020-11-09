;;; ---------------------------------------------------------------------
;;; Random fonts
;;;

(defun bj:font-random ()
  (interactive)

  (let ((fonts (list "Lucida Console-13"))
        font)
    (if mac-p
        (setq fonts (list "iA Writer Mono S-15"
                          "Menlo-14"
                          "Monaco-14"
                          "NovaMono-15"
                          "Anonymous Pro-16"
                          "CozetteVector-17"
                          "Victor Mono-15"
                          ))
      (setq fonts (list "NovaMono-12"
                        "Share Tech Mono-12"
                        "Ubuntu Mono-12")))
    (setq font (nth (random (length fonts)) fonts))
    (set-frame-font font)
    (message (format "Random font: %s" font))))

(bj:font-random)
