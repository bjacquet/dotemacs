;;; ---------------------------------------------------------------------
;;; Shrink Whitespace
;;;
(bj/ensure-package 'shrink-whitespace)
(require 'shrink-whitespace)
(define-key global-map (kbd "C-x C-o") 'shrink-whitespace)
