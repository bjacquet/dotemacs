;;; ---------------------------------------------------------------------
;;; Ace Jump
;;;
(ensure-package 'ace-jump-mode)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c a") 'ace-jump-mode)
