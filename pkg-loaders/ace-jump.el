;;; ---------------------------------------------------------------------
;;; Ace Jump
;;;
(add-to-list 'load-path (expand-package "ace-jump"))
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c a") 'ace-jump-mode)
