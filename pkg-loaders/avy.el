;;; ---------------------------------------------------------------------
;;; Avy
;;;
(ensure-package 'avy)
(require 'avy)
(define-key global-map (kbd "C-S-j") 'avy-goto-word-or-subword-1)
(setq avy-case-fold-search nil)
