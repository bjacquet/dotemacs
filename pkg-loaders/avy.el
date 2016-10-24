;;; ---------------------------------------------------------------------
;;; Avy
;;;
(bj:ensure-package 'avy)
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-word-or-subword-1)
(setq avy-case-fold-search nil)
