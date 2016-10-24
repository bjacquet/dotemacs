;;; ---------------------------------------------------------------------
;;; Expand Region
;;;
(bj:ensure-package 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
