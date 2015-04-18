;;; ---------------------------------------------------------------------
;;; Expand Region
;;;
(add-to-list 'load-path (expand-package "expand-region"))
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
