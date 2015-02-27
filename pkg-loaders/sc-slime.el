;;; ---------------------------------------------------------------------
;;; SC-Slime - Slime for use at SISCOG
;;;

(add-to-list 'load-path (expand-package "slime"))
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-indentation))
