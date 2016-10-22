;;; ---------------------------------------------------------------------
;;; SC-Slime - Slime for use at SISCOG
;;;
(bj:ensure-package 'slime)
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-indentation))
(setq lisp-indent-function 'common-lisp-indent-function)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
