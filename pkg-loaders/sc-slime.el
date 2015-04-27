;;; ---------------------------------------------------------------------
;;; SC-Slime - Slime for use at SISCOG
;;;
(ensure-package 'slime)
(require 'slime-autoloads)
(setq lisp-indent-function 'common-lisp-indent-function)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
