;;; ---------------------------------------------------------------------
;;; Color Theme & Color Theme Random
;;;
(bj/ensure-package 'color-theme-modern)
(bj/ensure-package 'color-theme-solarized)
(bj/ensure-package 'tangotango-theme)
(load-file (bj/expand-package "color-theme-random.el"))
(autoload 'color-theme-random (bj/expand-package "color-theme-random.el"))
(color-theme-random)
