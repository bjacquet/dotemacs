;;; ---------------------------------------------------------------------
;;; Color Theme & Color Theme Random
;;;
(ensure-package 'color-theme-modern)
(ensure-package 'color-theme-solarized)
(ensure-package 'tangotango-theme)
(load-file (expand-package "color-theme-random.el"))
(autoload 'color-theme-random (expand-package "color-theme-random.el"))
(color-theme-random)
