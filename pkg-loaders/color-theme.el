;;; ---------------------------------------------------------------------
;;; Color Theme & Color Theme Random
;;;
(bj:ensure-package 'color-theme-modern)
(bj:ensure-package 'tangotango-theme)
(bj:ensure-package 'darktooth-theme)
(bj:ensure-package 'color-theme-solarized)
(load-file (bj:expand-package "color-theme-random.el"))
(autoload 'color-theme-random (bj:expand-package "color-theme-random.el"))
(bj:color-theme-random)
