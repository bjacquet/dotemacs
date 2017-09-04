;;; ---------------------------------------------------------------------
;;; Color Theme & Color Theme Random
;;;
(bj:ensure-package 'color-theme-modern)
(bj:ensure-package 'tangotango-theme)
(bj:ensure-package 'darktooth-theme)
(bj:ensure-package 'color-theme-solarized)
(bj:ensure-package 'naquadah-theme)
(bj:ensure-package 'birds-of-paradise-plus-theme)
(bj:ensure-package 'tango-2-theme)
(load-file (bj:expand-package "color-theme-random.el"))
(autoload 'color-theme-random (bj:expand-package "color-theme-random.el"))
(bj:color-theme-random)
