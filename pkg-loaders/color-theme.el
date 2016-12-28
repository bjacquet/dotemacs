;;; ---------------------------------------------------------------------
;;; Color Theme & Color Theme Random
;;;
(use-package color-theme-modern :ensure t)
(use-package tangotango-theme   :ensure t)
(use-package darktooth-theme    :ensure t)
(bj:ensure-package 'color-theme-solarized)
(load-file (bj:expand-package "color-theme-random.el"))
(autoload 'color-theme-random (bj:expand-package "color-theme-random.el"))
(bj:color-theme-random)
