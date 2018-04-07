;;; ---------------------------------------------------------------------
;;; Color Theme & Color Theme Random
;;;
(use-package birds-of-paradise-plus-theme :ensure t)
(use-package color-theme-modern           :ensure t)
(use-package color-theme-solarized        :ensure t)
(use-package darktooth-theme              :ensure t)
(use-package naquadah-theme               :ensure t)
(use-package nord-theme                   :ensure t)
(use-package tango-2-theme                :ensure t)
(use-package tangotango-theme             :ensure t)
(load-file (bj:expand-package "color-theme-random.el"))
(autoload 'color-theme-random (bj:expand-package "color-theme-random.el"))
(bj:color-theme-random)
