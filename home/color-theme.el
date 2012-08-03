;;; ---------------------------------------------------------------------
;;; Color Theme & Color Theme Random
;;;
(add-to-list 'load-path (concat-package-dir "color-theme"))
(autoload 'color-theme (expand-package "color-theme"))
(eval-after-load "color-theme"
  '(color-theme-initialize))
(load-file (expand-package "color-theme-random.el"))
(autoload 'color-theme-random (expand-package "color-theme-random.el"))
(color-theme-random)
