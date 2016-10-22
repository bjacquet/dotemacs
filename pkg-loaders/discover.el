;;; ---------------------------------------------------------------------
;;; Discover.el
;;;
(add-to-list 'load-path (bj:expand-package "discover.el"))
(add-to-list 'load-path (bj:expand-package "makey"))
(require 'discover)
(global-discover-mode 1)
