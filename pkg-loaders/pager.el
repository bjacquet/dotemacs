;;; ---------------------------------------------------------------------
;;; Pager
;;;
(bj/ensure-package 'pager)
(require 'pager)
(global-set-key "\C-v" 	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\M-v"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)
