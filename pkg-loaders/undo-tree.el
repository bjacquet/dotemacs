;;; ---------------------------------------------------------------------
;;; Undo Tree
;;;
(bj/ensure-package 'undo-tree)
(load "undo-tree")
(setq undo-tree-mode-lighter nil)
(global-undo-tree-mode)
