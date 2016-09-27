;;; ---------------------------------------------------------------------
;;; Visible Bookmarks
;;;
(bj/ensure-package 'bm)
(global-set-key (kbd "<left-fringe> <wheel-up>")   'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <wheel-down>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>")    'bm-toggle-mouse)
(global-set-key (kbd "<left-fringe> <mouse-3>")    'bm-show-all)
