;;; ---------------------------------------------------------------------
;;; Visible Bookmarks
;;;
(use-package bm
  :ensure t
  :init
  (setq bm-cycle-all-buffers t)
  (global-set-key (kbd "<left-fringe> <wheel-up>")   'bm-next-mouse)
  (global-set-key (kbd "<left-fringe> <wheel-down>") 'bm-previous-mouse)
  (global-set-key (kbd "<left-fringe> <mouse-1>")    'bm-toggle-mouse)
  (global-set-key (kbd "<left-fringe> <mouse-3>")    'bm-show-all)

  :bind (("C-c m" . bm-toggle)
         ("C-c j" . bm-previous)))
