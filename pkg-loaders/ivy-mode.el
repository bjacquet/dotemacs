;;; ---------------------------------------------------------------------
;;; Auto Complete
;;;
(bj:ensure-package 'ivy)
(ivy-mode 1)
(setq ivy-count-format      "(%d/%d) "
      ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
