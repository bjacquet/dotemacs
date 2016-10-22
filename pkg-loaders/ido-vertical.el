;;; ---------------------------------------------------------------------
;;; IDO Vertical
;;;
(bj:ensure-package 'ido-vertical-mode)
(require 'ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
