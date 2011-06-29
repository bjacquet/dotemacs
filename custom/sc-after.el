;; -*- coding: utf-8; -*-
;;; .emacs - SISCOG specific to organize
;;;


;;; ---------------------------------------------------------------------
;;; SISCOG Stuff
;;;

(sc-set-acl-version :v8-1 t)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq ps-landscape-mode t)
(setq ps-number-of-columns 2)
(global-unset-key [f11])
(define-key global-map [M-left]  'backward-sexp)
(define-key global-map [M-right] 'forward-sexp)


;;; ---------------------------------------------------------------------
;;; Personal Stuff
;;;

