;;; ---------------------------------------------------------------------
;;; Avy
;;;
(use-package avy
  :ensure t
  :init (setq avy-case-fold-search nil)
  :bind (("C-:" . avy-goto-word-or-subword-1)))
