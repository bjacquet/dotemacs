;;; ---------------------------------------------------------------------
;;; JavaScript
;;;

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (setq-default js2-basic-offset 2))


(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))
