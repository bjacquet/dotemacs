;;; ---------------------------------------------------------------------
;;; Magit
;;;
(use-package magit
  :ensure t
  :hook ((dired-load-hook . (lambda () (load "dired-x")))
         (dired-mode-hook . (lambda ())))
  :config
  (autoload 'magit-status "magit" "Loads magit-mode" t))

(use-package forge
  :ensure t
  :after magit)
