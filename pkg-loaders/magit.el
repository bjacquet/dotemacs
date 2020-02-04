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

(use-package magit-todos
  :ensure t
  :after magit
  :init
  (setq magit-todos-exclude-globs '("vendor/*"))
  :config
  (add-hook 'magit-status-mode-hook 'magit-todos-mode))
