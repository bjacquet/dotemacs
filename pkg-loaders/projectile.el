;;; ---------------------------------------------------------------------
;;; Projectile
;;;
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ido))
