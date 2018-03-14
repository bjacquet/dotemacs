;;; ---------------------------------------------------------------------
;;; Org-mode
;;;


(use-package org-bullets
  :ensure t
  :config
  (setq org-clock-into-drawer t))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'visual-line-mode)
