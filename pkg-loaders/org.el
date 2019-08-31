;;; ---------------------------------------------------------------------
;;; Org-mode
;;;
(use-package org-bullets
  :ensure t
  :config
  (setq org-clock-into-drawer t)
  (setq org-priority-faces '())
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook 'visual-line-mode))


(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("❢" "⬆" "⬇")))
