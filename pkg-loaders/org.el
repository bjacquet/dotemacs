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
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook (lambda () (text-scale-increase 2))))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⚠ " "⬇")))


(use-package magit-org-todos
  :ensure t
  :config
  (magit-org-todos-autoinsert))

(use-package org-pomodoro
  :ensure t)

(bj:load-pkg-loader "./org-agenda-files.el")
