;;; ---------------------------------------------------------------------
;;; Org-mode
;;;


(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
