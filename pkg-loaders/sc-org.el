;;; ---------------------------------------------------------------------
;;; SISCOG Org
;;;
(load-file (bj:expand-package "/sc-org/sc-org.el"))
(bj:ensure-package 'org-trello)
(require 'org-trello)

;; org-trello major mode for all .trello files
(add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
(add-hook 'org-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name (current-buffer))))
              (when (and filename (string= "trello" (file-name-extension filename)))
		(org-trello-mode)))))

;; Overrides saving to use my super-duper cryptic save.
(add-hook 'org-mode-hook
	  '(lambda () (local-set-key (kbd "C-x C-s") 'bj:save-rot13)))

(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
