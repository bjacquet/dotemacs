;;; ---------------------------------------------------------------------
;;; Dashboard
;;;
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (add-to-list 'dashboard-items '(agenda) t)
  (setq show-week-agenda-p t)
  (setq dashboard-items '((agenda . 5)
                          (projects . 15)
                          (recents  . 5)
                          (bookmarks . 5)
                          (registers . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))
