;;; ---------------------------------------------------------------------
;;; Deft
;;;
(use-package deft
  :ensure t
  :commands (deft)
  :config
  (setq deft-directory "~/Documents/Notes"
        deft-extensions '("md" "org")
        deft-use-filename-as-title t
        deft-current-sort-method 'title))
