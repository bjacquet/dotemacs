;;; ---------------------------------------------------------------------
;;; Deft
;;;
(use-package deft
  :bind ("<f6>" . deft)
  :commands (deft)
  :config (setq deft-directory "~/Documents/Diary"
                deft-extensions '("md" "org")
                deft-use-filename-as-title t
                deft-current-sort-method 'title))
