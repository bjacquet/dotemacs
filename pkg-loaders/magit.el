;;; ---------------------------------------------------------------------
;;; Magit
;;;
(when (equal system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/git/bin/"))

(add-to-list 'load-path (expand-package "git-modes"))
(add-to-list 'load-path (expand-package "magit"))
(autoload 'magit-status "magit" "Loads magit-mode" t)
