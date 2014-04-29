;;; ---------------------------------------------------------------------
;;; Magit
;;;
(when (equal system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/git/bin/"))


(add-hook 'dired-load-hook
          (lambda ()
	    (load "dired-x")))
(add-hook 'dired-mode-hook
          (lambda ()))

(add-to-list 'load-path (expand-package "git-modes"))
(add-to-list 'load-path (expand-package "magit"))
(autoload 'magit-status "magit" "Loads magit-mode" t)
