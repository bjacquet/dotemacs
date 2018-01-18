;;; ---------------------------------------------------------------------
;;; Magit
;;;
(bj:ensure-package 'magit)

(when mac-p
  (add-to-list 'exec-path "/usr/local/git/bin/"))

(add-hook 'dired-load-hook
          (lambda ()
	    (load "dired-x")))
(add-hook 'dired-mode-hook
          (lambda ()))

(autoload 'magit-status "magit" "Loads magit-mode" t)
