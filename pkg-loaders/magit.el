;;; ---------------------------------------------------------------------
;;; Magit
;;;
(ensure-package 'magit)
(when (equal system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/git/bin/"))


(add-hook 'dired-load-hook
          (lambda ()
	    (load "dired-x")))
(add-hook 'dired-mode-hook
          (lambda ()))

(autoload 'magit-status "magit" "Loads magit-mode" t)

(setq magit-last-seen-setup-instructions "1.4.0")
