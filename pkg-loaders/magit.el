;;; ---------------------------------------------------------------------
;;; Magit
;;;
(if (equal system-type 'windows-nt)
    (add-to-list 'load-path (bj:expand-package "magit-1.4.2"))
    (bj:ensure-package 'magit))

(when (equal system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/git/bin/"))


(add-hook 'dired-load-hook
          (lambda ()
	    (load "dired-x")))
(add-hook 'dired-mode-hook
          (lambda ()))

(autoload 'magit-status "magit" "Loads magit-mode" t)

(setq magit-last-seen-setup-instructions "1.4.0")
