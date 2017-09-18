;;; ---------------------------------------------------------------------
;;; Magit
;;;
(bj:ensure-package 'magit)

(when (equal system-type 'windows-nt)
  (add-to-list 'exec-path "d:/tmp/Git/bin/")
  (setenv "SSH_ASKPASS" "git-gui--askpass"))

(when (equal system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/git/bin/"))

(add-hook 'dired-load-hook
          (lambda ()
	    (load "dired-x")))
(add-hook 'dired-mode-hook
          (lambda ()))

(autoload 'magit-status "magit" "Loads magit-mode" t)
