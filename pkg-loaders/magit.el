;;; ---------------------------------------------------------------------
;;; Magit
;;;
(use-package magit
  :ensure t
  :hook ((dired-load-hook . (lambda () (load "dired-x")))
         (dired-mode-hook . (lambda ())))
  :config
  (when mac-p
    (add-to-list 'exec-path "/usr/local/git/bin/"))
  (autoload 'magit-status "magit" "Loads magit-mode" t))

(use-package forge
  :ensure t
  :after magit)
