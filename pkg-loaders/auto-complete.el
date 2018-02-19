;;; ---------------------------------------------------------------------
;;; Auto Complete
;;;
(use-package auto-complete
  :ensure t
  ;; :bind (("\t"   . ac-complete)
  ;;        ("\r"   . nil)
  ;;        ("\C-n" . ac-next)
  ;;        ("\C-p" . ac-previous))
  :init
  (setq ac-auto-start 3)
  (setq ac-dwim t)
  (global-auto-complete-mode t))
