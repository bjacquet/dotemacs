;;; ---------------------------------------------------------------------
;;; Auto Complete
;;;
(eval-after-load 'auto-complete
  '(progn
     (global-auto-complete-mode t)
     (setq ac-auto-start 3)
     (setq ac-dwim t)
     (define-key ac-complete-mode-map "\t" 'ac-complete)
     (define-key ac-complete-mode-map "\r" nil)
     (define-key ac-complete-mode-map "\C-n" 'ac-next)
     (define-key ac-complete-mode-map "\C-p" 'ac-previous)))

