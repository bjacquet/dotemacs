;;; ---------------------------------------------------------------------
;;; Paredit
;;;
(ensure-package 'paredit)
(autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code."
    t)

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(eval-after-load 'paredit
    '(progn
       (define-key paredit-mode-map (kbd "ESC M-A-C-s-)")
         'paredit-dwim)))
