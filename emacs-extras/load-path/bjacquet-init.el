;; -*- coding: utf-8; -*-
;;;; bjacquet-init.el - Extra Utilities


;;; ---------------------------------------------------------------------
;;; Modes loaded on request
;;;
(add-to-list 'load-path (concat emacs-extras-d "/load-path"))


;;; ---------------------------------------------------------------------
;;; Emacs Utilities
;;;
(unless (featurep 'dotemacs)
  (load-library "dotemacs.el"))


;;; ---------------------------------------------------------------------
;;; Org
;;;
(when at-siscog-p
  (load-file (expand-file-name (concat emacs-extras-d "/sc-org/sc-org.el"))))


;;; ---------------------------------------------------------------------
;;; YaSnippet
;;;
(unless at-siscog-p
  (add-to-list 'load-path (concat emacs-extras-d "/yasnippet-0.6.1c"))
  (eval-after-load 'yasnippet
    ;; Initialize Yasnippet
    ;; Don't map TAB to yasnippet
    ;; In fact, set it to something we'll never use because
    ;; we'll only ever trigger it indirectly.
    '(progn 
       (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
       (yas/initialize)
       (yas/load-directory (concat emacs-extras-d "/yasnippet-0.6.1c/snippets")))))


(provide 'bjacquet-init)

;; bjacquet-init.el EOF
