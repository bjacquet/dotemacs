;; -*- coding: utf-8; -*-

;;; ---------------------------------------------------------------------
;;; package --- Bruno Jacquet Emacs config
;;;

(defun load-config()
  "Load the actual configuration in literate 'org-mode' elisp."
  (interactive)
  (org-babel-load-file "~/.config/dotemacs/future-configuration.org"))

(load-config)

;;; load-config.el ends here
