;; -*- coding: utf-8; -*-

;;; ---------------------------------------------------------------------
;;; package --- Bruno Jacquet Emacs config
;;;

(package-initialize)

(defun load-config()
  "Load the actual configuration in literate 'org-mode' elisp."
  (interactive)
  (org-babel-load-file "~/.emacs.d/future-configuration.org"))

(load-config)


(load-file (expand-file-name "~/.emacs.d/dotemacs/rtrv.el"))

;;; init.el ends here
