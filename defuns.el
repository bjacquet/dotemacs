;; -*- coding: utf-8; -*-
;;; defuns.el - Utilities

(setq debug-on-error t)


(defvar packages-directory "~/dotemacs/packages/"
  "The packages directory.")


(defvar configuration-directory "~/dotemacs/home/"
  "The directory of this configuration.")


(defmacro concat-package-dir (package-name)
  "(concat packages-directory package-name)"
  (list 'concat packages-directory package-name))


(defmacro expand-package (package-name)
  "(expand-file-name (concat-package-dir package-name))"
  (list 'expand-file-name (list 'concat-package-dir package-name)))


(defmacro load-configuration (config-file-name)
  `(load-file (expand-file-name (concat configuration-directory ,config-file-name))))


(add-to-list 'load-path packages-directory)


(defun title-set-title ()
  "Set title to current`s buffer \[buffer-file-name] name
or to \[buffer-name if it has no file"
  (let ((name (format "%s"
                      (cond
                       ((string-match
                         (getenv "HOME")
                         (or (buffer-file-name (current-buffer))
                             (buffer-name)))
                        (concat "~"
                                (substring
                                 (buffer-file-name
                                  (current-buffer))
                                 (match-end 0))))
                       (t (or
                           (buffer-file-name (current-buffer))
                           (buffer-name)))))))
    (modify-frame-parameters (selected-frame)
                             (list (cons `name name)))))
(add-hook `post-command-hook
	  (lambda ()
	    (title-set-title)))

;;; defuns.el ends here
