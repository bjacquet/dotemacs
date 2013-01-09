;; -*- coding: utf-8; -*-
;;; pkg-config.el - Top level definitions.


(defvar packages-directory "~/dotemacs/packages/"
  "The packages directory.")


(defvar configuration-directory "~/dotemacs/pkg-loaders/"
  "The directory of package loading configuration.")


(defmacro concat-package-dir (package-name)
  "(concat packages-directory package-name)"
  (list 'concat packages-directory package-name))


(defmacro expand-package (package-name)
  "(expand-file-name (concat-package-dir package-name))"
  (list 'expand-file-name (list 'concat-package-dir package-name)))


(defmacro load-pkg-loader (config-file-name)
  `(load-file (expand-file-name (concat configuration-directory ,config-file-name))))


(defun load-configuration ()
  "Queries for a package and loads its pkg-loader."
  (interactive)
  (load-pkg-loader (concat (read-from-minibuffer "Package: ") ".el")))


(add-to-list 'load-path packages-directory)

;;; pkg-config.el ends here
