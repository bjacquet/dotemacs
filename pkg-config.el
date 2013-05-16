;; -*- coding: utf-8; -*-
;;; pkg-config.el - Top level definitions.


(defvar packages-directory (concat emacs-dir "packages/")
  "The packages directory.")


(defvar configuration-directory (concat emacs-dir "pkg-loaders/")
  "The directory of package loading configuration.")


(defmacro concat-package-dir (package-name)
  "(concat packages-directory package-name)"
  (list 'concat packages-directory package-name))


(defmacro expand-package (package-name)
  "(expand-file-name (concat-package-dir package-name))"
  (list 'expand-file-name (list 'concat-package-dir package-name)))


(defmacro load-pkg-loader (config-file-name)
  `(load-file (expand-file-name (concat configuration-directory ,config-file-name))))


(defun available-configurations ()
  (directory-files configuration-directory nil ".el$"))


(defun load-configuration ()
  "Queries for a pkg-loader and loads it."
  (interactive)
  (let ((configuration (completing-read "Package: " (available-configurations))))
    (load-pkg-loader configuration)
    (load-library configuration)))


(add-to-list 'load-path packages-directory)

;;; pkg-config.el ends here
