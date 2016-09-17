;; -*- coding: utf-8; -*-
;;; pkg-config.el - Top level definitions.


(defvar *packages-directory* (concat emacs-dir "packages/")
  "The packages directory.")


(defvar *pkg-loaders-directory* (concat emacs-dir "pkg-loaders/")
  "The directory of package loading configuration.")


(defmacro concat-package-dir (package-name)
  "(concat *packages-directory* package-name)"
  (list 'concat *packages-directory* package-name))


(defmacro expand-package (package-name)
  "(expand-file-name (concat-package-dir package-name))"
  (list 'expand-file-name (list 'concat-package-dir package-name)))


(defmacro load-pkg-loader (config-file-name)
  `(load-file (expand-file-name (concat *pkg-loaders-directory* ,config-file-name))))


(defun available-packages ()
  (directory-files *pkg-loaders-directory* nil ".el$"))


(defun load-package ()
  "Queries for a pkg-loader and loads it."
  (interactive)
  (let ((package (completing-read "Package: " (available-packages))))
    (load-pkg-loader package)
    (load-library package)))


(add-to-list 'load-path *packages-directory*)


;; package.el

(require 'package)
(setq package-user-dir (expand-file-name *packages-directory*))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(defvar *auto-refreshed-packages* nil
  "True if `ensure-package' has already refreshed the package
list in the current session")


(defun ensure-package (name)
  (unless (package-installed-p name)
    (unless *auto-refreshed-packages*
      (package-refresh-contents)
      (setq *auto-refreshed-packages* t))
    (package-install name)))


;; use-package

(ensure-package 'use-package)
(eval-when-compile
  (require 'use-package)
(setq use-package-always-ensure t))


;;; pkg-config.el ends here
