;; -*- coding: utf-8; -*-
;;; pkg-config.el - Top level definitions.


(defvar *bj:packages-directory* (concat emacs-dir "packages/")
  "The packages directory.")


(defvar *bj:pkg-loaders-directory* (concat emacs-dir "pkg-loaders/")
  "The directory of package loading configuration.")


(defmacro bj:concat-package-dir (package-name)
  "(concat *bj:packages-directory* package-name)"
  (list 'concat *bj:packages-directory* package-name))


(defmacro bj:expand-package (package-name)
  "(expand-file-name (bj:concat-package-dir package-name))"
  (list 'expand-file-name (list 'bj:concat-package-dir package-name)))


(defmacro bj:load-pkg-loader (config-file-name)
  `(load-file (expand-file-name (concat *bj:pkg-loaders-directory* ,config-file-name))))


(defun bj:available-packages ()
  (directory-files *bj:pkg-loaders-directory* nil ".el$"))


(defun bj:load-package ()
  "Queries for a pkg-loader and loads it."
  (interactive)
  (let ((package (completing-read "Package: " (bj:available-packages))))
    (bj:load-pkg-loader package)
    (load-library package)))


(add-to-list 'load-path *bj:packages-directory*)


;; package.el

(require 'package)
(setq package-user-dir (expand-file-name *bj:packages-directory*))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(defvar *auto-refreshed-packages* nil
  "True if `ensure-package' has already refreshed the package
list in the current session")


(defun bj:ensure-package (name)
  (unless (package-installed-p name)
    (unless *auto-refreshed-packages*
      (package-refresh-contents)
      (setq *auto-refreshed-packages* t))
    (package-install name)))


;; use-package

(bj:ensure-package 'use-package)
(eval-when-compile
  (require 'use-package)
(setq use-package-always-ensure t))


;;; pkg-config.el ends here
