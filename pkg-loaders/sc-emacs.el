;;; ---------------------------------------------------------------------
;;; SC-EMACS
;;;

(setenv "SISCOG_EMACS_DIR" "y:/.emacs.d/packages/sc-emacs")
(setq sc-legacy-mode nil)
(load (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))
(load (format "%s/custom/sc-user-param.el" (getenv "SISCOG_EMACS_DIR_LOCAL")))