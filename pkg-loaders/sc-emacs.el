;;; ---------------------------------------------------------------------
;;; SC-EMACS
;;;

(setenv "SISCOG_EMACS_DIR" "z:/siscog/sc-emacs")
(setenv "PATH" (format "d:\\tmp\\cygwin64\\bin;%s"
                       (getenv "PATH")))
(setq sc-legacy-mode nil)
(load (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))


;;; ---------------------------------------------------------------------
;;; SC User Parameters
;;;


;;;-----------------------------------------------------------------------------
;;; Local CREWS top directory (see sc-models.el)
;;;-----------------------------------------------------------------------------
(setq *sc-local-repository-dir*  "z:/siscog")

;;;-----------------------------------------------------------------------------
;;; Local CREWS top directory for binaries and patches
;;; (see 'sc-models.el' and 'sc-run-crews-images.el' (definition 'sc-select-appl-version-dir'))
;;;-----------------------------------------------------------------------------
(setq *sc-binary-repository-dir* "y:/siscog")

;;;-----------------------------------------------------------------------------
;;; SISCOG area CREWS top directory (see sc-models.el)
;;;-----------------------------------------------------------------------------
(setq *sc-central-repository-dir* "x:/siscog")

;;;-----------------------------------------------------------------------------
;;; Modifications management (see sc-mod.el)
;;;-----------------------------------------------------------------------------
(setq *default-mod-date*    "09/10/06")
(setq *default-author*      "B Jacquet")
(setq *default-org-dir*     *sc-central-repository-dir*)
(setq *default-src-dir*     (getenv "SISCOG_DIR"))
(setq *default-mod-dir*     "y:/siscog/modifications")
(setq *modif-request-file*  (format "%s/modif-requests" *default-mod-dir*))
(setq *actions-log-file*    (format "%s/.big-brother-file" *default-mod-dir*))
(setq *modifs-mail-dir*     "y:/siscog/modifications/emails")
(setq *modif-mail-from*     "bjacquet@siscog.pt")
(setq *modif-mail-name*     "modif-request-mail")
(setq *mod-mail-method*     :ascii)
(setf *digest-mail-name*    "y:/mail/modifs")
(setq *emacs-init-file*     (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))
(setq *emacs-program*       "emacs")
(setq *default-acl-version* :v9-0-64)

;;;-----------------------------------------------------------------------------
;;; Modifications mail (see sc-mail.el)
;;;-----------------------------------------------------------------------------
(setq user-full-name "Bruno Jacquet")
(setq user-mail-address "bjacquet@siscog.pt")
