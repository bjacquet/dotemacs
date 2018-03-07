;;; ---------------------------------------------------------------------
;;; Spellings - ispell, flyspell, flyspell-popup
;;;

;; Local Variables:
;; lexical-binding: t
;; End:

;; (bj:ensure-package 'flyspell)
;; (setq flyspell-mode-line-string nil)
;; (let ((idx 0)
;;       (options (list 'flyspell-mode
;;                      'flyspell-prog-mode
;;                      'flyspell-mode-off)))
;;   (setf (cdr (last options)) options)
;;   (defun flyspell-mode-cycle ()
;;     (interactive)
;;     (let ((option (elt options idx)))
;;       (setf idx (1+ idx))
;;       (message "%s" option)
;;       (funcall option))))

;; (global-set-key [f7]   'flyspell-mode-cycle)
;; (global-set-key [S-f7] 'flyspell-buffer)


(use-package flyspell
  :ensure t
  :delight
  :hook ((text-mode
          git-commit-mode
          org-mode) . turn-on-flyspell)
  :config
  (progn
    (setq ispell-program-name "/usr/local/Cellar/aspell/0.60.6.1_1/bin/aspell"
           ;; ispell-local-dictionary "en_US"
           ;; ispell-current-dictionary "en_US"
          )
    (use-package flyspell-popup
      :ensure t
      :commands (flyspell-popup-correct)
      :bind (:map flyspell-mode-map
             ("C-;" . flyspell-popup-correct))
      )))
