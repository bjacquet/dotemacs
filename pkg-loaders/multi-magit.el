;;; ---------------------------------------------------------------------
;;; Multi-magit
;;;
(load-file "z:/multi-magit/multi-magit.el")
(magit-add-section-hook 'magit-status-sections-hook
                        'multi-magit-insert-repos-overview
                        nil t)

(setq multi-magit-selected-repositories
      '(;; "~/.emacs.d"
        ))

(global-set-key (kbd "C-x G") 'multi-magit-status)


(setq magit-repository-directories
      '("~/.emacs.d"))
