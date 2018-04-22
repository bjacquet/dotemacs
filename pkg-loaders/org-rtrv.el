;;; ---------------------------------------------------------------------
;;; Org-mode at Runtime Revolution
;;;

(bj:load-pkg-loader "org.el")

(setq org-agenda-files
      '("~/Documents/Blog/"
        "~/Documents/Diary/"))
(bj:load-pkg-loader "./org-agenda-files.el")

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "WIP(w)" "AWAITING(a)" "PENDING(p)" "|" "FIXED(f)" "SEP(s)")
        (sequence "|" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "black" :background "red2"))
        ("WIP" .  (:foreground "black" :background "yellow" :weigth bold))
        ;;        ("CANCELED" . (:foreground "blue" :weight bold))
        ("WFF" .  (:foreground "black" :background "goldenrod1"))
        ("CANCELED" . (:foreground "green" :background "black" :weight bold))))
