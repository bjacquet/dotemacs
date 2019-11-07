;;; ---------------------------------------------------------------------
;;; Org-mode at Runtime Revolution
;;;

(bj:load-pkg-loader "org.el")

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "WIP(w)" "HOLD(h)" "BLOCKED(b)"  "IN-REVIEW(r)" "QA(q)" "RTD" "preRTD" "|" "FIXED(f)" "SEP(s)")
        (sequence "|" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO"      . (:foreground "black" :background "red2"))
        ("WIP"       . (:foreground "black" :background "yellow" :weigth bold))
        ("BLOCKED"   . (:foreground "white" :background "firebrick" :weight bold))
        ("IN-REVIEW" . (:foreground "black" :background "goldenrod1"))
        ("CANCELED"  . (:foreground "green" :background "black" :weight bold))))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
