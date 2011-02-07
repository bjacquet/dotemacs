;;; ---------------------------------------------------------------------
;;; GNUS
;;;
(setq gnus-select-method '(nntp "news.gmane.org"))
(setq gnus-read-newsrc-file nil)
(add-hook 'gnus-summary-mode-hook 'my-setup-hl-line)
(add-hook 'gnus-group-mode-hook 'my-setup-hl-line)

(defun my-setup-hl-line ()
 (hl-line-mode 1)
 (setq cursor-type nil))

;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(when window-system
 (setq gnus-sum-thread-tree-indent "  ")
 (setq gnus-sum-thread-tree-root "● ")
 (setq gnus-sum-thread-tree-false-root "◯ ")
 (setq gnus-sum-thread-tree-single-indent "◎ ")
 (setq gnus-sum-thread-tree-vertical        "│")
 (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
 (setq gnus-sum-thread-tree-single-leaf     "╰─► ")
 (setq gnus-user-date-format-alist '(;; ((gnus-seconds-today)
                                     ;;  . "%k:%M")
                                     ;; (604800 . "%a %k:%M")
                                     ;; ((gnus-seconds-month)
                                     ;;  . "%a %d")
                                     ((gnus-seconds-year)
                                      . "%d %b '%y")
                                     (t . "%d %b '%y"))))
(setq gnus-summary-line-format
     (concat
      "%0{%U%R%z%}"
      "%3{│%}" "%1{%&user-date;%}" "%3{│%}" ;; date
      "  "
      "%4{%-15,15f%}"               ;; name
      "  "
      "%3{│%}"
      " "
      "%1{%B%}"
      "%s\n"))
(setq gnus-summary-display-arrow t)
(gnus-add-configuration
 '(summary
   (vertical 1.0
	     (horizontal 0.25
			 [group 0.30]
			 [summary 1.0 point])
	     (vertical 1.0
		       [article 1.0]))))
