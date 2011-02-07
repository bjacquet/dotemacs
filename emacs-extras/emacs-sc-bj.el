(set-default-font "DejaVu Sans Mono-11")
(scroll-bar-mode nil)

(eval-after-load
    "bjacquet-init"
  (load-file (expand-file-name "~/emacs/emacs-extras/sc-org/sc-org.el")))


;;; ---------------------------------------------------------------------
;;; W3M
;;;
(add-to-list 'load-path "~/emacs/emacs-extras/emacs-w3m/")
(require 'w3m-load)
(setq w3m-home-page "http://intranet")
(setq w3m-use-cookies t)
(setq w3m-bookmark-file "~/Documents/bookmarks/w3m.bmk")

(global-set-key (kbd "C-c g") 'w3m-search) ; google search

(defun my-dictionary-lookup (word)
  (interactive "sProcurar palavra no dicionário: ")
  (w3m-browse-url
   (concat "http://www.priberam.pt/DLPO/default.aspx?pal="
	   (url-hexify-string word))
   t))

(global-set-key (kbd "C-c d") 'my-dictionary-lookup)


;;; ---------------------------------------------------------------------
;;; Emacs Jabber
;;;
(add-to-list 'load-path "~/emacs/emacs-extras/emacs-jabber/")
(require 'jabber-autoloads)
(load-file (expand-file-name "~/emacs/emacs-extras/jabber-config.el"))


;;; ---------------------------------------------------------------------
;;; GNUS
;;;
(setq gnus-select-method '(nntp "news.gmane.org"))

(add-hook 'gnus-summary-mode-hook 'my-setup-hl-line)
(add-hook 'gnus-group-mode-hook 'my-setup-hl-line)

(defun my-setup-hl-line ()
  (hl-line-mode 1)
  (setq cursor-type nil) ; Comment this out, if you want the cursor to
             ; stay visible.
  )

;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► ")
  (setq gnus-user-date-format-alist '(;; ((gnus-seconds-today)
				      ;;  . "%k:%M")
				      ;; (604800 . "%a %k:%M")
				      ;; ((gnus-seconds-month)
				      ;;  . "%a %d")
				      ((gnus-seconds-year)
				       . "%b %d '%y")
				      (t . "%b %d '%y"))))
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%&user-date;%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
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


(defun start.up ()
  "default setup"
  (interactive)
  (org-agenda-list 1)
  (eshell)
  (find-file "~/Documents/diary/TSS.org")
  (find-file "~/Documents/diary/siscog.org")
  (delete-other-windows)
  (split-window-vertically)
  (split-window-horizontally)
  (previous-multiframe-window)
  (split-window-horizontally)
  (other-window 2)
  (switch-to-buffer "siscog.org")
  (other-window 1)
  (switch-to-buffer "*Org Agenda*")
  (other-window 1)
  (switch-to-buffer "TSS.org")
  (other-window 1)
  (switch-to-buffer "*eshell*")
  (other-window 1))

(add-hook 'after-init-hook (start.up))

(global-set-key (kbd "C-z")
		(lambda ()
		  (interactive)
		  (start.up)
		  (suspend-frame)))

(define-key global-map [f6] 'start.up)
