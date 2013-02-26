;;; ---------------------------------------------------------------------
;;; W3M
;;;
(add-to-list 'load-path (concat-package-dir "emacs-w3m"))
(require 'w3m-load)
(setq w3m-use-cookies t)
(setq w3m-home-page "http://intranet")
(setq w3m-bookmark-file "~/My Documents/bookmarks/w3m.bmk")
(global-set-key (kbd "C-c g") 'w3m-search) ; google search

(defun my-dictionary-lookup (word)
  (interactive "sProcurar palavra no dicionário: ")
  (w3m-browse-url
   (concat "http://www.priberam.pt/DLPO/default.aspx?pal="
	   (url-hexify-string word))
   t))
(global-set-key (kbd "C-c d") 'my-dictionary-lookup)
