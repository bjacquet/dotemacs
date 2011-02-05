(set-default-font "DejaVu Sans Mono-11")
(scroll-bar-mode nil)

(eval-after-load "bjacquet-init"
  (load-file (expand-file-name (concat emacs-extras-d "/sc-org/sc-org.el")))
  (load-file (expand-file-name (concat emacs-extras-d "/w3m.el")))
  (load-file (expand-file-name (concat emacs-extras-d "/jabber.el")))
  (load-file (expand-file-name (concat emacs-extras-d "/gnus.el"))))


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
