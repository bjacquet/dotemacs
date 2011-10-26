;; -*- coding: utf-8; -*-
;;; .emacs - SISCOG specific
;;;
(set-default-font "DejaVu Sans Mono-11")
(scroll-bar-mode nil)


;;; ---------------------------------------------------------------------
;;; Modes loaded on request
;;;
(add-to-list 'load-path (concat emacs-extras-d "/load-later"))


;;; ---------------------------------------------------------------------
;;; Extra Utilities
;;;
(unless (featurep 'bjacquet-init)
  (load-library "bjacquet-init.el"))


(eval-after-load "bjacquet-init"
  '(progn
    (load-file (expand-file-name (concat emacs-extras-d "/sc-org/sc-org.el")))
    (load-file (expand-file-name (concat emacs-extras-d "/w3m.el")))
    (load-file (expand-file-name (concat emacs-extras-d "/jabber.el")))))

(setq org-agenda-files (quote ("~/My Documents/diary/remember.org"
			       "~/My Documents/diary/tss-notes.org"
			       "~/My Documents/diary/tss-clock.org"
			       "~/My Documents/diary/siscog-notes.org"
			       "~/My Documents/diary/siscog-clock.org"
			       "~/My Documents/diary/clock-tables.org"))
      desktop-menu-directory "~/My Documents/diary/desktop/")

(defun start.up ()
  "default setup"
  (interactive)
  (org-agenda-list 1)
  (eshell)
  (find-file "~/My Documents/diary/tss-clock.org")
  (find-file "~/My Documents/diary/tss-notes.org")
  (delete-other-windows)
  (split-window-vertically)
  (previous-multiframe-window)
  (split-window-horizontally)
  (other-window 2)
  (switch-to-buffer "tss-notes.org")
  (other-window 1)
  (switch-to-buffer "tss-clock.org")
  (other-window 1)
  (switch-to-buffer "*Org Agenda*")
  (other-window 1))

(add-hook 'after-init-hook (start.up))

(global-set-key (kbd "C-z")
		(lambda ()
		  (interactive)
		  (start.up)
		  (suspend-frame)))

(define-key global-map [f6] 'start.up)
