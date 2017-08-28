;; -*- coding: utf-8; -*-
;;; siscog-org.el - SISCOG Org configuration


(defvar emacs-dir
  "~/.emacs.d/")


;;; ---------------------------------------------------------------------
;;; Look & Feel
;;;
(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(debug-on-error t)
 '(default-input-method "portuguese-prefix")
 '(global-font-lock-mode t nil (font-lock))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(user-mail-address "bjacquet@siscog.pt")
 '(user-full-name "B Jacquet")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(visible-bell t)
 '(inhibit-startup-message t)
 '(line-number-mode t)
 '(scroll-step 1)           ; scroll one line past the edge of the screen
 '(global-font-lock-mode t) ; sintax highlight
 '(transient-mark-mode t)   ; highlighted when the mark is active
 '(show-paren-mode t)       ; highlight matching parenthesis
 '(next-line-add-newlines nil)
 '(require-final-newline t)
 '(next-line-extends-end-of-buffer nil)
 '(auto-save-default nil)
 '(make-backup-files nil)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode t)
 '(setq kmacro-call-mouse-event nil)
 '(fill-column 80)
 '(recentf-mode t)
 '(recentf-max-saved-items 100)
 '(isearch-allow-scroll t)
 '(blink-cursor-blinks 0)
 '(calendar-week-start-day 1)
 '(dired-dnd-protocol-alist nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

(set-default-font "Consolas-12")

;; Slime messes up my frame title.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))


;;; ---------------------------------------------------------------------
;;; Package's Configurations
;;;
(load-file (expand-file-name (concat emacs-dir "pkg-config.el")))
(bj:load-pkg-loader "auto-complete.el")
(bj:load-pkg-loader "avy.el")
(bj:load-pkg-loader "color-theme.el")
(bj:load-pkg-loader "expand-region.el")
(bj:load-pkg-loader "flyspell.el")
(bj:load-pkg-loader "ido-vertical.el")
(bj:load-pkg-loader "multiple-cursors.el")
(bj:load-pkg-loader "pager.el")
(bj:load-pkg-loader "sc-org.el")
(bj:load-pkg-loader "smart-mode-line.el")
(bj:load-pkg-loader "undo-tree.el")
;;(bj:load-pkg-loader "whitespace.el")
(bj:load-pkg-loader "winpoint.el")
(bj:load-pkg-loader "wn.el")
(bj:load-pkg-loader "yasnippet.el")
(load "last-closed-files")


(setq org-agenda-files (quote ("~/Documents/diary/remember.org"
			       "~/Documents/diary/siscog-notes.org"
			       "~/Documents/diary/siscog-clock.org"
			       "~/Documents/diary/clock-tables.org"
			       "~/Documents/diary/npo-notes.org"
			       "~/Documents/diary/npo-clock.org"
			       "~/Documents/diary/diary.org")))


(defun bj:rot13 (object &optional start end)
  "Return ROT13 encryption of OBJECT, a buffer or string."
  (if (bufferp object)
      (with-current-buffer object
	(rot13-region (or start (point-min)) (or end (point-max))))
    (rot13-string object)))


(defun start.up (&optional decrypt)
  "Default setup at SISCOG for Emacs Org."
  (interactive)
  (when decrypt
    (mapcar (lambda (file)
	    (bj:rot13 (find-file file)))
	    org-agenda-files))
  (org-agenda-list 1)
  (delete-other-windows)
  (split-window-vertically)
  (previous-multiframe-window)
  (split-window-horizontally)
  (other-window 2)
  (switch-to-buffer "npo-clock.org")
  (other-window 1)
  (switch-to-buffer "diary.org")
  (other-window 1)
  (switch-to-buffer "*Org Agenda*")
  (other-window 1))

(add-hook 'after-init-hook (start.up t))


(load-file (expand-file-name (concat emacs-dir "defuns.el")))
(load-file (expand-file-name (concat emacs-dir "key-bindings.el")))
(global-set-key [f6]   'start.up)
(global-set-key "\C-z" (lambda ()
			 (interactive)
			 (start.up)
			 (suspend-frame)))

;;; ---------------------------------------------------------------------
;;; Enable disabled commands
;;;
(put 'narrow-to-page 'disabled nil)


;;; siscog-org.el ends here
