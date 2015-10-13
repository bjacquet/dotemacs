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
 '(isearch-allow-scroll t))
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
(load-pkg-loader "avy.el")
(load-pkg-loader "auto-complete.el")
(load-pkg-loader "color-theme.el")
(load-pkg-loader "expand-region.el")
(load-pkg-loader "flyspell.el")
(load-pkg-loader "ido-vertical.el")
(load-pkg-loader "jabber.el")
(load-pkg-loader "multiple-cursors.el")
(load-pkg-loader "pager.el")
(load-pkg-loader "powerline.el")
(load-pkg-loader "sc-org.el")
(load-pkg-loader "undo-tree.el")
(load-pkg-loader "winpoint.el")
(load-pkg-loader "yasnippet.el")
(load "last-closed-files")


(setq org-agenda-files (quote ("~/Documents/diary/remember.org"
			       "~/Documents/diary/tk-notes.org"
			       "~/Documents/diary/tk-clock.org"
			       "~/Documents/diary/tk.trello"
			       "~/Documents/diary/tpo-notes.org"
			       "~/Documents/diary/tpo-clock.org"
			       "~/Documents/diary/tss-notes.org"
			       "~/Documents/diary/tss-clock.org"
			       "~/Documents/diary/siscog-notes.org"
			       "~/Documents/diary/siscog-clock.org"
			       "~/Documents/diary/clock-tables.org"
			       "~/Documents/diary/npo-notes.org"
			       "~/Documents/diary/npo-clock.org"))
      desktop-menu-directory "~/Documents/diary/desktop/")

;; (setq org-trello-files (quote ("~/Documents/diary/todos-os-todos.trello"
;; 			       ;; "~/Documents/diary/remember.org"
;; 			       ;; "~/Documents/diary/tk-notes.org"
;; 			       ;; "~/Documents/diary/tpo-notes.org"
;; 			       ;; "~/Documents/diary/tss-notes.org"
;; 			       ;; "~/Documents/diary/siscog-notes.org"
;; 			       ;; "~/Documents/diary/vrc-notes.org"
;; 			       )))


(defun start.up ()
  "Default setup at SISCOG for Emacs Org."
  (interactive)
  (org-agenda-list 1)
  (eshell)
  (find-file "~/Documents/diary/npo-clock.org")
  (find-file "~/Documents/diary/npo-notes.org")
  (delete-other-windows)

  (split-window-vertically)
  (previous-multiframe-window)
  (split-window-horizontally)
  (other-window 2)
  (switch-to-buffer "npo-notes.org")
  (other-window 1)
  (switch-to-buffer "npo-clock.org")
  (other-window 1)
  (switch-to-buffer "*Org Agenda*")
  (other-window 1))

(add-hook 'after-init-hook (start.up))


;;; ---------------------------------------------------------------------
;;; Keys
;;;
(global-set-key [home]  'beginning-of-line)
(global-set-key [end]   'end-of-line)
(global-set-key [f5]    'comment-region)
(global-set-key [S-f5]  'uncomment-region)
(global-set-key [f6]    'start.up)
(global-set-key [f8]    'find-file-at-point)
(global-set-key [f9]    'last-closed-files)
(global-set-key [S-f9]  'recentf-open-files)
(global-set-key "\C-cl" 'goto-line)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-xO" 'previous-multiframe-window)
(global-set-key "\C-z"  (lambda ()
			  (interactive)
			  (start.up)
			  (suspend-frame)))


(load-file (expand-file-name (concat emacs-dir "defuns.el")))


;;; ---------------------------------------------------------------------
;;; Enable disabled commands
;;;
(put 'narrow-to-page 'disabled nil)


;;; siscog-org.el ends here
