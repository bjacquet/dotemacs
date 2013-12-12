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
 ;; '(pc-select-meta-moves-sexps t)         ; marked as obsolete
 ;; '(pc-selection-mode t nil (pc-select))  ; marked as obsolete
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
 '(iswitchb-mode t)         ; intelligent buffer switcher (in minibuffer)
 ;; '(ido-enable-flex-matching t)
 ;; '(ide-everywhere t)
 ;; '(ido-mode t)
 '(setq kmacro-call-mouse-event nil)
 '(fill-column 80)
 '(recentf-mode t)
 '(recentf-max-saved-items nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

(set-default-font "DejaVu Sans Mono-11")

;; Slime messes up my frame title.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))


;;; ---------------------------------------------------------------------
;;; Package's Configurations
;;;
(load-file (expand-file-name (concat emacs-dir "pkg-config.el")))
(load-pkg-loader "auto-complete.el")
(load-pkg-loader "color-theme.el")
(load-pkg-loader "flyspell.el")
(load-pkg-loader "pager.el")
(load-pkg-loader "sc-org.el")
(load-pkg-loader "undo-tree.el")
(load-pkg-loader "w3m.el")
(load-pkg-loader "winpoint.el")
(load-pkg-loader "yasnippet.el")
(load "last-closed-files")


(setq org-agenda-files (quote ("~/My Documents/diary/remember.org"
;			       "~/My Documents/diary/tk-notes.org"
			       "~/My Documents/diary/tk-clock.org"
;			       "~/My Documents/diary/tpo-notes.org"
			       "~/My Documents/diary/tpo-clock.org"
;			       "~/My Documents/diary/tss-notes.org"
			       "~/My Documents/diary/tss-clock.org"
;			       "~/My Documents/diary/vip-notes.org"
			       "~/My Documents/diary/vip-clock.org"
;			       "~/My Documents/diary/siscog-notes.org"
			       "~/My Documents/diary/siscog-clock.org"
;			       "~/My Documents/diary/clock-tables.org"
			       "~/My Documents/diary/wm-clock.org"))
      desktop-menu-directory "~/My Documents/diary/desktop/")


(defun start.up ()
  "Default setup at SISCOG for Emacs Org."
  (interactive)
  (org-agenda-list 1)
  (eshell)
  (find-file "~/My Documents/diary/tpo-clock.org")
  (find-file "~/My Documents/diary/tpo-notes.org")
  (delete-other-windows)

  (split-window-vertically)
  (previous-multiframe-window)
  (split-window-horizontally)
  (other-window 2)
  (switch-to-buffer "tpo-notes.org")
  (other-window 1)
  (switch-to-buffer "tpo-clock.org")
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

;;; siscog-org.el ends here
