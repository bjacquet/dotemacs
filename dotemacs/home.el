;; -*- coding: utf-8; -*-
;;; home.el - Home configuration


(defvar emacs-dir
  "~/.emacs.d/")


;;; ---------------------------------------------------------------------
;;; Look & Feel
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(debug-on-error t)
 '(default-input-method "portuguese-prefix")
 '(global-font-lock-mode t nil (font-lock))
 '(ido-mode t)
 '(ido-everywhere t)
 '(ido-enable-flex-matching t)
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(line-number-mode t)
 '(make-backup-files nil)
 '(next-line-add-newlines nil)
 '(next-line-extends-end-of-buffer nil)
 '(org-trello-files '("~/Documents/Castelo/castelo.org"))
 '(recentf-max-saved-items nil)
 '(recentf-mode t)
 '(require-final-newline t)
 '(scroll-step 1)
 '(set-default-font "DejaVu Sans Mono-11")
 '(setq kmacro-call-mouse-event)
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-full-name "Bruno Jacquet")
 '(user-mail-address "bruno.jacquet@gmail.com")
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Slime messes up my frame title.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))


;;; ---------------------------------------------------------------------
;;; Package's Configurations
;;;
(load-file (expand-file-name (concat emacs-dir "/pkg-config.el")))
(load-pkg-loader "avy.el")
(load-pkg-loader "auto-complete.el")
(load-pkg-loader "color-theme.el")
(load-pkg-loader "expand-region.el")
(load-pkg-loader "ido-vertical.el")
(load-pkg-loader "magit.el")
(load-pkg-loader "pager.el")
(load-pkg-loader "paredit.el")
(load-pkg-loader "powerline.el")
(load-pkg-loader "undo-tree.el")
(load-pkg-loader "winpoint.el")
(load "last-closed-files")


;;; ---------------------------------------------------------------------
;;; Keys
;;;
(global-set-key [home]  'beginning-of-line)
(global-set-key [end]   'end-of-line)
(global-set-key [f5]    'comment-region)
(global-set-key [S-f5]  'uncomment-region)
(global-set-key [f8]    'find-file-at-point)
(global-set-key [f9]    'last-closed-files)
(global-set-key [S-f9]  'recentf-open-files)
(global-set-key "\C-cl" 'goto-line)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-xO" 'previous-multiframe-window)


(load-file (expand-file-name (concat emacs-dir "defuns.el")))


;;; ---------------------------------------------------------------------
;;; Enable disabled commands
;;;
(put 'narrow-to-page 'disabled nil)


;;; home.el ends here
