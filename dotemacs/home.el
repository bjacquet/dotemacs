;; -*- coding: utf-8; -*-
;;; home.el - Home configuration


(defvar emacs-dir
  "~/.emacs.d/")


(defvar mac-p (or (eq window-system 'ns) (eq window-system 'mac)))
(when mac-p
  (setq mac-option-key-is-meta  nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier    'meta)
  (setq mac-option-modifier     nil))


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
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(debug-on-error t)
 '(default-input-method "portuguese-prefix")
 '(global-font-lock-mode t nil (font-lock))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode t nil (ido))
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(line-number-mode t)
 '(make-backup-files nil)
 '(next-line-add-newlines nil)
 '(next-line-extends-end-of-buffer nil)
 '(org-trello-files (quote ("~/Documents/Castelo/castelo.org")))
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
 '(visible-bell t)
 '(indent-tabs-mode nil)
 '(blink-cursor-blinks 0)
 '(org-hide-leading-stars t)
 '(dired-dnd-protocol-alist nil))
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
(bj:load-pkg-loader "auto-complete.el")
(bj:load-pkg-loader "avy.el")
(bj:load-pkg-loader "color-theme.el")
(bj:load-pkg-loader "expand-region.el")
(bj:load-pkg-loader "ido-vertical.el")
(bj:load-pkg-loader "magit.el")
(bj:load-pkg-loader "pager.el")
(bj:load-pkg-loader "paredit.el")
(bj:load-pkg-loader "smart-mode-line.el")
(bj:load-pkg-loader "undo-tree.el")
;;(bj:load-pkg-loader "whitespace.el")
(bj:load-pkg-loader "winpoint.el")
(bj:load-pkg-loader "wn.el")
(load "last-closed-files")
(load-file (expand-file-name (concat emacs-dir "defuns.el")))
(load-file (expand-file-name (concat emacs-dir "key-bindings.el")))


;;; ---------------------------------------------------------------------
;;; Enable disabled commands
;;;
(put 'narrow-to-page 'disabled nil)


;;; home.el ends here
