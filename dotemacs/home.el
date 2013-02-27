;; -*- coding: utf-8; -*-
;;; home.el - Home configuration


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
 '(user-mail-address "bruno.jacquet@gmail.com")
 '(user-full-name "Bruno Jacquet")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(tool-bar-mode nil)
 '(set-default-font "DejaVu Sans Mono-11")
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
 ;'(iswitchb-mode t)         ; intelligent buffer switcher (in minibuffer)
 '(ido-enable-flex-matching t)
 '(ide-everywhere t)
 '(ido-mode t)
 '(setq kmacro-call-mouse-event nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )


;;; ---------------------------------------------------------------------
;;; Package's Configurations
;;;
(load-file (expand-file-name (concat emacs-dir "/pkg-config.el")))
(load-pkg-loader "auto-complete.el")
(load-pkg-loader "color-theme.el")
(load-pkg-loader "magit.el")
(load-pkg-loader "pager.el")
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
(global-set-key "\C-cl" 'goto-line)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-xO" 'previous-multiframe-window)


(load-file (expand-file-name (concat emacs-dir "/defuns.el")))

;;; home.el ends here
