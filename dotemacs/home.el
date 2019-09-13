;; -*- coding: utf-8; -*-
;;; home.el - Home configuration

(defvar emacs-dir "~/.emacs.d/")
(load-file (expand-file-name (concat emacs-dir "defuns.el")))

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
 '(blink-cursor-blinks 0)
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   (quote
    ("7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(debug-on-error t)
 '(default-input-method "portuguese-prefix")
 '(dired-dnd-protocol-alist nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 80)
 '(global-font-lock-mode t nil (font-lock))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode t nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(line-number-mode t)
 '(make-backup-files nil)
 '(next-line-add-newlines nil)
 '(next-line-extends-end-of-buffer nil)
 '(org-hide-leading-stars t)
 '(package-selected-packages
   (quote
    (magit-todos magit-org-todos org-fancy-priorities projectile-rails ag flycheck rjsx-mode js2-mode enh-ruby-mode robe shrink-whitespace org-bullets yasnippet wn-mode winpoint undo-tree tangotango-theme tango-2-theme smart-mode-line rust-mode paredit pager naquadah-theme multiple-cursors markdown-mode ido-vertical-mode haskell-mode expand-region diminish darktooth-theme color-theme-solarized color-theme-modern chess bm birds-of-paradise-plus-theme avy auto-complete arbitools)))
 '(recentf-max-saved-items nil)
 '(recentf-mode t)
 '(require-final-newline t)
 '(scroll-step 1)
 '(setq kmacro-call-mouse-event)
 '(show-paren-mode t)
 '(tab-always-indent (quote complete))
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
(bj:load-pkg-loader "auto-complete.el")
(bj:load-pkg-loader "avy.el")
(bj:load-pkg-loader "color-theme.el")
(bj:load-pkg-loader "expand-region.el")
(bj:load-pkg-loader "ido-vertical.el")
(bj:load-pkg-loader "magit.el")
(bj:load-pkg-loader "multiple-cursors.el")
(bj:load-pkg-loader "pager.el")
(bj:load-pkg-loader "org.el")
(bj:load-pkg-loader "paredit.el")
(bj:load-pkg-loader "shrink-whitespace.el")
(bj:load-pkg-loader "smart-mode-line.el")
(bj:load-pkg-loader "undo-tree.el")
(bj:load-pkg-loader "whitespace.el")
(bj:load-pkg-loader "wn.el")
(load "last-closed-files")
(load "random-font")
(load-file (expand-file-name (concat emacs-dir "key-bindings.el")))


;;; ---------------------------------------------------------------------
;;; Enable disabled commands
;;;
(put 'narrow-to-page 'disabled nil)


;;; home.el ends here
