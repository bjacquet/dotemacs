;; -*- coding: utf-8; -*-
;;; rtrv.el - Runtime Revolution configuration

(load-file (expand-file-name "~/.emacs.d/future.emacs.el"))

(defvar emacs-dir "~/.emacs.d/")

(defconst rtrvp t)

(setq mac-option-key-is-meta  nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier    'meta)
(setq mac-option-modifier     nil)

;;; ---------------------------------------------------------------------
;;; Look & Feel
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(custom-safe-themes
   '("ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(dired-dnd-protocol-alist nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(next-line-add-newlines nil)
 '(next-line-extends-end-of-buffer nil)
 '(org-hide-leading-stars t)
 '(package-selected-packages
   '(all-the-icons-dired all-the-icons dashboard react-snippets js-react-redux-yasnippets yasnippet-snippets yasnippet panda-theme ranger treemacs-magit treemacs-icons-dired treemacs-projectile deft goto-last-change json-mode detour golden-ratio org-pomodoro htmlize flychek jsx-mode darkroom flyspell-popup rinari ag handlebars-mode handlebars-sgml-mode slim-mode sr-speedbar robe shrink-whitespace wn-mode winpoint undo-tree smart-mode-line rust-mode paredit pager naquadah-theme multiple-cursors markdown-mode haskell-mode expand-region diminish darktooth-theme color-theme-modern chess bm birds-of-paradise-plus-theme auto-complete arbitools))
 '(recentf-max-saved-items nil)
 '(recentf-mode t)
 '(scroll-step 1)
 '(setq kmacro-call-mouse-event)
 '(show-paren-mode t)
 '(smartparens-global-mode t)
 '(tab-always-indent 'complete)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style 'forward nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; ---------------------------------------------------------------------
;;; Package's Configurations
;;;
(load-file (expand-file-name (concat emacs-dir "/pkg-config.el")))
(bj:load-pkg-loader "auto-complete.el")
(bj:load-pkg-loader "avy.el")
(bj:load-pkg-loader "bm.el")
(bj:load-pkg-loader "dashboard.el")
(bj:load-pkg-loader "deft.el")
(bj:load-pkg-loader "email.el")
(bj:load-pkg-loader "expand-region.el")
(bj:load-pkg-loader "goto-last-change.el")
(bj:load-pkg-loader "htmlize.el")
(bj:load-pkg-loader "javascript.el")
(bj:load-pkg-loader "magit.el")
(bj:load-pkg-loader "multiple-cursors.el")
(bj:load-pkg-loader "pager.el")
(bj:load-pkg-loader "projectile.el")
(bj:load-pkg-loader "org-rtrv.el")
(bj:load-pkg-loader "paredit.el")
(bj:load-pkg-loader "ruby.el")
(bj:load-pkg-loader "shrink-whitespace.el")
(bj:load-pkg-loader "smart-mode-line.el")
(bj:load-pkg-loader "spellings.el")
(bj:load-pkg-loader "treemacs.el")
(bj:load-pkg-loader "whitespace.el")
(bj:load-pkg-loader "wn.el")
(bj:load-pkg-loader "yaml.el")
(bj:load-pkg-loader "yasnippet.el")
(load "last-closed-files")
(load-file (expand-file-name (concat emacs-dir "key-bindings.el")))


;;; ---------------------------------------------------------------------
;;; Open Dashboard
;;;
(bj:open-dashboard)

;;; rtrv.el ends here
