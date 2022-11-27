;; -*- coding: utf-8; -*-
;;; home.el - Home configuration

(defvar emacs-dir "~/.emacs.d/")

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
 '(ansi-term-color-vector
   [unspecified "#1F1611" "#660000" "#144212" "#EFC232" "#5798AE" "#BE73FD" "#93C1BC" "#E6E1DC"] t)
 '(case-fold-search t)
 '(custom-safe-themes
   '("ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(dired-dnd-protocol-alist nil)
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#452E2E")
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(next-line-add-newlines nil)
 '(next-line-extends-end-of-buffer nil)
 '(package-selected-packages
   (quote
    (auto-compile all-the-icons-dired all-the-icons dashboard react-snippets js-react-redux-yasnippets yasnippet-snippets yasnippet panda-theme ranger treemacs-magit treemacs-icons-dired treemacs-projectile deft goto-last-change json-mode detour golden-ratio htmlize flychek jsx-mode darkroom flyspell-popup rinari ag handlebars-mode handlebars-sgml-mode slim-mode sr-speedbar robe shrink-whitespace wn-mode winpoint tangotango-theme tango-2-theme smart-mode-line rust-mode paredit pager naquadah-theme multiple-cursors markdown-mode haskell-mode expand-region diminish darktooth-theme color-theme-modern chess birds-of-paradise-plus-theme arbitools)))
 '(pdf-view-midnight-colors (quote ("#FDF4C1" . "#282828")))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
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
(bj:load-pkg-loader "magit.el")
(bj:load-pkg-loader "pager.el")
(bj:load-pkg-loader "paredit.el")
(bj:load-pkg-loader "shrink-whitespace.el")
(bj:load-pkg-loader "smart-mode-line.el")
(bj:load-pkg-loader "whitespace.el")
(load "last-closed-files")


;;; home.el ends here
