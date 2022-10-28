;; -*- coding: utf-8; -*-

;;; ---------------------------------------------------------------------
;;; package --- Bruno Jacquet Emacs config
;;;

(defun load-config()
  "Load the actual configuration in literate 'org-mode' elisp."
  (interactive)
  (org-babel-load-file "~/.config/emacs/future-configuration.org"))

(load-config)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-pomodoro org-fancy-priorities elfeed yaml-mode web-mode tide feature-mode rvm projectile-rails smartparens enh-ruby-mode mix flycheck-credo rg org-roam org-bullets multiple-cursors auto-complete yasnippet-snippets yasnippet wn-mode winpoint undo-tree treemacs-projectile treemacs-magit treemacs-icons-dired sr-speedbar smart-mode-line slim-mode shrink-whitespace selectrum-prescient selectrum rust-mode robe rjsx-mode rinari react-snippets ranger paredit panda-theme pager nord-theme neotree markdown-mode marginalia json-mode js-react-redux-yasnippets ido-vertical-mode htmlize haskell-mode handlebars-sgml-mode handlebars-mode goto-last-change golden-ratio flyspell-popup flycheck expand-region elixir-mode dracula-theme detour deft dashboard darktooth-theme darkroom color-theme-modern bm birds-of-paradise-plus-theme all-the-icons-dired all-the-icons ag use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
