;;; ---------------------------------------------------------------------
;;; For RoR development at Runtime Revolution
;;;

(use-package enh-ruby-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  ;; (add-hook 'enh-ruby-mode-hook 'yard-mode)
  (add-hook 'enh-ruby-mode 'smartparens-minor-mode)
  )

(use-package robe :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (require 'smartparens-ruby)
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  (sp-with-modes '(rhtml-mode)
    (sp-local-pair "<" ">")
    (sp-local-pair "<%" "%>")))

(use-package ag
  :ensure t
  :config (setq ag-executable "/usr/local/bin/ag"))

(use-package rinari :ensure t)

(use-package projectile-rails :ensure t)

(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)