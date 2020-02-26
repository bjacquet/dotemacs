;;; ---------------------------------------------------------------------
;;; For RoR development at Runtime Revolution
;;;

;;; auto-complete configuration
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)

(use-package enh-ruby-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               '("\\(?:\\.rb\\|arb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  ;; (add-hook 'enh-ruby-mode-hook 'yard-mode)
  (add-hook 'enh-ruby-mode 'smartparens-minor-mode)
  (add-hook 'enh-ruby-mode 'projectile-rails-mode)
  (setq enh-ruby-add-encoding-comment-on-save nil))

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

;; Either use this or projectile-rails.
;; (use-package rinari :ensure t)

(use-package projectile-rails
  :ensure t
  :config
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package rvm
  :ensure t
  :config (rvm-use-default))
