;;; ---------------------------------------------------------------------
;;; JavaScript
;;;


;; Setup taken from https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (setq js-indent-level 2))

(use-package xref-js2
  :ensure t
  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package company
  :ensure t)

;; May 20, 2020: Company-tern has been removed from cyberspace.

;; (use-package company-tern
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-tern)
;;   (add-hook 'js2-mode-hook (lambda ()
;;                              (tern-mode)
;;                              (company-mode)))

;;   ;; Disable completion keybindings, as we use xref-js2 instead
;;   (define-key tern-mode-keymap (kbd "M-.") nil)
;;   (define-key tern-mode-keymap (kbd "M-,") nil))

(use-package indium
  :ensure t
  :config
  (add-hook 'js-mode-hook #'indium-interaction-mode))

;; (use-package rjsx-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))
