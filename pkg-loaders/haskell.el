;;; ---------------------------------------------------------------------
;;; Haskell
;;;
(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :init  (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))
  :bind ("C-c C-l" . haskell-process-load-or-reload)
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-program-name "/usr/bin/ghci -fobject-code"))
