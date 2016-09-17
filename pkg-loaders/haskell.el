;;; ---------------------------------------------------------------------
;;; Haskell
;;;
(use-package haskell-mode
   :mode "\\.hs\\'"
   :bind (("C-c C-c" . haskell-compile)
	  ("C-c C-l" . haskell-process-load-or-reload))
   :config
   (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
   (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
   (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
   (setq haskell-program-name "/usr/bin/ghci -fobject-code"))
