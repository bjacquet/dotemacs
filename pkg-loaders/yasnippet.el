;;; ---------------------------------------------------------------------
;;; YaSnippet
;;;
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yasnippet-snippets-initialize))

(use-package js-react-redux-yasnippets
  :ensure t)

(use-package react-snippets
  :ensure t)
