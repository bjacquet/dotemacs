;;; ---------------------------------------------------------------------
;;; YaSnippet
;;;
(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

(use-package js-react-redux-yasnippets
  :ensure t)

(use-package react-snippets
  :ensure t)

(eval-after-load 'yasnippet
  ;; Initialize Yasnippet
  ;; Don't map TAB to yasnippet
  ;; In fact, set it to something we'll never use because
  ;; we'll only ever trigger it indirectly.
  '(progn
     (setq yas-snippet-dirs
           (list (concat emacs-dir "packages/yasnippet-snippets-20200122.1140/snippets")
                 (concat emacs-dir "packages/snippets/")))
     (yas-global-mode 1)))
