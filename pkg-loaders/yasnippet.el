;;; ---------------------------------------------------------------------
;;; YaSnippet
;;;
(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

(eval-after-load 'yasnippet
  ;; Initialize Yasnippet
  ;; Don't map TAB to yasnippet
  ;; In fact, set it to something we'll never use because
  ;; we'll only ever trigger it indirectly.
  '(progn
     (setq yas-snippet-dirs (list yas-installed-snippets-dir
                                  (concat emacs-dir "packages/snippets/")))
     (yas-global-mode 1)))
