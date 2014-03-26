;;; ---------------------------------------------------------------------
;;; YaSnippet
;;;
(add-to-list 'load-path (expand-package "yasnippet"))
(eval-after-load 'yasnippet
  ;; Initialize Yasnippet
  ;; Don't map TAB to yasnippet
  ;; In fact, set it to something we'll never use because
  ;; we'll only ever trigger it indirectly.
  '(progn
     (require 'yasnippet)
     (setq yas-snippet-dirs (list (expand-package "yasnippet/snippets/")
				  (concat emacs-dir "packages/snippets/")))
     (yas-global-mode 1)
     (yas--load-directory-1 (concat emacs-dir "packages/snippets/sc-mode/")
      			    'text-mode)
     (yas--load-directory-1 (concat emacs-dir "packages/snippets/sc-mode/")
			    'fundamental-mode)))
