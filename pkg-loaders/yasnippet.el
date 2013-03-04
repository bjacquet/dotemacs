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
    (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
    (yas/initialize)
    (yas/load-directory (expand-package "yasnippet/snippets"))))
