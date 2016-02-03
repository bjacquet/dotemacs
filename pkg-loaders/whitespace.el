;;; ---------------------------------------------------------------------
;;; Whitespace
;;;
(setq whitespace-style
      (quote (face
              tabs
              tab-mark
              space-before-tab
              space-after-tab
              trailing)))
(global-whitespace-mode 1)

(setq-default indicate-empty-lines t)
