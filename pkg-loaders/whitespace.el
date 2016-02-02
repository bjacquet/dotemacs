;;; ---------------------------------------------------------------------
;;; Whitespace
;;;
(global-whitespace-mode 1)

(setq whitespace-style
      (quote (face
              tabs
              tab-mark
              space-before-tab
              space-after-tab
              trailing)))

(setq-default indicate-empty-lines t)
