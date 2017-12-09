;;; ---------------------------------------------------------------------
;;; Slime - for use at home
;;;

(load (expand-file-name "~/Applications/quicklisp/slime-helper.el"))

(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-asdf slime-indentation slime-banner
               slime-tramp slime-mdot-fu slime-quicklisp))
(setq inferior-lisp-program "sbcl")

(setq lisp-indent-function 'common-lisp-indent-function)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(setq common-lisp-hyperspec-root "~/Documents/References/HyperSpec/")

(global-set-key "\C-cs" 'slime-selector)
