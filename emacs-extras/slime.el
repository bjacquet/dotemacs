(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "~/quicklisp/dists/quicklisp/software/slime-20110219-cvs/")  ; your SLIME directory
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

