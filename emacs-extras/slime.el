
(cond ((string-equal (system-name) "owl") 
       (setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
       (add-to-list 'load-path "~/quicklisp/dists/quicklisp/software/slime-20110219-cvs/")  ; your SLIME directory
       (require 'slime-autoloads)
       (slime-setup '(slime-fancy)))
      ((string-equal (system-name) "puffin.lan")
       (load (expand-file-name "~/quicklisp/slime-helper.el"))
       (setq inferior-lisp-program "~/Desktop/sbcl-1.0.51-x86-darwin/run-sbcl.sh")))
