;; tavora config
;; (eval-after-load 'ispell
;;   `(progn
;;      (add-to-list 'exec-path "c:/tmp/hunspell/bin")
;;      (setq ispell-program-name "hunspell")
;;      (setq ispell-really-aspell nil)
;;      (setq ispell-library-directory "c:/tmp/hunspell/share/hunspell")))

;; M-x ispell-change-dictionary
;; M-x ispell-region
;; M-x ispell-buffer


(eval-after-load 'ispell
  `(progn
     (add-to-list 'exec-path "c:/tmp/hunspell/bin")
     (setq ispell-program-name "hunspell")
     (setq ispell-library-directory "c:/tmp/hunspell/share/hunspell")
     (setq ispell-local-dictionary-alist
           '(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil iso-8859-1)
             ("portugues" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "pt_PT") nil iso-8859-1)))))
