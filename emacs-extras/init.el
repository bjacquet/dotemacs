;;; -*- Mode: Emacs-Lisp -*-
;:* $Id: init.el,v 1.1.1.1 2006-08-07 15:51:13 bruno Exp $
;:*======================

(message 
 (concat "welcome to [" (system-name) "]" 
		 " exporting to [" (getenv "DISPLAY") "]"))

;; ---------------------------------------------------------
;; font configuration
;; ---------------------------------------------------------

(set-face-font
 'default
;"-*-fixed-medium-r-*-*-*-120-*-*-*-*-iso8859-1"
;"-*-fixed-medium-r-*-*-*-140-*-*-*-*-iso8859-1"
"-*-courier-bold-r-*-*-*-120-*-*-*-*-iso8859-*"
;"-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*"
;"-*-courier-bold-r-*-*-*-130-*-*-*-*-iso8859-*"
;"-*-courier-medium-r-*-*-*-140-*-*-*-*-iso8859-1"
;"-*-courier-bold-r-*-*-*-140-*-*-*-*-iso8859-*"
)

;; ---------------------------------------------------------
;; Posicao da janela principal
;; ---------------------------------------------------------

;; localicacao por omissao:
(defvar *width*  80)
(defvar *height* 24)
(defvar *left*    0)
(defvar *top*     0)

;; modificacoes por estar em determinada maquina
(when (equal (system-name) "vincent.local"))
(when (equal (system-name) "bilbo.local"))
(when (equal (system-name) "anubis.local"))
(when (equal (system-name) "ronin.local"))
(when (equal (system-name) "ninfa.local"))

;; modificacoes por estar definido um DISPLAY
(when (getenv "DISPLAY")
  ;; POSICAO DA JANELA
  ;;(setq *left*  400)
  (setq *left*  258)
  (setq *top*    20)
  ;; TAMANHO DA JANELA
  (setq *width*  80)
  (setq *height* 40)
  ;; ---- moficacoes para maquinas especiais
  ;; bilbo: dualscreen 
  (when (or (equal (getenv "DISPLAY") "bilbo:0")
			(equal (getenv "DISPLAY") "bilbo.local:0"))
	(setq *left* 1580)
	(setq *top*    15)
	(setq *height* 70)
	)
  ;; ronin
  (when (or (equal (getenv "DISPLAY") "ronin:0")
			(equal (getenv "DISPLAY") "ronin.local:0"))
	;;(setq *left*  560)
	;;(setq *height* 54)
	(setq *left*  285)
	(setq *height* 20)
	(setq *height* 48)
	)
  (when (equal (getenv "DISPLAY") ":0.0")
	(setq *left*  530)
	(setq *height* 56)
	)
  (when (equal (getenv "DISPLAY") "localhost:10.0")
	(setq *left*  258)
	(setq *top*    42)
	(setq *height* 47)
	)
  (when (equal (getenv "DISPLAY") "gollum:0")
	(setq *left*  258)
	(setq *top*    42)
	(setq *height* 47)
	)
)

; para testes
;(set-frame-position *frame-da-joana* 258 42)
;(set-frame-size *frame-da-joana* 80 47)

;; utilizar definicoes
(defvar *frame-da-joana* (first (device-frame-list)))
(set-frame-size *frame-da-joana* *width* *height*)
(set-frame-position *frame-da-joana* *left* *top*)

(setq file-name-history 
	  (list (expand-file-name "~")
			(expand-file-name "~/work/tese")
			(expand-file-name "~/work/ATA")
			(expand-file-name "/afs/l2f/projects/")
			(expand-file-name "~/.xemacs/init.el")
			))

;; ---------------------------------------------------------
;; Speedbar configuration
;; ---------------------------------------------------------

(defvar *speedbar-width*  30)
(defvar *speedbar-height* 40)
(load-library "~/.xemacs/speedbar")
;; redifine-se a funcao:
(defun speedbar-frame-reposition-smartly ()
  "Reposition the speedbar frame to be next to the attached frame."
  (set-frame-position speedbar-frame (- *left* 233) *top*))
(defun my-speedbar ()
  "My speedbar is always on the same place and with the same size"
  (speedbar)
  (set-frame-size speedbar-frame *speedbar-width* *speedbar-height*)
  (set-frame-position speedbar-frame (- *left* 292) *top*))

;; so queremos comecar a speedbar se estiver definido um display
;; (getenv "XPTO") = nil (se nao estiver definido)
;;(when (getenv "DISPLAY") (my-speedbar))
(message "speedbar initialised")

;; ---------------------------------------------------------
;; some basic configs
;; ---------------------------------------------------------

(setq default-frame-alist (list (cons 'cursor-type      'box)
				(cons 'foreground-color "wheat")
				(cons 'background-color "black")))

(line-number-mode t)                    ; Display the line number on modeline
(column-number-mode t)                  ; Display the column number on modeline

(setq-default fill-column 78)           ; wrap column
(setq-default tab-width 4)              ; Show tabs as 4 cols

(setq-default transient-mark-mode t)    ; Turn on transient mark mode
(setq inhibit-startup-message t)        ; I'm sick of the welcome message


(setq require-final-newline 'ask) ;; Terminate all files with a newline
;; nil - never; t - add newline if none exists; else - ask before inserting

;; Change the pointer used when the mouse is over a modeline
(set-glyph-image modeline-pointer-glyph "leftbutton")

(require 'winring)             ; The `window ring' is *very* useful
(require 'scroll-in-place)     ; We want to scroll nicely
(require 'uniquify)            ; This really is deeply deeply nifty
(require 'icomplete)           ; So too is this
(require 'balloon-help)        ; Tooltips are go!

;; MOUSE SET UP. Paste at point NOT at cursor
(setq mouse-yank-at-point 't)


;; ---------------------------------------------------------
;; display-time config
;; ---------------------------------------------------------
;; (have to run display-time before calendar)
(setq display-time-form-list
	  '(date
		;time
		time-text
		;mail
		load
		;load-text
		))
;(setq display-time-mail-file "~/imap/Inbox")
(setq supress-day-of-week       t)
(setq supress-day-of-week       t)
(setq display-time-day-and-date t)
(setq display-time-24hr-format  t)
(setq calendar-week-start-day   1) ;; 1: Monday
;;(setq display-time-interval 15)
;(setq display-time-string-forms '((if mail "[M]" "")))
(display-time)

;:*======================
;; If you want the default colors, you could do this:
;; (setq font-lock-use-default-fonts nil)
;; (setq font-lock-use-default-colors t)
;; but I want to specify my own colors, so I turn off all
;; default values.
(setq font-lock-use-default-fonts nil)
(setq font-lock-use-default-colors nil)

(require 'font-lock)

;; Mess around with the faces a bit.  Note that you have
;; to change the font-lock-use-default-* variables *before*
;; loading font-lock, and wait till *after* loading font-lock
;; to customize the faces.

(set-face-background-pixmap 'default "")
(set-face-background-pixmap 'bold    "")

(set-face-background 'default "black")
(set-face-foreground 'default "wheat")

(set-face-background 'zmacs-region "dim gray")
(set-face-foreground 'zmacs-region "yellow")

(set-face-background 'highlight    "darkblue")
(set-face-foreground 'highlight    "cyan")

(set-face-background 'modeline     "black")
(set-face-foreground 'modeline     "wheat")

(setq x-pointer-foreground-color   "red")
(setq x-pointer-background-color   "red")
(set-face-foreground 'pointer      "green")
(set-face-background 'text-cursor  "red")

(set-face-background 'isearch      "yellow")
(set-face-foreground 'isearch      "blue")

(set-face-foreground 'font-lock-builtin-face         "SteelBlue")
(set-face-foreground 'font-lock-comment-face         "grey50")
(set-face-foreground 'font-lock-constant-face        "orange")
(set-face-foreground 'font-lock-doc-string-face      "CadetBlue")
(set-face-foreground 'font-lock-function-name-face   "ForestGreen")
(set-face-foreground 'font-lock-keyword-face         "deepskyblue") ;turquoise
(set-face-foreground 'font-lock-preprocessor-face    "Orchid")
(set-face-foreground 'font-lock-reference-face       "SteelBlue")
(set-face-foreground 'font-lock-string-face          "orange")
(set-face-foreground 'font-lock-type-face            "DodgerBlue")
(set-face-foreground 'font-lock-variable-name-face   "SeaGreen")
(set-face-foreground 'font-lock-warning-face         "red")
(set-face-foreground 'modeline-buffer-id             "RoyalBlue")

(message "colors set and ready to go")

(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
(add-hook 'lisp-mode-hook 'turn-on-font-lock)
(add-hook 'scheme-mode-hook     'turn-on-font-lock)

(add-hook 'tex-mode-hook        'turn-on-font-lock)
(add-hook 'latex-mode-hook      'turn-on-font-lock)
(add-hook 'texinfo-mode-hook    'turn-on-font-lock)

(add-hook 'c-mode-hook    'turn-on-font-lock)
(add-hook 'c++-mode-hook  'turn-on-font-lock)
(add-hook 'makefile-mode-hook   'turn-on-font-lock)
(add-hook 'tcl-mode-hook        'turn-on-font-lock)
(add-hook 'perl-mode-hook 'turn-on-font-lock)
(add-hook 'postscript-mode-hook 'turn-on-font-lock)
(add-hook 'dired-mode-hook  'turn-on-font-lock)
(add-hook 'ada-mode-hook  'turn-on-font-lock)
(add-hook 'prolog-mode-hook     'turn-on-font-lock)
(add-hook 'java-mode-hook       'turn-on-font-lock)
(add-hook 'pascal-mode-hook     'turn-on-font-lock)

(add-hook 'nroff-mode-hook      'turn-on-font-lock)
(add-hook 'sh-mode-hook         'turn-on-font-lock)
(add-hook 'news-reply-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-setup-hook      'turn-on-auto-fill)
(add-hook 'sgml-mode-hook       'turn-on-font-lock)
(add-hook 'html-mode-hook       'turn-on-auto-fill)
(add-hook 'text-mode-hook       'turn-on-auto-fill)

(message "have a colorful day")

(custom-set-variables
 '(paren-mode (quote sexp) nil (paren))
 '(font-lock-maximum-decoration t) 
 '(font-menu-ignore-scaled-fonts nil)
 '(c-basic-offset 2)
 '(font-lock-mode t nil (font-lock)))

(setq c-font-lock-keywords    c-font-lock-keywords-2
      c++-font-lock-keywords  c++-font-lock-keywords-2
      lisp-font-lock-keywords lisp-font-lock-keywords-2)

(setq font-lock-maximum-decoration t
      font-lock-maximum-size nil)

(font-lock-mode 1)

(message "basic fonts initialised")

;; ---------------------------------------------------------
;; keyboard config
;; ---------------------------------------------------------

(define-key global-map '(control right)      'forward-word)
(define-key global-map '(control left)       'backward-word)
(define-key global-map '(control tab)        'other-window)
(define-key global-map '(shift delete)       'kill-entire-line)
(define-key global-map '(control delete)     'copy-entire-line)
(define-key global-map '(shift remove)       'copy-region-as-kill)

(global-set-key [home]            'beginning-of-line)
(global-set-key [(control home)]  'beginning-of-buffer)
(global-set-key [end]             'end-of-line)
(global-set-key [(control end)]   'end-of-buffer)
(global-set-key [(meta right)]    'forward-sentence)
(global-set-key [(meta left)]     'backward-sentence)
(global-set-key [delete]          'delete-char)
(define-key global-map "\C-x\C-j" 'dired-jump)

(global-set-key [(control tab)]       'switch-to-other-buffer)
(global-set-key [(control meta tab)]  'other-window)

(message "orientation keys set!")

(global-set-key [(meta f1)]          'describe-key)
(global-set-key [(control f1)]       'describe-function)
(global-set-key [(control meta f1)]  'describe-variable)

(global-set-key [f2]                 'save-buffer)      ;; Save
(global-set-key [(shift f2)]         'write-file)       ;; Save-as...

(global-set-key [f3]                 'find-file)        ;; Open
(global-set-key [(shift f3)]         'kill-this-buffer) ;; Close

(global-set-key [f4]                 'indent-region)    ;; Indent
(global-set-key [(shift f4)]         'comment-region)
(global-set-key [(control f4)]       'ispell)

(global-set-key [f5]                 'clear-shell-buffer)
(global-set-key [(shift f5)]         'speedbar-get-focus)
(global-set-key [(control f5)]       'my-speedbar)

(global-set-key [f6]                 'other-window) ;; Toggle between two win
(global-set-key [(shift f6)]         'switch-to-other-buffer)
(global-set-key [(alt f6)]           'switch-to-previous-buffer)

(global-set-key [(shift f7)]         'term)
(global-set-key [(alt f7)]           'shell)
(global-set-key "\C-z"               'shell) ;; .emacs_shellname - shells rc



(global-set-key [f11]                'spellarticle)

(global-set-key [f12]                'tese)
(global-set-key [(shift f12)]        'alltese)
(global-set-key [(control f12)]      'spelltese)

(message "Some more... F2 ... F7")

(define-key global-map '(button1)         'mouse-track)
(define-key global-map '(button2)         'x-set-point-and-insert-selection)
(define-key global-map '(button3)         'popup-mode-menu)
(define-key global-map '(button4)         'scroll-down)
(define-key global-map '(button5)         'scroll-up)
(define-key global-map '(meta button2)    'x-insert-selection)
(define-key global-map '(control shift button1) 'copy-to-register)
(define-key global-map '(control shift button2) 'insert-register)
(define-key global-map '(control shift button3) 'view-register)

;;; Extra key bindings:
(global-set-key "\M-%" 'query-replace-regexp)

(global-set-key "\C-f" 'isearch-forward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-R" 'isearch-repeat-backward)

(global-set-key "\C-cP" 'lpr-buffer)
(global-set-key "\C-cp" 'lpr-region)

(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cd" 'gdb)
(global-set-key "\C-cL" 'goto-line)

(global-set-key "\C-cb" 'ispell-buffer)
(global-set-key "\C-cr" 'ispell-region)
(global-set-key "\C-cw" 'ispell-word)

;; ---------------------------------------------------------
;; Comandos
;; ---------------------------------------------------------

(auto-compression-mode)
(resize-minibuffer-mode)
(setq resize-minibuffer-window-exactly nil)

(setq options-save-faces t)


(defun tese ()
  (interactive)
  (message "Vou compilar a tese...")
  (compile "cd /afs/l2f/home/joana/work/msc.thesys; make"))

(defun spelltese ()
  (interactive)
  (ispell-change-dictionary "portugues")
  (ispell))

(defun spellarticle ()
  (interactive)
  (ispell-change-dictionary "english")
  (ispell))

(defun alltese ()
  (spelltese)
  (tese))

(defun clear-shell-buffer ()
  (interactive)
  (goto-char (point-min))
  (replace-string "[0m" "")
  (goto-char (point-min))
  (replace-string "[1m" "")
  (goto-char (point-min))
  (replace-string "[1;31m" "")
  (goto-char (point-min))
  (replace-string "[01;32m" "")
  (goto-char (point-min))
  (replace-string "[01;34m" "")
  (goto-char (point-min))
  (replace-string "[01;36m" "")
  (goto-char (point-min))
  (replace-string "[33m" "")
  (goto-char (point-min))
  (replace-string "[34m" "")
  (goto-char (point-min))
  (replace-string "[36m" "")
  (goto-char (point-min))
  (replace-string "[m" "")
  (goto-char (point-min))
  (replace-string "[J" "")
  (goto-char (point-min))
  (replace-string "[K" "")
  (goto-char (point-min))
  (replace-string "

" "")
  (goto-char (point-min))
  (replace-string "" "")
  (message "buffer cleaned and ready to go again...")
  (goto-char (point-max))
)

(setq compilation-window-height 10)
(setq compilation-finish-function '(lambda (&buf msg) (beep)))

(setq c++-mode-hook
      '(lambda () 
	 (or (file-exists-p "makefile") (file-exists-p "Makefile")
	     (progn (make-local-variable 'compile-command)
		    (setq compile-command
			  (concat "make -k '"
				  (file-name-sans-extension 
				   (file-name-nondirectory buffer-file-name))
				  "'"
				  ))))))
(setq c-mode-hook
      '(lambda () 
	 (or (file-exists-p "makefile") (file-exists-p "Makefile")
	     (progn (make-local-variable 'compile-command)
		    (setq compile-command
			  (concat "gcc -Wall -ansi -pedantic -g -o '"
				  (file-name-sans-extension 
				   (file-name-nondirectory buffer-file-name))
				  "' '" 
				  (file-name-nondirectory buffer-file-name)
				  "'"))))))

(message "commands initialised")

;; ---------------------------------------------------------
;; prepara o ISPELL para o portugues:
;; ---------------------------------------------------------

;;; Prepare for portuguese dictionary:
(setq ispell-dictionary-alist
	  (append 
	   '(("portuguestex"  ;; para usar em tex tradicional
		  "[A-Za-z\\\\ÀÁÂÃÈÉËÌÍÎÒÓÔÕÙÚÛÇàáâãèéêìíîòóôõùúûç]"
		  "[^A-Za-z\\\\ÀÁÂÃÈÉËÌÍÎÒÓÔÕÙÚÛÇàáâãèéêìíîòóôõùúûç]"
		  "[---~`'^{}]" t
		  ("-d" "portugues")
		  "~tex" iso-latin-1)
		 ("portugues"     ;; para usar em iso-latin1
		  "[A-Za-zÀÁÂÃÈÉËÌÍÎÒÓÔÕÙÚÛÇàáâãèéêìíîòóôõùúûç]"
		  "[^A-Za-zÀÁÂÃÈÉËÌÍÎÒÓÔÕÙÚÛÇàáâãèéêìíîòóôõùúûç]"
		  "[---']"  t 
		  ("-d" "portugues")  
		  "~lat" iso-latin-1)
		 ("br"
		  "[A-Za-zÀÁÂÃÇÉÊÍÓÔÕÚÜàáâãçéêíôõúü]"
		  "[^A-Za-zÀÁÂÃÇÉÊÍÓÔÕÚÜàáâãçéêíôõúü]"
		  "[---]" nil 
		  ("-B" "-d" "br") 
		  "~tex" iso-latin-1))
	   ispell-dictionary-alist))

(setq ispell-dictionary "portuguestex")

;; MORE RESERVED WORDS:
(setq latex-block-names '("theorem" "corollary" "proof"))
(message "ispell dict set to go")

;; -------------------------------------------------
;; System Customizations
;; -------------------------------------------------

;; Set buffer behaviour
(setq next-line-add-newlines nil)
(setq scroll-step 1)
(setq scroll-conservatively 5)

;; -------------------------------------------------
;; Misc customizations
;; -------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s by y
(defconst use-backup-dir t)             ;use backup directory
(defconst query-replace-highlight t)    ;highlight during query
(defconst search-highlight t)           ;highlight incremental search
(setq ls-lisp-dirs-first t)             ;display dirs first in dired

(message "some custumization....")

;; -------------------------------------------------
;; Ediff customizations
;; -------------------------------------------------

(defconst ediff-ignore-similar-regions t)
(defconst ediff-use-last-dir t)
(defconst ediff-diff-options " -b ")

;; -------------------------------------------------
;; Set the name of the host and current path/file in title bar:
;; -------------------------------------------------

(setq frame-title-format
      (list 
       (format "%s @ %s -> %s working on [ " 
	       (getenv "USER")(system-name) (getenv "DISPLAY"))
       '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
       (format " ]")))

(message "name of the host on the title bar...")



;; -------------------------------------------------
;; Grab possible word completions from all active sessions
;; -------------------------------------------------

(defconst dabbrev-always-check-other-buffers t)
(defconst dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

(message "hippie-expand-try-functions-list: ")

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
		try-expand-dabbrev-all-buffers
		try-expand-dabbrev-from-kill
		try-complete-file-name-partially
		try-complete-file-name
		try-complete-lisp-symbol-partially
		try-complete-lisp-symbol
		try-expand-whole-kill))

(message "More things ok!")

;; -------------------------------------------------
;; set some good auto-modes
;; -------------------------------------------------

(setq 
 auto-mode-alist
 (mapcar 
  'purecopy
  '(("\\.emacs$" . emacs-lisp-mode)
    ("\\.lisp$" . lisp-mode)
    ("\\.lsp$" . lisp-mode)
    ("\\.cl$" . lisp-mode)
    ("\\.c$" . c-mode)
    ("\\.cpp$". c++-mode)
    ("\\.cxx$". c++-mode)
    ("\\.cc$". c++-mode)
    ("\\.C$". c++-mode)
    ("\\.h$" . c-mode)
    ("\\.hh$" . c++-mode)
    ("\\.hpp$". c++-mode)
    ("\\.bib$" . bibtex-mode)
    ("\\.tex$" . TeX-mode)
    ("\\.sty$" . TeX-mode)
    ("\\.txi$" . Texinfo-mode)
    ("\\.el$" . emacs-lisp-mode)
    ("\\.pl$" . perl-mode)
    ("\\.dpl$" .prolog-mode) ;; DProlog-Files
    ("\\.pro$" .prolog-mode)
    ("\\.html$" . html-mode)
    ("\\.sgml$" . sgml-mode)
    ("\\.xml$" . xml-mode)
    ("\\.lsp$" . lisp-mode)
    ("\\.lisp$" . lisp-mode)
    ("\\.txt$" . text-mode)
    ("\\.me$" . text-mode)
    ("\\.README$" . text-mode)
    (".*READ\\.ME$" . text-mode)
    ("\\.doc$" . text-mode)
    ("\\.csh$" . csh-mode)
    ("\\.sh$" . sh-mode)
    ("\\.java$" . java-mode)
    ("\\.a$" . c-mode)
    ("\\.nw$" . noweb-mode))))

;(setq dired-guess-shell-alist-user-joana
;      '(("\\_tar\\.gz$" "tar zxvf *&")
;		("\\_tar\\.gz$" "tar ztvf *&")
;		("\\.tgz$" "tar zxvf *&")
;		("\\.tgz$" "tar ztvf *&")
;		("\\.TGZ$" "tar zxvf *&")
;		("\\.TGZ$" "tar ztvf *&")
;		("\\.tar\\.gz$" "tar zxvf *&")
;		("\\.Z$" "gunzip *&")
;		("\\.z$" "gunzip *&")
;		("\\.zip$" "unzip *&")
;		("\\.gz$" "gunzip *&")
;		("\\.GZ$" "gunzip *&")
;		("\\.bz2$" "bunzip2 *&")
;		("\\.BZ2" "bunzip2 *&")
;		("\\.\\(g\\|\\)z" "zcat *&")
;		("\\.lzh$" "lha32 x  *&")
;		("\\.mgp$" "mgp * &")
;		("\\.MGP$" "mgp * &")
;		("\\.gms$" "gams * &")
;		("\\.GMS$" "gams * &")   
;		("\\.dvi$" "fiber * &")
;		("\\.ps$" "fiber *&")
;		("\\.PS$" "fiber *&")
;		("\\.pdf$" "fiber *&")
;		("\\.PDF$" "fiber *&")
;		("\\.tex$" "latexmk -f *&")
;		("\\.m$" "octave * &")
;		("\\.M$" "octave * &")
;		("\\.\\(e\\|\\)ps$" "gsview32 * &")
;		("\\.plt$" "gnuplot *&")
;		("\\.jp[e]?g$" "fiber * &")
;		("\\.JP[E]?G$" "fiber * &")
;		("\\.png$" "fiber * &")
;		("\\.PNG$" "fiber * &")
;		("\\.gif$" "fiber * &")
;		("\\.GIF$" "fiber * &")
;		("\\.bmp$" "fiber * &")
;		("\\.BMP$" "fiber * &")
;		("\\.html$" "fiber *&")
;		("\\.htm$" "fiber *&")
;		("\\.HTML$" "fiber *&")
;		("\\.HTM$" "fiber *&")
;		("\\.xls$" "fiber *&")
;		("\\.XLS$" "fiber *&")
;		("\\.123$" "fiber *&")
;		("\\.lwp$" "fiber *&")
;		("\\.LWP$" "fiber *&")
;		("\\.doc$" "fiber *&")
;		("\\.DOC$" "fiber *&")
;		("\\.mpg$" "fiber *&")
;		("\\.mpg$" "fiber *&")
;		))

(message "AutoMode OK!")

;;============================================================

(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(message "dos2unix e unix2dos")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Customization of specific packages             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shell
(autoload 'sh-mode "sh-script" "Mode for editing sh-scripts" t)
(if (file-exists-p "/usr/bin/zsh")
    (setq explicit-shell-file-name "/usr/bin/zsh"))

(modify-coding-system-alist 'process "" 'undecided-unix)
;;(modify-coding-system-alist 'process "/usr/bin/zsh" 'undecided-unix)
;;(modify-coding-system-alist 'process ".*sh//exe" 'undecided-unix)

(setq shell-command-option "-c")
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t) 
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(setq comint-process-echoes nil)
(setq shell-font-lock-keywords
      '((eval cons shell-prompt-pattern 'font-lock-string-face)
		("[ 	]\\([+-][^ 	\n]+\\)" 1 font-lock-comment-face)
		("^[^ 	\n]+:.*" . font-lock-string-face)
		("^\\[[1-9][0-9]*\\]" . font-lock-string-face)))

(setq 
 compilation-error-regexp-alist
 (list
  ;; works for jikes
  '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:"
	1 2 3)
  ;; works for javac
  ;; thanks to Barrie Treloar <Barrie.Treloar@camtech.com.au>
  '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2)))

(add-hook 'shell-mode-hook
          (lambda ()
            (setq font-lock-defaults nil)
            (font-lock-mode 0)
            (make-local-variable 'comint-preoutput-filter-functions)
            (add-hook 'comint-preoutput-filter-functions 'ansi-color-apply)))

;(setq explicit-shell-file-name "bash.exe")
;(setq shell-file-name "bash.exe")
(setq shell-command-switch "-ic")
(setq shell-command-switch "-c")
(setq my-comint-process-coding-system '(undecided-unix undecided-dos))
(defun my-shell-mode-hook ()
  (setq comint-input-sender (function my-comint-simple-send)))
(defun my-comint-simple-send (proc string)
  (run-hooks 'my-comint-send-hook)
  (comint-send-string proc string)
  (comint-send-string proc "\n"))
(defun my-comint-send-hook ()
  (apply 'set-buffer-process-coding-system my-comint-process-coding-system))
(eval-after-load "shell"
  '(progn
	(add-hook 'shell-mode-hook 'my-shell-mode-hook)
	(add-hook 'my-comint-send-hook 'my-comint-send-hook)))
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; auto-save
(setq auto-save-directory (expand-file-name "~/.autosaves/")
	  auto-save-directory-fallback auto-save-directory
	  auto-save-hash-p nil
	  auto-save-interval 2000)
(require 'auto-save)
(message "auto-saving.... reduncing risks...")

;; arquivos de backup em ~/.backups/
(defun make-backup-file-name (file-name)                     
  "Create the non-numeric backup file name for `file-name'." 
  (require 'dired)
  (if (file-exists-p "~/.backups") 
      (concat (expand-file-name "~/.backups/")
      (dired-replace-in-string "/" "|" file-name))
      (concat file-name "~")))

;; no-web mode
(autoload 'noweb-mode "noweb-mode" "Editing noweb files." t)

;;; **************
;;; Settings for htmlize.el
(load-library "~/.xemacs/htmlize")
(autoload 'htmlize-buffer "htmlize" "Autocreate HTML" t)
(autoload 'htmlize-region "htmlize" "Autocreate HTML" t)
(autoload 'htmlize-file "htmlize" "Autocreate HTML" t)
(autoload 'htmlize-many-files "htmlize" "Autocreate HTML" t)
(autoload 'htmlize-many-files-dired "htmlize" "Autocreate HTML" t)

(message "htmlize: you may now create your html files!!! :)")

;; ------------------------------------------------------------
;; - - - - - - Para arrancar com o Lisp - - - - - - - - - - - - 
;; ------------------------------------------------------------
;; CLISP to run
(autoload 'clisp-hs    "ilisp" "Inferior Haible/Stoll CLISP Common Lisp." t)
(autoload 'clisp       "ilisp" "Inferior Haible/Stoll CLISP Common Lisp." t)

(defun lisp ()       (clisp-hs))
(defun start-lisp () (clisp-hs))

(setq ilisp-*use-fsf-compliant-keybindings* t
	  ilisp-*arglist-message-lisp-space-p* t
	  ilisp-print-info-message-command t
	  lisp-no-popper t)

(setq inferior-lisp-program "/usr/bin/clisp -I")

;; Gnu CLISP - ILISP (switches for ANSI & no banner)
(defvar clisp-dir "/afs/l2f/usr/local/lib/lisp/")
(when (equal (system-name) "vincent.local")
  (setq clisp-dir "/usr/lib/clisp/full/"))
(defvar clisp-exe (concat clisp-dir "lisp.run"))
(defvar clisp-hs-program 
        (concat clisp-exe 
				" -B " clisp-dir 
				" -M " clisp-dir "lispinit.mem " 
				"-ansi -q")) ;; -q tira a apresentacao

(message "clisp set to go")

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t nil)))

(defun generic-eval-last-sexp (arg)
  "Evaluate last s-expression in either elisp, ilisp, acl, or corman."
  (interactive "p")
  (cond
   ((or (equal mode-name "Emacs-Lisp")
      	(equal mode-name "Emacs Lisp")
	      (equal (current-buffer) (get-buffer "*scratch*")))
    (eval-last-sexp nil))
   ((or (eq lisp-used :clisp-ilisp)
	      (eq lisp-used :lw-ilisp))
    (save-excursion
      (backward-char 1)
      (if (looking-at "\\s\)") 
          (progn
            (forward-char 1) 
            (backward-list 1)
            (eval-next-sexp-lisp)))))
   ((eq lisp-used :acl-eli)
    (fi:lisp-eval-last-sexp))
   ((eq lisp-used :corman-inf)
    (lisp-eval-last-sexp))))

(defvar lisp-implementations '() "Lisp installed!")

(if (file-exists-p clisp-exe) 
   (setq lisp-implementations 
         (cons :clisp-ilisp lisp-implementations)))

(defvar lisp-used :clisp-ilisp "Last Lisp implementation used")

(setq ilisp-prefix "\C-z")
(setq lisp-mode-hook '(lambda () (require 'ilisp)))

(message "lisp mode hooks hooked...")
(message "some more bindings...")
(message "Lisp ready to go...")
(message "gentlemens, start your engines...")

; Automatically save place in files, so that visiting them later
; (even during a different Emacs session) automatically moves point
; to the saved position, when the file is first found.  Uses the
; value of buffer-local variable save-place to determine whether to
; save position or not.
;
; Thanks to Stefan Schoef, who sent a patch with the
; `save-place-version-control' stuff in it.
;
(require 'saveplace)
(setq-default save-place t)
(setq save-place-limit 30)

(set-frame-size *frame-da-joana* *width* *height*)

;; directoria correcta
(cd "/afs/l2f/home/joana")
;; (term "/usr/bin/zsh")
(message "Your emacs is all set and ready to go... have fun! :)")

