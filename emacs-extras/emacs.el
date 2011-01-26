;; -*- coding: utf-8; -*-
;;;; .emacs - Emacs Utilities
(setq debug-on-error t)


(defvar host
  (cond
   ((getenv "COMPUTERNAME"))
   ((getenv "HOSTNAME"))))


(defvar user
  (cond
   ((getenv "USER"))
   ((getenv "USERNAME"))))


(defvar emacs-d
  (if (string-equal host "SETUBAL")  ; SISCOG 
      "~/emacs"
    "~/flash_drive/emacs"))


(defvar emacs-extras-d
  (concat emacs-d "/emacs-extras"))


(defvar notes-d
  (if (string-equal host "SETUBAL")  ; SISCOG 
      "~/Documents/notes"
    "~/flash_drive/notes"))


(defvar diary-d
  (if (string-equal host "SETUBAL")  ; SISCOG 
      "~/Documents/diary"
    "~/flash_drive/diary"))


(message (concat "host " host "  user " user))


;;; ---------------------------------------------------------------------
;;; Look & Feel
;;;
(add-to-list 'load-path (concat emacs-extras-d "/color-theme-6.6.0"))
(require 'color-theme)
(eval-after-load "color-theme"
  '(color-theme-initialize))

(load-file (expand-file-name (concat emacs-extras-d "/color-theme-random.el")))
(require 'color-theme-random)
(color-theme-random)

(tool-bar-mode nil)

(set-default-font "DejaVu Sans Mono-11")

(setq visible-bell t)

(setq inhibit-startup-message t)

(line-number-mode t)
(column-number-mode t)

(setq scroll-step 1) ; scroll one line past the edge of the screen

(global-font-lock-mode t) ; sintax highlight
(transient-mark-mode t)   ; highlighted when the mark is active
(show-paren-mode t)       ; highlight matching parenthesis

(setq nex-line-add-newlines nil)
(setq require-final-newline t)
(setq next-line-extends-end-of-buffer nil)

(setq search-highlith t)
(setq query-replace-highlight t)

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq abbrev-mode t)

(iswitchb-mode t) ;intelligent buffer switcher (in minibuffer)

(load-file (expand-file-name (concat emacs-extras-d "/sql-indent.el")))
(eval-after-load "sql"
  '(require 'sql-indent))

;; Emacs title bar to reflect file name
(defun title-set-title ()
  "Set title to current`s buffer \[buffer-file-name] name
or to \[buffer-name if it has no file"
  (let ((name (format "%s %s@%s"
                      (cond
                       ((string-match
                         (getenv "HOME")
                         (or (buffer-file-name (current-buffer))
                             (buffer-name)))
                        (concat "~"
                                (substring
                                 (buffer-file-name
                                  (current-buffer))
                                 (match-end 0))))
                       (t (or
                           (buffer-file-name (current-buffer))
                           (buffer-name))))
		      user
		      host)))
    (modify-frame-parameters (selected-frame)
                             (list (cons `name name)))))
(add-hook `post-command-hook
	  (lambda ()
	    (title-set-title)))


(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(debug-on-error t)
 '(default-input-method "portuguese-prefix")
 '(desktop-menu-directory "~/Documents/diary/desktop/")
 '(global-font-lock-mode t nil (font-lock))
 '(org-agenda-files (quote ("y:/Documents/diary/remember.org" "y:/Documents/diary/TSS.org" "y:/Documents/diary/siscog.org")))
 '(pc-select-meta-moves-sexps t)
 '(pc-selection-mode t nil (pc-select))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(user-mail-address "bruno.jacquet@gmail.com")
 '(user-full-name "Bruno Jacquet")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )


;;; ---------------------------------------------------------------------
;;; Bookmark-Extensions
;;;
(load-file (expand-file-name (concat emacs-extras-d "/emacs-bookmark-extension/bookmark-extensions.el")))
(require 'bookmark-extensions)


;;; ---------------------------------------------------------------------
;;; Desktop-Menu
;;;
(load-file (expand-file-name (concat emacs-extras-d "/desktop-menu.el")))
(require 'desktop-menu)


;;; ---------------------------------------------------------------------
;;; Org & Remember
;;;
(unless (string-equal host "SETUBAL") ; SISCOG
  (load-file (expand-file-name (concat emacs-extras-d "/sc-org/sc-org.el"))))


;;; ---------------------------------------------------------------------
;;; YaSnippet
;;;
(unless (string-equal host "SETUBAL") ; SISCOG
  (add-to-list 'load-path (concat emacs-extras-d "/yasnippet-0.6.1c"))
  (require 'yasnippet)
  ;; Initialize Yasnippet
  ;; Don't map TAB to yasnippet
  ;; In fact, set it to something we'll never use because
  ;; we'll only ever trigger it indirectly.
  (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
  (yas/initialize)
  (yas/load-directory (concat emacs-extras-d "/yasnippet-0.6.1c/snippets")))


;;; ---------------------------------------------------------------------
;;; reStruturedText
;;;
(unless (string-equal host "SETUBAL") ; SISCOG
  (load-file (expand-file-name (concat emacs-extras-d "/rst.el")))
  (require 'rst))


;;; ---------------------------------------------------------------------
;;; Winpoint
;;;
(load-file (expand-file-name (concat emacs-extras-d "/winpoint.el")))
(require 'winpoint)
(winpoint-mode t)


;;; ---------------------------------------------------------------------
;;; Auto Complete
;;;
(load-file (expand-file-name (concat emacs-extras-d "/auto-complete.el")))
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-auto-start 3)
(setq ac-dwim t)


;;; ---------------------------------------------------------------------
;;; W3M
;;;
(unless (string-equal host "SETUBAL") ; SISCOG
  (load-file (expand-file-name (concat emacs-extras-d "/w3m.el"))))

;;; ---------------------------------------------------------------------
;;; GNUS
;;;
(unless (string-equal host "SETUBAL") ; SISCOG
  (load-file (expand-file-name (concat emacs-extras-d "/gnus.el"))))


;;; ---------------------------------------------------------------------
;;; Stuff
;;;
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))

(defun espacos () (interactive)
  "Eliminate whitespace at enfs of all lines in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t][ \t]*$" nil t)
      (delete-region (match-beginning 0) (point)))))

(defun abre-emacs-el ()
  (interactive)
  (find-file (expand-file-name (concat emacs-extras-d "/emacs.el"))))

(defun abre-notes-org ()
  (interactive)
  (find-file (expand-file-name "~/flash_drive/notes/note.org"))
  (find-file (expand-file-name "~/flash_drive/notes/london.org"))
  (find-file (expand-file-name "~/flash_drive/notes/remember.org")))

(lexical-let ((idx 0)
	      (options (list 'flyspell-mode ; turns on
			     'flyspell-prog-mode
			     'flyspell-buffer
			     'flyspell-mode ; turns off
			     )))
  (setf (cdr (last options)) options)
  (defun flyspell-mode-cycle ()
    (interactive)
    (let ((option (elt options idx)))
      (setf idx (1+ idx))
      (message "%s" option)
      (funcall option))))

(setq
 auto-mode-alist
 (mapcar
  'purecopy
  '(("\\.emacs$" . emacs-lisp-mode)
    ("\\.lisp$" . lisp-mode)
    ("\\.lsp$" . lisp-mode)
    ("\\.cl$" . lisp-mode)
    ("\\.dic$" . lisp-mode)
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
    ("\\.xml$" . xml-mode)
    ("\\.txt$" . text-mode)
    ("\\.me$" . text-mode)
    ("\\.README$" . text-mode)
    (".*READ\\.ME$" . text-mode)
    ("\\.doc$" . text-mode)
    ("\\.csh$" . csh-mode)
    ("\\.sh$" . sh-mode)
    ("\\.java$" . java-mode)
    ("\\.a$" . c-mode)
    ("\\.nw$" . noweb-mode)
    ("Makefile" . makefile-mode)
    ("\\.css$" . css-mode)
    ("\\.php$" . php-mode)
    ("\\.phtml$" . php-mode)
    ("\\.py$" . python-mode)
    ("\\.org\\'" . org-mode)
    (".bashrc$" . shell-script-mode)
    ("\\.sql$" . sql-mode)
    ("\\.html$" . html-mode)
    ("\\.rst" . rst-mode)
    ("\\.rest" . rst-mode))))


;;; ---------------------------------------------------------------------
;;; Keys
;;;
;; (global-set-key [delete]    'delete-char)
;; (global-set-key [kp-delete] 'delete-char)
;; (global-set-key [backspace] 'backward-delete-char)
(global-set-key [home]      'beginning-of-line)
(global-set-key [end]       'end-of-line)

(global-set-key [f5]    'comment-region)
(global-set-key [S-f5]  'uncomment-region)
(global-set-key [f7]    'flyspell-mode-cycle)
(global-set-key [f8]    'find-file-at-point)
(global-set-key "\C-cl" 'goto-line)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-xO" 'previous-multiframe-window)
(global-set-key "\C-cr" 'remember)

;; Pager
(load-file (expand-file-name (concat emacs-extras-d "/pager.el")))
(require 'pager)
(global-set-key "\C-v" 	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\M-v"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)


;; Auto-Complete
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)


;; Pabbrev
;; (load-file (expand-file-name (concat emacs-extras-d "/pabbrev.el")))
;; (require 'pabbrev)

(provide 'bjacquet-init)
;; .emacs EOF
