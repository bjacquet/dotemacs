;; -*- coding: utf-8; -*-
;;;; dotemacs.el - Emacs Utilities

(setq debug-on-error t)


(defvar host
  (cond
   ((getenv "COMPUTERNAME"))
   ((getenv "HOSTNAME"))
   ((system-name))))


(defvar user
  (cond
   ((getenv "USER"))
   ((getenv "USERNAME"))))


(setq at-siscog-p (string-equal host "SETUBAL"))


(defvar emacs-d
  (if at-siscog-p
      "~/emacs"
    "~/dotemacs"))


(defvar emacs-extras-d
  (concat emacs-d "/emacs-extras"))


(defvar notes-d
  (if at-siscog-p
      "~/Documents/notes"
    "~/flash_drive/notes"))


(defvar diary-d
  (if at-siscog-p
      "~/Documents/diary"
    "~/flash_drive/diary"))


(message (concat "host " host "  user " user))


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


;;; ---------------------------------------------------------------------
;;; Look & Feel
;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(debug-on-error t)
 '(default-input-method "portuguese-prefix")
 '(global-font-lock-mode t nil (font-lock))
 '(pc-select-meta-moves-sexps t)
 '(pc-selection-mode t nil (pc-select))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(user-mail-address "bruno.jacquet@gmail.com")
 '(user-full-name "Bruno Jacquet")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(tool-bar-mode nil)
 '(set-default-font "DejaVu Sans Mono-11")
 '(visible-bell t)
 '(inhibit-startup-message t)
 '(line-number-mode t)
 '(scroll-step 1)           ; scroll one line past the edge of the screen
 '(global-font-lock-mode t) ; sintax highlight
 '(transient-mark-mode t)   ; highlighted when the mark is active
 '(show-paren-mode t)       ; highlight matching parenthesis
 '(next-line-add-newlines nil)
 '(require-final-newline t)
 '(next-line-extends-end-of-buffer nil)
 '(auto-save-default nil)
 '(make-backup-files nil)
 '(iswitchb-mode t)         ; intelligent buffer switcher (in minibuffer)
 '(setq kmacro-call-mouse-event nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )


;;; ---------------------------------------------------------------------
;;; Color Theme & Color Theme Random
;;;

(add-to-list 'load-path (concat emacs-extras-d "/color-theme-6.6.0"))
(autoload 'color-theme (expand-file-name (concat emacs-extras-d "/color-theme-6.6.0")))
(eval-after-load "color-theme"
  '(color-theme-initialize))
(load-file (expand-file-name (concat emacs-extras-d "/color-theme-random.el")))
(autoload 'color-theme-random (expand-file-name (concat emacs-extras-d "/color-theme-random.el")))
(color-theme-random)

(setq
 auto-mode-alist
 (mapcar
  'purecopy
  '(("\\.emacs$"    . emacs-lisp-mode)
    ("\\.el$"       . emacs-lisp-mode)
    ("\\.lisp$"     . lisp-mode)
    ("\\.lsp$"      . lisp-mode)
    ("\\.cl$"       . lisp-mode)
    ("\\.dic$"      . lisp-mode)
    ("\\.a$"        . c-mode)
    ("\\.c$"        . c-mode)
    ("\\.h$"        . c-mode)
    ("\\.cpp$"      . c++-mode)
    ("\\.cxx$"      . c++-mode)
    ("\\.cc$"       . c++-mode)
    ("\\.C$"        . c++-mode)
    ("\\.hh$"       . c++-mode)
    ("\\.hpp$"      . c++-mode)
    ("\\.bib$"      . bibtex-mode)
    ("\\.tex$"      . TeX-mode)
    ("\\.sty$"      . TeX-mode)
    ("\\.txi$"      . Texinfo-mode)
    ("\\.xml$"      . xml-mode)
    ("\\.txt$"      . text-mode)
    ("\\.me$"       . text-mode)
    ("\\.README$"   . text-mode)
    (".*READ\\.ME$" . text-mode)
    ("\\.doc$"      . text-mode)
    (".bashrc$"     . shell-script-mode)
    ("\\.csh$"      . csh-mode)
    ("\\.sh$"       . sh-mode)
    ("\\.java$"     . java-mode)
    ("\\.nw$"       . noweb-mode)
    ("Makefile"     . makefile-mode)
    ("\\.php$"      . php-mode)
    ("\\.phtml$"    . php-mode)
    ("\\.py$"       . python-mode)
    ("\\.org\\'"    . org-mode)
    ("\\.sql$"      . sql-mode)
    ("\\.html$"     . html-mode)
    ("\\.css$"      . css-mode)
    ("\\.rst"       . rst-mode)
    ("\\.rest"      . rst-mode))))


;;; ---------------------------------------------------------------------
;;; Winpoint
;;;
(load-file (expand-file-name (concat emacs-extras-d "/winpoint.el")))
(winpoint-mode t)


;;; ---------------------------------------------------------------------
;;; Undo Tree
;;;
(load-file (expand-file-name (concat emacs-extras-d "/undo-tree.el")))
(setq undo-tree-mode-lighter nil)
(global-undo-tree-mode)


;;; ---------------------------------------------------------------------
;;; Flyspell
;;;
(setq flyspell-mode-line-string nil)
(lexical-let ((idx 0)
	      (options (list 'flyspell-mode ; turns on
			     'flyspell-prog-mode
			     'flyspell-mode ; turns off
			     )))
  (setf (cdr (last options)) options)
  (defun flyspell-mode-cycle ()
    (interactive)
    (let ((option (elt options idx)))
      (setf idx (1+ idx))
      (message "%s" option)
      (funcall option))))
(global-set-key [f7]    'flyspell-mode-cycle)
(global-set-key [S-f7]  'flyspell-buffer)


;;; ---------------------------------------------------------------------
;;; Magit
;;;
(add-to-list 'load-path (expand-file-name (concat emacs-extras-d "/magit-0.8.2")))


;;; ---------------------------------------------------------------------
;;; Column Marker
;;;
(load-file (expand-file-name (concat emacs-extras-d "/column-marker.el")))
(require 'column-marker)
(defface column-marker-bj '((t (:background "red" :foreground "white")))
  "Face used for a column marker.  Usually a background color."
  :group 'faces)
(defvar column-marker-bj-face 'column-marker-bj
    "Face used for a column marker.  Usually a background color.
Changing this directly affects only new markers.")
(column-marker-create column-marker-bj column-marker-bj-face)
(add-hook 'emacs-lisp-mode-hook (lambda () (interactive) (column-marker-bj 80)))
(add-hook 'lisp-mode-hook (lambda () (interactive) (column-marker-bj 80)))


;;; ---------------------------------------------------------------------
;;; Keys
;;;
(global-set-key [home]  'beginning-of-line)
(global-set-key [end]   'end-of-line)
(global-set-key [f5]    'comment-region)
(global-set-key [S-f5]  'uncomment-region)
(global-set-key [f8]    'find-file-at-point)
(global-set-key "\C-cl" 'goto-line)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-xO" 'previous-multiframe-window)


;;; ---------------------------------------------------------------------
;;; Pager
;;;
(load-file (expand-file-name (concat emacs-extras-d "/pager.el")))
(global-set-key "\C-v" 	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\M-v"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)


;;; ---------------------------------------------------------------------
;;; Auto Complete
;;;
(eval-after-load 'auto-complete
  '(progn
     (global-auto-complete-mode t)
     (setq ac-auto-start 3)
     (setq ac-dwim t)
     (define-key ac-complete-mode-map "\t" 'ac-complete)
     (define-key ac-complete-mode-map "\r" nil)
     (define-key ac-complete-mode-map "\C-n" 'ac-next)
     (define-key ac-complete-mode-map "\C-p" 'ac-previous)))


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
  (find-file (locate-library "dotemacs.el")))

(defun abre-notes-org ()
  (interactive)
  (find-file (expand-file-name "~/flash_drive/notes/note.org"))
  (find-file (expand-file-name "~/flash_drive/notes/london.org"))
  (find-file (expand-file-name "~/flash_drive/notes/remember.org")))

(defun trim-str (str)
  "Trims leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))


;;; ---------------------------------------------------------------------
;;; Modes loaded on request
;;;
(add-to-list 'load-path (concat emacs-extras-d "/load-path"))


(provide 'dotemacs)

;; dotemacs.el EOF
