;; -*- coding: utf-8; -*-
;;;; dotemacs.el - Emacs Utilities

(setq debug-on-error t)

(defvar packages-d "~/dotemacs/packages/"
  "The packages directory.")

(defmacro concat-package-dir (package-name)
  "(concat packages-d package-name)"
  (list 'concat packages-d package-name))

(defmacro expand-package (package-name)
  "(expand-file-name (concat-package-dir package-name))"
  (list 'expand-file-name (list 'concat-package-dir package-name)))


(defun title-set-title ()
  "Set title to current`s buffer \[buffer-file-name] name
or to \[buffer-name if it has no file"
  (let ((name (format "%s"
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
                           (buffer-name)))))))
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
 ;'(iswitchb-mode t)         ; intelligent buffer switcher (in minibuffer)
 '(ido-enable-flex-matching t)
 '(ide-everywhere t)
 '(ido-mode t)
 '(setq kmacro-call-mouse-event nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )


;;; ---------------------------------------------------------------------
;;; Color Theme & Color Theme Random
;;;
(add-to-list 'load-path (concat-package-dir "color-theme"))
(autoload 'color-theme (expand-package "color-theme"))
(eval-after-load "color-theme"
  '(color-theme-initialize))
(load-file (expand-package "color-theme-random.el"))
(autoload 'color-theme-random (expand-package "color-theme-random.el"))
(color-theme-random)


;;; ---------------------------------------------------------------------
;;; Winpoint
;;;
(load-file (expand-package "winpoint.el"))
(winpoint-mode t)


;;; ---------------------------------------------------------------------
;;; Undo Tree
;;;
(load-file (expand-package "undo-tree.el"))
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


;;; ---------------------------------------------------------------------
;;; Magit
;;;
(add-to-list 'load-path (expand-package "magit"))
(autoload 'magit-status "magit" "Loads magit-mode" t)


;;; ---------------------------------------------------------------------
;;; Last Closed Files
;;;
(load-file (expand-package "last-closed-files.el"))
(global-set-key [f9] 'last-closed-files)


;; dotemacs.el EOF
