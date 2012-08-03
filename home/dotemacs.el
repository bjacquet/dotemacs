;; -*- coding: utf-8; -*-
;;;; dotemacs.el - Emacs Utilities

(setq debug-on-error t)

(defvar packages-directory "~/dotemacs/packages/"
  "The packages directory.")

(defvar configuration-directory "~/dotemacs/home/"
  "The directory of this configuration.")

(defmacro concat-package-dir (package-name)
  "(concat packages-directory package-name)"
  (list 'concat packages-directory package-name))

(defmacro expand-package (package-name)
  "(expand-file-name (concat-package-dir package-name))"
  (list 'expand-file-name (list 'concat-package-dir package-name)))

(defmacro load-configuration (config-file-name)
  `(load-file (expand-file-name (concat configuration-directory ,config-file-name))))

(add-to-list 'load-path packages-directory)

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
;;; Package's Configurations
;;;
(load-configuration "color-theme.el")
(load-configuration "windpoint.el")
(load-configuration "undo-tree.el")
(load-configuration "flyspell.el")
(load-configuration "magit.el")
(load-configuration "last-closed-files.el")
(load-configuration "auto-complete.el")


;;; ---------------------------------------------------------------------
;;; Keys
;;;
(global-set-key [home]  'beginning-of-line)
(global-set-key [end]   'end-of-line)
(global-set-key [f5]    'comment-region)
(global-set-key [S-f5]  'uncomment-region)
(global-set-key [f7]    'flyspell-mode-cycle)
(global-set-key [S-f7]  'flyspell-buffer)
(global-set-key [f8]    'find-file-at-point)
(global-set-key [f9]    'last-closed-files)
(global-set-key "\C-cl" 'goto-line)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-xO" 'previous-multiframe-window)


;; dotemacs.el EOF
