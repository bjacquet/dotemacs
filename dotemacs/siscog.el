;; -*- coding: utf-8; -*-
;;; siscog.el - SISCOG configuration


(defvar emacs-dir
  "~/.emacs.d/")


;;; ---------------------------------------------------------------------
;;; Load all things SISCOG
;;;
(load-file (expand-file-name (concat emacs-dir "pkg-config.el")))
(bj:load-pkg-loader "sc-emacs.el")
(bj:load-pkg-loader "sc-slime.el")
(bj:load-pkg-loader "sc-sly.el")


(defun random-elem (list)
  (nth (random (length list)) list))

(setq fancy-splash-image (format "%s/custom/%s"
                                 (getenv "SISCOG_EMACS_DIR_LOCAL")
                                 (random-elem (list "lisplogo-alien.xpm"
                                                    "siscog-symbol.xpm"
                                                    "lisplogo-flag.xpm"
                                                    "glider.xpm"
                                                    "splash.xpm"))))


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
 ;; '(pc-selection-mode t nil (pc-select)) ; marked as obsolete
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(user-mail-address "bjacquet@siscog.pt")
 '(user-full-name "B Jacquet")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(visible-bell t)
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
 ;; '(ido-enable-flex-matching t)
 ;; '(ido-everywhere t)
 ;; '(ido-mode t)
 '(ivy-mode 1)
 '(setq kmacro-call-mouse-event nil)
 '(recentf-mode t)
 '(recentf-max-saved-items 100)
 '(isearch-allow-scroll t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(indent-tabs-mode nil)
 '(tab-always-indent 'complete)
 '(blink-cursor-blinks 0)
 '(dired-dnd-protocol-alist nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

(set-default-font "Consolas-12")

;; Slime messes up my frame title.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concatenate 'string
                                (abbreviate-file-name (buffer-file-name))
                                (if (and (boundp 'slime-mode) slime-mode (slime-current-connection))
                                    (concatenate 'string
                                                 " ["
                                                 (slime-current-package)
                                                 ": "
                                                 (slime-connection-name)
                                                 "]")))
                   "%b"))))


;;; ---------------------------------------------------------------------
;;; Package's Configurations
;;;
(bj:load-pkg-loader "auto-complete.el")
(bj:load-pkg-loader "avy.el")
(bj:load-pkg-loader "bm.el")
(bj:load-pkg-loader "color-theme.el")
(bj:load-pkg-loader "expand-region.el")
(bj:load-pkg-loader "flyspell.el")
;; (bj:load-pkg-loader "ido-vertical.el")
(bj:load-pkg-loader "htmlize.el")
(bj:load-pkg-loader "magit.el")
(bj:load-pkg-loader "multiple-cursors.el")
(bj:load-pkg-loader "pager.el")
(bj:load-pkg-loader "paredit.el")
(bj:load-pkg-loader "scratch.el")
(bj:load-pkg-loader "shrink-whitespace.el")
(bj:load-pkg-loader "smart-mode-line.el")
(bj:load-pkg-loader "undo-tree.el")
;;(bj:load-pkg-loader "whitespace.el")
(bj:load-pkg-loader "winpoint.el")
(bj:load-pkg-loader "wn.el")
(bj:load-pkg-loader "yasnippet.el")
(bj:load-pkg-loader "hunspell.el")
(load "last-closed-files")
(load-file (expand-file-name (concat emacs-dir "defuns.el")))
(load-file (expand-file-name (concat emacs-dir "key-bindings.el")))


;;; ---------------------------------------------------------------------
;;; Enable disabled commands
;;;
(put 'narrow-to-page 'disabled nil)


;;; siscog.el ends here
