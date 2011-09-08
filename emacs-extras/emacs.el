;; -*- coding: utf-8; -*-
;;;; .emacs - Emacs Utilities

(load-file "~/dotemacs/emacs-extras/common.el")


;;; ---------------------------------------------------------------------
;;; Desktop-Menu
;;;
(defun desktop ()
  (interactive)
  (load-file (expand-file-name (concat emacs-extras-d "/desktop-menu.el"))))


;;; ---------------------------------------------------------------------
;;; Org
;;;
(unless (string-equal host "SETUBAL") ; SISCOG
  (load-file (expand-file-name (concat emacs-extras-d "/sc-org/sc-org.el"))))


;;; ---------------------------------------------------------------------
;;; YaSnippet
;;;
(unless (string-equal host "SETUBAL") ; SISCOG
  (add-to-list 'load-path (concat emacs-extras-d "/yasnippet-0.6.1c"))
  (eval-after-load 'yasnippet
    ;; Initialize Yasnippet
    ;; Don't map TAB to yasnippet
    ;; In fact, set it to something we'll never use because
    ;; we'll only ever trigger it indirectly.
    '(progn 
       (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
       (yas/initialize)
       (yas/load-directory (concat emacs-extras-d "/yasnippet-0.6.1c/snippets")))))


;;; ---------------------------------------------------------------------
;;; SLIME 
;;;
;; (unless (string-equal host "SETUBAL") ; SISCOG
;;   (load-file (expand-file-name (concat emacs-extras-d "/slime.el"))))


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

(defun trim-str (str)
  "Trims leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))


;;; ---------------------------------------------------------------------
;;; Auto Complete
;;;
(defun auto-complete ()
  (interactive)
  (load-file (expand-file-name (concat emacs-extras-d "/auto-complete.el")))
  (global-auto-complete-mode t)
  (setq ac-auto-start 3)
  (setq ac-dwim t)
  (define-key ac-complete-mode-map "\t" 'ac-complete)
  (define-key ac-complete-mode-map "\r" nil)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous))


;;; ---------------------------------------------------------------------
;;; Pabbrev
;;;
(defun pabbrev ()
  (interactive)
  (load-file (expand-file-name (concat emacs-extras-d "/pabbrev.el"))))


(provide 'bjacquet-init)
;; .emacs EOF
