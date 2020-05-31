;;; ---------------------------------------------------------------------
;;; Dashboard
;;;
(defun bj:random-dashboard-startup-banner ()
  "Selects a random banner for dashboard."
  (bj:random-elem
   (append (list 'official 'logo 1 2 3)
           (mapcar #'(lambda (file)
                        (format "%scustom/%s" emacs-dir file))
                   (list "glider.xpm"
                         "lisplogo-alien.xpm"
                         "lisplogo-flag.xpm"
                         "rtrv-symbol.xpm"
                         "ruby.xpm"
                         "sourcerer.xpm"
                         "splash.xpm"
                         "t-rex.xpm")))))

(defun bj:random-elem (list)
  (nth (random (length list)) list))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (add-to-list 'dashboard-items '(agenda) t)
  (setq show-week-agenda-p t)
  (setq dashboard-items '((agenda . 10)
                          (projects . 5)
                          (recents  . 5)
                          (bookmarks . 5)
                          (registers . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner (bj:random-dashboard-startup-banner)))

(defun bj:open-dashboard ()
   "Open the *dashboard* buffer and jump to the first widget."
  (interactive)
  (delete-other-windows)
  ;; Refresh dashboard buffer
  (if (get-buffer dashboard-buffer-name)
      (kill-buffer dashboard-buffer-name))
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name)
  ;; Jump to the first section
  (goto-char (point-min))
  (bj:dashboard-goto-agenda))

(defun bj:dashboard-goto-agenda ()
  "Go to agenda."
  (interactive)
  (funcall (local-key-binding "a")))
