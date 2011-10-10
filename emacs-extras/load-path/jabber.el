;;; ---------------------------------------------------------------------
;;; Emacs Jabber
;;; Windows setup only
(add-to-list 'load-path "~/emacs/emacs-extras/emacs-jabber/")
(require 'jabber-autoloads)

(setenv "PATH" (format "c:\\cygwin\\bin;c:\\cygwin\\usr\\bin;%s"
		       (getenv "PATH")))

(eval-after-load 'jabber
  `(progn
     ;; Faces
     (set-face-foreground 'jabber-chat-prompt-local "OrangeRed4")
     (set-face-foreground 'jabber-chat-text-local "OrangeRed4")
     (set-face-foreground 'jabber-chat-prompt-foreign "OrangeRed3")
     (set-face-foreground 'jabber-chat-text-foreign "OrangeRed3")
     (set-face-foreground 'jabber-roster-user-online "LimeGreen")
     (set-face-foreground 'jabber-roster-user-away "YellowGreen")
     (set-face-foreground 'jabber-roster-user-dnd "IndianRed")
     (set-face-foreground 'jabber-activity-face "yellow4")
     (set-face-foreground 'jabber-activity-personal-face "yellow4")
     (set-face-attribute 'jabber-title-medium nil
                         :width 'unspecified :height 'unspecified)
     ;; Roster Options
     (setq jabber-vcard-avatars-retrieve nil)
     (setq jabber-roster-show-title nil)
     (setq jabber-roster-show-bindings nil)
     (setq jabber-show-offline-contacts nil)
     (setq jabber-show-resources nil)
     (setq jabber-sort-order nil)
     ;; Chat Options
     (add-hook 'jabber-roster-mode-hook (lambda () (setq truncate-lines t)))
     (setq jabber-chat-local-prompt-format "[%t] Jacquet> ")
     (add-hook 'jabber-chat-mode-hook
               (lambda ()
                 (visual-line-mode t)
                 (set-input-method 'portuguese-prefix)))
     ;; Misc Options
     (setq jabber-default-status "SISCOG")
     (setq jabber-default-show "dnd")
     (setq jabber-alert-presence-hooks nil)
     (setq jabber-alert-message-hooks '(jabber-message-scroll))))

(defun gtalk ()
  (interactive)
  (let ((jabber-account-list
         '(("bruno.jacquet@gmail.com"
            (:password . nil)
            (:network-server . "talk.google.com")
            (:port . 443)
            (:connection-type . ssl)))))
    (jabber-connect-all)))


(defun gtalk-groupchat ()
  (interactive)
  (let ((group (apply 'format "private-chat-%x%x%x%x%x%x%x%x-%x%x%x%x-%x%x%x%x-%x%x%x%x-%x%x%x%x%x%x%x%x%x%x%x%x@groupchat.google.com"
                      (mapcar (lambda (x) (random x)) (make-list 32 15))))
        (account (jabber-read-account)))
    (jabber-groupchat-join account group (jabber-muc-read-my-nickname account group) t)))