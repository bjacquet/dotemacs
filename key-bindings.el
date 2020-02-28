;; -*- coding: utf-8; -*-
;;; key-bindings.el - Key bindings

(global-set-key [home]  'beginning-of-line)
(global-set-key [end]   'end-of-line)
(global-set-key [f5]    'comment-region)
(global-set-key [S-f5]  'uncomment-region)
(global-set-key [f6]    'bj:open-dashboard)
(global-set-key [f8]    'find-file-at-point)
(global-set-key [f9]    'last-closed-files)
(global-set-key [S-f9]  'recentf-open-files)
(global-set-key "\C-cl" 'goto-line)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-xO" 'previous-multiframe-window)
(global-set-key "\C-x2" 'bj:split-window-vertically)
(global-set-key "\C-x3" 'bj:split-window-horizontally)
(global-set-key "\C-c|" 'bj:toggle-window-split)
(global-set-key "\M-c"  'capitalize-dwim)
(global-set-key "\M-l"  'downcase-dwim)
(global-set-key "\M-u"  'upcase-dwim)

;;; key-bindings.el ends here
