;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2010, SISCOG - Sistemas Cognitivos, SA
;;;                           All rights reserved
;;;
;;;-----------------------------------------------------------------------------
;;;
;;;                         RESTRICTED RIGHTS LEGEND
;;;
;;;-----------------------------------------------------------------------------
;;;
;;;     Use, duplication or disclosure is subject to authorization by
;;;
;;;                 SISCOG - Sistemas Cognitivos, SA
;;;                      Campo Grande 378, 3º
;;;                        1700-097 LISBOA
;;;                           PORTUGAL
;;;
;;;-----------------------------------------------------------------------------
;;; Description
;;;	
;;; History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created from ELI definitions in sc-allegro.el.
;;;					Moved indentation settings from sc-global-param.el.
;;;					Added to `fi:common-lisp-indent-hook':
;;;					  'do.alist
;;;					  'dolist.dbind
;;;					  'with.gensyms
;;;					Documented definitions
;;;					  LISP-PROCESS
;;;					  REPL-NEWLINE
;;;					  RUN-COMMON-LISP
;;;					  GOTO-ALLEGRO-LISP-BUFFER
;;;					  RUN-ALLEGRO-LISP-IMAGE
;;;					Added definitions
;;;					  ADD-NEXT-PREVIOUS-TO-LISTENER-MODE
;;;					  LEP::FIND-FILE - advice
;;;					  FI::SHOW-FOUND-DEFINITION - advice
;;;					  LISP-CONNECTION-CLEANUP
;;;-----------------------------------------------------------------------------

(load "fi-site-init.el")

;; Patches to correct problems with previous versions of Allegro and Emacs
(sc-load "fi-sc-patch")
(sc-load "fi-sc-manual")


;;; Indentation settings
(put 'catching-errors-in-string 'fi:common-lisp-indent-hook '(like dolist))
(put 'check.add.button.end 'fi:common-lisp-indent-hook 2)
(put 'def.class 'fi:common-lisp-indent-hook '((1 3 lambda-list) (0 t 3)))
(put 'do-in-list 'fi:common-lisp-indent-hook 1)
(put 'do.alist  'fi:common-lisp-indent-hook '(like dolist))
(put 'do.each.ycode 'fi:common-lisp-indent-hook 1)
(put 'do.freq.basic.decomposition 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.freq.dates 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.freq.decomposition 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.freq.object.plist 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.gen.tasks 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.in.all.stations.from.route 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.in.route 'fi:common-lisp-indent-hook 1)
(put 'do.list 'fi:common-lisp-indent-hook 1)
(put 'do.movements 'fi:common-lisp-indent-hook 1)
(put 'do.next.nodes 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.panels 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.plist 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.ptrip.arcs 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.ptrip.node.arcs 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.ptrip.nodes 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.route 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.rsr.links 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.scheduler.st.ffwps.table.freqs 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.train.movements 'fi:common-lisp-indent-hook '(like dolist))
(put 'do.wfreq.basic.wfreqs 'fi:common-lisp-indent-hook '(like dolist))
(put 'dolist.dbind 'fi:common-lisp-indent-hook '(like dolist))
(put 'if 'fi:common-lisp-indent-hook 3)
(put 'inhibiting.system.backup 'fi:common-lisp-indent-hook '(like dolist))
(put 'letv-slots 'fi:common-lisp-indent-hook 4)
(put 'listdo 'fi:common-lisp-indent-hook '(like dolist))
(put 'listdo-r 'fi:common-lisp-indent-hook '(like dolist))
(put 'object.let 'fi:common-lisp-indent-hook '(like with-slots))
(put 'tasksel.let 'fi:common-lisp-indent-hook '(like with-slots))
(put 'translating-in 'fi:common-lisp-indent-hook '(like dolist))
(put 'translating-out 'fi:common-lisp-indent-hook '(like dolist))
(put 'while.inhibited 'fi:common-lisp-indent-hook 2)
(put 'while.inhibiting 'fi:common-lisp-indent-hook 2)
(put 'with-translation-objects-in 'fi:common-lisp-indent-hook 1)
(put 'with.IP.SPACE.display 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.IP.SPACE.thermometer 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.boxes 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.button.thermometer 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.clipping.rectangle 'fi:common-lisp-indent-hook 1)
(put 'with.component.values 'fi:common-lisp-indent-hook '(like with-slots))
(put 'with.components 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.components.without.on-change 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.dispatcher.thermometer 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.force.output 'fi:common-lisp-indent-hook 1)
(put 'with.gcontext 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.gensyms 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.last.date.transfered.to.dispatcher 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.loaded.module 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.menu.items 'fi:common-lisp-indent-hook '(like defun))
(put 'with.modal.window 'fi:common-lisp-indent-hook 1)
(put 'with.new.candgroups 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.new.state 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.object.gcontext 'fi:common-lisp-indent-hook 1)
(put 'with.open.printer.protected 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.positions 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.rsr.connections.pattern.links.tasks.table 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.seq.gen.method 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.tasks.table 'fi:common-lisp-indent-hook '(like dolist))
(put 'with.thermometer 'fi:common-lisp-indent-hook '(like dolist))
(put 'within.state.world 'fi:common-lisp-indent-hook '(like dolist))
(put 'within.wd 'fi:common-lisp-indent-hook '(like dolist))
(put 'within.world.context 'fi:common-lisp-indent-hook '(like dolist))
(put 'within.world.state 'fi:common-lisp-indent-hook '(like dolist))
(put 'without.flickering 'fi:common-lisp-indent-hook '(like dolist))
(put 'without.on.change 'fi:common-lisp-indent-hook '(like dolist))


;;;-----------------------------------------------------------------------------
;;;FI::ADD-COMMON-LISP-POPUP-MENU-ITEMS
;;;Description
;;;	Defines the menu under the event control-mouse-3 click, that presents
;;;	menu options related with LISP
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{main-map} is a \emph{map}
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	09/02/13	J. P. Varandas	Created
;;;-----------------------------------------------------------------------------
(defun fi::add-common-lisp-popup-menu-items (main-map)
  (let* ((name "CL popup menu")
	 (map (make-sparse-keymap name)))
    (define-key main-map [C-mouse-3] (cons name map))
  
    (define-key map [macroexpand-recursively]
      (fi::menu "Macroexpand recursively" 'fi:lisp-macroexpand-recursively
		:enable '(fi::connection-open)))
  
    (define-key map [macroexpand]
      (fi::menu "Macroexpand" 'fi:lisp-macroexpand
		:enable '(fi::connection-open)))
  
    (define-key map [toggle-trace]
      (fi::menu "Toggle trace" 'fi:menu-toggle-trace-definition
		:enable '(fi::connection-open)))
  
    (define-key map [arglist]
      (fi::menu "Arglist" 'fi:menu-lisp-arglist
		:enable '(fi::connection-open)))
  
    (define-key map [find-next-definition]
      (fi::menu "Find next definition" 'fi:lisp-find-next-definition
		:enable '(fi::connection-open)))
  
    (define-key map [lisp-find-definition]
      (fi::menu "Find definition" 'fi:menu-lisp-find-definition
		:enable '(fi::connection-open)))
  
    (define-key map [sep1] (fi::menu "----"))
  
    (define-key map [compile-file]
      (fi::menu "Compile file" 'fi:menu-compile-file
		:enable '(fi::source-buffer-p)))
  
    (define-key map [compile-and-load-file]
      (fi::menu "Compile and load file" 'fi:menu-compile-and-load-file
		:enable '(fi::source-buffer-p)))
  
    (define-key map [compile-region]
      (fi::menu "Compile region" 'fi:lisp-compile-region
		:enable '(fi::acl-buffer-p)))
  
    (define-key map [compile-active-region-or-defun]
      (fi::menu "Compile form" 'fi:lisp-compile-active-region-or-defun
		:enable '(fi::acl-buffer-p)))))

;; Hooks
;;
;; History navigation
;; On Windows there's no initial listener via inferior-lisp-mode
;; so the only hook we need is `fi:lisp-listener-mode-hook'

;;;-----------------------------------------------------------------------------
;;;ADD-NEXT-PREVIOUS-TO-LISTENER-MODE
;;;Description
;;;	Sets key bindings for M-p and M-n to yank the previous and next
;;;	used expressions in the listener buffer.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created (POA.20221.0)
;;;-----------------------------------------------------------------------------
(defun add-next-previous-to-listener-mode ()
  (local-set-key [?\M-p] 'fi:pop-input)
  (local-set-key [?\M-n] 'fi:push-input))

(add-hook 'fi:lisp-listener-mode-hook
	  'add-next-previous-to-listener-mode)

;; Polling the lisp status eats CPU
(remove-hook 'fi:start-lisp-interface-hook 'fi:show-run-status)


;;;-----------------------------------------------------------------------------
;;;RUN-ALLEGRO-LISP-IMAGE
;;;Description
;;;	Runs ACL with the given image and connects to it.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		\arg{image} is a \emph{string} representing a file name.
;;;		
;;;		\arg{args} is a \emph{list} of \emph{string}.
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Doc updated
;;;-----------------------------------------------------------------------------
(defun run-allegro-lisp-image (image args)
  (setq fi:common-lisp-image-arguments nil)
  (fi:common-lisp allegro-common-lisp-buffer-name
		  allegro-common-lisp-directory
		  allegro-common-lisp-image-name
		  args
		  (system-name)
		  (if image
		      (format "%s.dxl" image)
		      allegro-common-lisp-image-file)))


;;;-----------------------------------------------------------------------------
;;;RUN-COMMON-LISP
;;;Description
;;;	Runs ACL with the default image and connects to it.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Doc updated
;;;-----------------------------------------------------------------------------
(defun run-common-lisp ()
  (interactive)
  (fi:common-lisp allegro-common-lisp-buffer-name
		  allegro-common-lisp-directory
		  allegro-common-lisp-image-name
		  allegro-common-lisp-image-arguments
		  allegro-common-lisp-host
		  allegro-common-lisp-image-file))


;;;-----------------------------------------------------------------------------
;;;GOTO-ALLEGRO-LISP-BUFFER
;;;Description
;;;	Switches to the current connection's REPL buffer.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Doc updated
;;;-----------------------------------------------------------------------------
(defun goto-allegro-lisp-buffer ()
  "Makes the *lisp* buffer the current buffer, running lisp if necessary"
  (interactive)
  (let ((buf (get-buffer allegro-common-lisp-buffer-name)))
    (if buf (pop-to-buffer buf))))


;;;-----------------------------------------------------------------------------
;;;LEP::FIND-FILE
;;;Description
;;;	Bring Emacs frame to front after editing command.
;;;	
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defadvice lep::find-file (after lep::file-find-focus-frame activate)
  (bring-to-front))

;;;-----------------------------------------------------------------------------
;;;FI::SHOW-FOUND-DEFINITION
;;;Description
;;;	Bring Emacs frame to front after editing command.
;;;	
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defadvice fi::show-found-definition (after fi::show-found-definition-focus-frame activate)
  (bring-to-front))


;;;-----------------------------------------------------------------------------
;;;REPL-NEWLINE
;;;Description
;;;	Simulates the user action that evaluates a form in the REPL.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		void
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Doc updated
;;;-----------------------------------------------------------------------------
(defun repl-newline ()
  (fi:inferior-lisp-newline))


;;;-----------------------------------------------------------------------------
;;;LISP-CONNECTION-CLEANUP
;;;Description
;;;	Generates the necessary Common-Lisp form to cleanup the connection's
;;;	side effects from the running image as much as possible.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		A \emph{tree} representing a LISP form.
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Created
;;;-----------------------------------------------------------------------------
(defun lisp-connection-cleanup ()
  `(setf lep::*connection* nil))


;;;-----------------------------------------------------------------------------
;;;LISP-PROCESS
;;;Description
;;;	Returns the current Lisp process.
;;;	
;;;	\visibility
;;;		SI
;;;		
;;;	\args
;;;		void
;;;		
;;;	\return-types
;;;		
;;;History
;;;	Date		Author		Description
;;;	82/06/20	P. Madeira	Doc updated
;;;-----------------------------------------------------------------------------
(defun lisp-process ()
  (get-process "*Allegro CL*"))

