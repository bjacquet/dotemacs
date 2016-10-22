;;; ---------------------------------------------------------------------
;;; Siscog's Org Mode
;;;

(require 'org)

(setq org-todo-keywords '((sequence "TODO(t)" "MAYBE(m)" "WAITING(w)" "|")
			  (sequence "|" "SUSPENDED(S)")
			  (sequence "OPEN(o)" "WIP(i)" "REVIEW(v)" "|" "SEP(s)" "DONE(d)" "RESOLVED(r)"))
      org-todo-keyword-faces '(("MAYBE"     . (:foreground "gold" :weight bold))
			       ("SUSPENDED" . shadow)
			       ("WAITING"   . (:foreground "gold" :weight bold))
			       ("WIP"       . (:foreground "orange red" :weight bold)))
      org-hide-leading-stars t
      org-footnote-define-inline t
      org-export-html-preamble nil
      org-export-html-postamble nil
      org-export-html-style-include-default nil
      org-columns-default-format "%45ITEM %10TODO %LastAction %LastActionDate %NextAction"
      org-clock-into-drawer "CLOCK"
      org-agenda-start-with-log-mode t
      org-agenda-start-with-clockreport-mode t
      org-link-search-must-match-exact-headline nil
      org-agenda-inhibit-startup nil)


;; Add extra tweaks when org-mode is started
(defvar org-extra-installed-p nil)
(add-hook 'org-mode-hook 'org-extra-install)
(add-hook 'org-agenda-mode-hook 'org-extra-install)


(defun org-extra-install ()
  "Add customisations to Org mode. This function is designed to be
added the mode hook, `org-mode-hook'"
  ;; Extras that need installing for every org-mode buffer
  ;; ... none at the moment
  ;;
  ;; One-off extras
  (unless org-extra-installed-p
    ;; Link handlers for PMS
    ;; eg [[POA:12345]] will create a clickable link to POA 12345.0
    (setq org-link-abbrev-alist
          `(("TASK" . org-extra-link-abbrev-task)
            ("POA" . org-extra-link-abbrev-poa)
            ,@org-link-abbrev-alist))
    ;; Jump to lisp definition
    (org-add-link-type "def" 'org-extra-link-def)
    (setq org-extra-installed-p t)))


(defun org-extra-split-poa (poa)
  (multiple-value-bind (poa life)
      (split-string poa "\\.")
    (list poa (or life 0))))


(defun org-extra-link-abbrev-poa (poa)
  "Returns a link to a POA"
  (apply 'format
         "https://pms.siscog/main_frame_link.asp?module=defects&category=poa&id=%s&life=%s"
         (org-extra-split-poa poa)))


(defun org-extra-link-abbrev-task (task)
  "Returns a link to a task"
  (format "https://pms.siscog/main_frame_link.asp?module=tasks&category=defect&id=%s" task))


(defun org-extra-link-def (thing)
  "Look up THING using in Allegro and go to its definition.
THING can be a symbol, an fspec, or their string representation."
  (fi::lisp-find-definition-common thing :other-window))


(defconst *bj:note.template.file*
  (bj:concat-package-dir "sc-org/note_template.txt"))


(defconst *bj:clock.template.file*
  (bj:concat-package-dir "sc-org/clock_template.txt"))


(defconst *bj:buffer.sandbox*
  "*sc-org-mode sandbox*"
  "Temporary buffer name for text and keyword replacements.")


(defun bj:get.user.input.poa ()
  (let* ((number            (read-from-minibuffer "POA number: "))
	 (description       (read-from-minibuffer "POA description: "))
	 (note.filename.aux (replace-regexp-in-string "clock" "notes" (buffer-name)))
	 (note.filename     (read-file-name "Note filename: "
					    nil
					    (expand-file-name note.filename.aux))))
    (values number description note.filename)))


(defun bj:fill.buffer.sandbox (template.file replacements)
  (set-buffer (bj:get.buffer.sandbox))
  (insert-file-contents template.file)
  (while (not (null replacements))
    (setq keyword (first (first replacements))
	  value (second (first replacements)))
    (goto-char (point-min))
    (perform-replace keyword value nil nil nil)
    (setq replacements (rest replacements))))


(defun bj:get.buffer.sandbox ()
  (get-buffer-create *bj:buffer.sandbox*))


(defun bj:write.buffer (buffer point)
  (set-buffer buffer)
  (goto-char point)
  (insert-buffer-substring (bj:get.buffer.sandbox)))


(defun bj:create.entry (buffer point template replacements)
  (bj:fill.buffer.sandbox template replacements)
  (bj:write.buffer buffer point)
  (kill-buffer (bj:get.buffer.sandbox)))


(defun bj:create.poa ()
  (interactive)
  (let ((number)
	(description)
	(note.filename)
	(clock.filename (buffer-file-name))
	(replacements))
    (multiple-value-setq (number description note.filename) (bj:get.user.input.poa))
    (setq replacements (list (list "<number>"         number)
			     (list "<description>"    description)
			     (list "<note_filename>"  note.filename)
			     (list "<clock_filename>" clock.filename)))
    (bj:create.entry (find-file clock.filename)
    		  (point)
    		  *bj:clock.template.file*
    		  replacements )
    (when note.filename
      (bj:create.entry (find-file note.filename)
		    0
		    *bj:note.template.file*
		    replacements))
   (find-file clock.filename)))


(defconst *pms.resolution.template.file*
  (bj:concat-package-dir "sc-org/pms_resolution_template.txt"))


(defun bj:get.user.input.pms.resolution ()
  (let* ((system      (trim-str (upcase (read-from-minibuffer "System: "))))
	 (author      (read-from-minibuffer "Author: " user-full-name))
	 (modspatches (if (string= "VDEV" (substring system -4 nil))
			  "MODS"
			"PATCHES"))
	 (files       "")
	 (file        (trim-str (downcase (read-from-minibuffer ": ")))))
    (while (not (string= file ""))
      (setq files (concat files " - " file "\n")
	    file (trim-str (downcase (read-from-minibuffer ": ")))))
    (if (> (length files) 0)
	(setq files (substring files 0 -1))) ; to remove that extra '\n'
    (values system author modspatches files)))


(defun bj:create.pms.resolution ()
  (interactive)
  (let ((today.date (format-time-string "%d/%m/%Y"))
	(system)
	(author)
	(modspatches)
	(overscore)
	(underscore)
	(slashes)
	(replacements)
	(files))
    (multiple-value-setq (system author modspatches files) (bj:get.user.input.pms.resolution))
    (setq overscore  (make-string (length system) ?_)
	  underscore (make-string (- 40 (length system) 3) ?_)
	  slashes    (make-string (- 40 (length author) 17) ?-))
    (setq replacements (list (list "<overscore>"   overscore)
			     (list "<system>"      system)
			     (list "<underscore>"  underscore)
			     (list "<author>"      author)
			     (list "<date>"        today.date)
			     (list "<slashes>"     slashes)
			     (list "<modspatches>" modspatches)
			     (list "<files>"       files)))
    (bj:create.entry (get-buffer-create "*scratch*")
		  0
		  *bj:pms.resolution.template.file*
		  replacements))
  (switch-to-buffer "*scratch*"))

;; sc-org.el ends here
