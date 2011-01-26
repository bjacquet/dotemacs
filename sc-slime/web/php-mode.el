;;; php-mode.el --- major mode for editing PHP3 code

;; Copyright (C) 2000 Turadg Aleahmad

;; Author: Turadg Aleahmad <turadg@guru.nu>
;; Maintainer: turadg@guru.nu
;; Keywords: php languages oop
;; Created: 1999-05-17
;; Modified: 2000-03-08

(defconst php-version "0.9.2"
  "PHP Mode version number.")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Usage

;; Place this file in your Emacs lisp path (eg. site-lisp)
;; and add to your .emacs file:
;; (require 'php-mode)

;; To change the indentation, modify the c-basic-offset.
;; You can do this through the customization facility,
;; or add this to your .emacs file:
;; (custom-set-variables
;;  '(c-basic-offset 4))

;;; Commentary

;; PHP mode is a major mode for editing PHP3 source code.  It inherits
;; the functionality of C mode.  The key difference is that it will
;; font-lock according to the PHP3 grammar.

;; I've received many reports of problems with XEmacs.  This mode
;; officially supports only GNU Emacs.  I only want to hear about
;; XEmacs if you have the solution to making it work.

;; By version 1.1 or 2.0, I plan to implement the functions referred to
;; in the PHP3 menu.  Please e-mail me any tips or code you have to
;; implement this.  And if you can think of any other useful features,
;; please let me know.   -- Turadg Aleahmad <turadg@guru.nu>

;;; Changelog

;; 0.9.2 2000-03-08
;;      Fixed bug with 1-character identifiers
;;      Fixed bug with class declaration coloring
;;      Added coloring for true, false, null
;;		Officially not supporting XEmacs (see above)
;; 0.9.1 2000-02-21
;;		Disabled keywords in XEmacs for compatibility (so I thought)
;;		Added usage info to comments

;; 0.9 2000-01-09
;;		Clarified bug with XEmacs ("Juanjo (DMAT)" juanjo@maite29.upc.es)
;;		Fixed minor bug with comment highlighting (Juanjo)
;;		Syntax parsing from PHP3 lexical scanner ("Torsten Martinsen" tma@gatehouse.dk)
;;		Highlights function and method names now
;;		Highlights "this" as keyword when used as an object in variable names

;; 0.8 1999-05-17
;;		Initial release.



;;; Code:

(require 'font-lock)
(require 'cc-mode)

(provide 'php-mode)

;;;###autoload
(define-derived-mode php-mode c-mode "PHP3"
  "Major mode for editing PHP code.\n\n\\{php-mode-map}"
  (setq case-fold-search t)
  (turn-on-font-lock)
  (setq font-lock-maximum-decoration t)
  )

;; Make php-mode the default mode for PHP source code buffers.
;;;###autoload
(setq auto-mode-alist
      (append
       '( ("\\.php\\'" . php-mode) ("\\.php3\\'" . php-mode) )
       auto-mode-alist))

;; Make a menu keymap (with a prompt string)
;; and make it the menu bar item's definition.
(define-key php-mode-map [menu-bar] (make-sparse-keymap))
(define-key php-mode-map [menu-bar php]
  (cons "PHP3" (make-sparse-keymap "PHP3")))

;; Define specific subcommands in this menu.
(define-key php-mode-map [menu-bar php complete-function]
  '("Complete function name" . php-complete-function))
(define-key php-mode-map
  [menu-bar php document-function]
  '("View function documentation" . php-document-function))
(define-key php-mode-map
  [menu-bar php browse-manual]
  '("Browse manual" . php-browse-manual))

;; Define function name completion function
(defun php-complete-function ()
  "Complete the function name at the point from known PHP3 functions."
  (interactive)
  (message "php-complete-function not implemented yet"))

;; Define function documentation function
(defun php-document-function ()
  "Bring up documentation for the function at the point."
  (interactive)
  (message "php-document-function not implemented yet"))

;; Define function for browsing manual
(defun php-browse-manual ()
  "Bring up manual for PHP3."
  (interactive)
  (message "php-browse-manual not implemented yet"))

;; Define abbreviations and their expansions
(define-abbrev php-mode-abbrev-table "ex" "extends")
(define-abbrev php-mode-abbrev-table "fu" "function")
(define-abbrev php-mode-abbrev-table "GL" "GLOBAL")
(define-abbrev php-mode-abbrev-table "req" "require(")
(define-abbrev php-mode-abbrev-table "ret" "return")

;; Set up font locking
(defconst php-font-lock-keywords-1 nil
  "Subdued level highlighting for PHP mode.")

(defconst php-font-lock-keywords-2 nil
  "Medium level highlighting for PHP mode.")

(defconst php-font-lock-keywords-3 nil
  "Gauchy level highlighting for PHP mode.")

(let* (
       (php-keywords 
	(regexp-opt
	 '("break" "continue" "do" "else" "for" "if" "return"
	   "switch" "while" "super" "new" "extends" "php" ) t))
       (php-types
	(regexp-opt
	 '("static" "const" "int" "integer" "real" "double" "float"
	   "string" "array" "object" "var" "GLOBAL" "function")))
       (php-builtins
	(regexp-opt
	 '("echo" "print" "require" "include")))
       )
  (setq php-font-lock-keywords-1
	(list
	 ;; Fontify type specifiers.
	 (cons (concat "\\<\\(" php-types "\\)\\>") 'font-lock-type-face)
	 ))

  (setq php-font-lock-keywords-2
    (append php-font-lock-keywords-1
      (list
       ;; Fontify keywords (except case, default and goto; see below).
       (cons (concat "\\<" php-keywords "\\>") 'font-lock-keyword-face)
	   ;;
       ;; Fontify case/goto keywords and targets, and case default/goto tags.
       '("\\<\\(case\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
	 (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))
	   ;;
	   ;; Fontify all constants.
	   '("\\<\\(false\\|null\\|true\\)\\>" . font-lock-constant-face)
	   ;;
       ;; Fontify function declarations
       '("\\<\\(function\\)\\s-+\\(\\sw+\\)\\s-*("
	 (1 font-lock-keyword-face)
	 (2 font-lock-function-name-face)
	 )
       )
      ))
    
  (setq php-font-lock-keywords-3
    (append php-font-lock-keywords-2
      (list
       ;; Fontify function and method names
       '("\\<\\(\\$\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)->\\)?\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)\\s-*("
	 (2 font-lock-variable-name-face t t)
	 (3 font-lock-function-name-face t t)
	 )
       ;; Fontify variable name references.
       '("\\(\\$\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*->\\)?\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)\\)"
	 (1 font-lock-variable-name-face t t)
	 (2 font-lock-variable-name-face t t)
	 )
       '("\\<\\$\\(this\\)->"
	 (1 font-lock-keyword-face t t) ;; "this" as keyword
	 )
       ;; Fontify class names
       '("\\<\\(class\\)\\s-+\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)\\(\\(\\s-+extends\\s-+\\)\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)\\)?\\s-*\\{"
	 (1 font-lock-keyword-face t t)  ;; class keyword
	 (2 font-lock-type-face t t)  ;; class name
	 (4 font-lock-keyword-face t t)   ;; extend keyword
	 (5 font-lock-type-face t t) ;; super class name
	 )
       )))
  )

(defvar php-font-lock-keywords php-font-lock-keywords-3
  "Default expressions to highlight in PHP mode.")

;; font-lock-add-keywords is only defined in GNU Emacs
(if (not (string-match "XEmacs" emacs-version))
    (font-lock-add-keywords 'php-mode php-font-lock-keywords))

;; Add char '#' as a start comment delimiter
;; we don't need to set up the end comment delimiter
(modify-syntax-entry ?\# "< b")
