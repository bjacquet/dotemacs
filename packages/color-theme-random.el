;;; color-theme-random.el ---  random pick a color theme

;; Copyright (C) 2008, Chaoji Li

;; Author: Chaoji Li <lichaoji AT gmail DOT com>
;; Version: 0.1
;; Date: Oct 02, 2008

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Put this file in a folder where Emacs can find it.
;;
;; Add following lines to your .emacs initialization file:
;;
;;     (require 'color-theme-random)
;;     (color-theme-random)
;;


(defvar bj:current-color-theme nil)

(defun bj:color-theme-current-theme ()
  (interactive)
  (message (format "Current theme is: %s" 
		   (symbol-name bj:current-color-theme))))

(defvar bj:color-theme-random-init nil)

(defvar bj:my-fav-color-themes
  '(;; aalto-dark
    ;; aalto-light
    ;; aliceblue
    ;; andreas
    ;; arjen
    ;; beige-diff
    ;; beige-eshell
    ;; bharadwaj-slate
    ;; bharadwaj
    (billw)
    ;; black-on-gray
    ;; blippblopp
    ;; blue-erc
    ;; blue-eshell
    ;; blue-gnus
    (blue-mood)
    ;; blue-sea
    ;; calm-forest
    (charcoal-black)
    (clarity)
    ;; classic
    (cobalt)
    ;; comidia
    ;; dark-blue
    (dark-blue2)
    ;; dark-erc
    ;; dark-font-lock
    ;; dark-gnus
    ;; dark-green
    ;; dark-info
    (dark-laptop)
    (deep-blue)
    (desert)
    ;; digital-ofs1
    ;; emacs-21
    ;; emacs-nw
    ;; euphoria
    ;; feng-shui
    ;; fischmeister
    ;; gnome
    ;; gnome2
    (goldenrod)
    ;; gray1
    (gray30)
    ;; greiner
    ;; gtk-ide
    ;; high-contrast
    (hober)
    ;; infodoc
    ;; jb-simple
    ;; jedit-grey
    ;; jonadabian-slate
    ;; jonadabian
    (jsc-dark)
    ;; jsc-light
    ;; jsc-light2
    (julie)
    ;; katester
    ;; kingsajz
    ;; late-night
    ;; lawrence
    ;; ld-dark
    ;; lethe
    ;; marine
    ;; marquardt
    ;; matrix
    ;; midnight
    ;; mistyday
    ;; montz
    ;; oswald
    ;; parus
    ;; pierson
    ;; pok-wob
    ;; pok-wog
    (railscast)
    ;; ramangalahy
    ;; raspopovic
    ;; renegade
    ;; resolve
    ;; retro-green
    ;; retro-orange
    ;; robin-hood
    ;; rotor
    ;; ryerson
    ;; salmon-diff
    ;; salmon-font-lock
    ;; scintilla
    ;; shaman
    (simple-1)
    ;; sitaramv-nt
    ;; sitaramv-solaris
    ;; snow
    ;; snowish
    ;; standard-ediff
    ;; standard
    (subdued)
    ;; subtle-blue
    ;; subtle-hacker
    ;; taming-mr-arneson
    ;; taylor
    ;; tty-dark
    ;; vim-colors
    ;; whateveryouwant
    ;; wheat
    ;; word-perfect
    ;; xemacs
    ;; xp

    ;; My added themes:
    (birds-of-paradise-plus)
    (darktooth)
    (naquadah)
    (nord)
    (tango-2)
    (tangotango)
    ))

(defun bj:give-other-themes-a-chance ()
  (funcall (car (nth (random (length color-themes)) color-themes))))

(defun bj:color-theme-random ()
  (interactive)
  (disable-theme bj:current-color-theme)
  (unless bj:color-theme-random-init (random t))
  (setq bj:color-theme-random-init t)
  (let ((weight-so-far 0) weight)
    (dolist (theme bj:my-fav-color-themes)
      (setq weight (nth 1 theme))
      (unless weight (setq weight 1)) ;; Default 1
      (if (>= (random (+ weight weight-so-far)) weight-so-far)
	  (setq bj:current-color-theme (car theme)))
      (setq weight-so-far (+ weight-so-far weight)))
    (when bj:current-color-theme
      (when (eq bj:current-color-theme 'solarized)
	(set-frame-parameter nil 'background-mode 'dark))
      (load-theme bj:current-color-theme t t)
      (enable-theme bj:current-color-theme))
    (message (format "Random color theme: %s" (symbol-name bj:current-color-theme)))))

(provide 'color-theme-random)
