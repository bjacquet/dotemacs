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


(require 'color-theme)

(setq color-theme-history-max-length 10)

(defun color-theme-current-theme ()
  (interactive)
  (message (format "Current theme is: %s" 
		   (symbol-name (car (car color-theme-history))))))

(defvar color-theme-random-init nil)

(defvar my-fav-color-themes
  '(					; (color-theme-aalto-dark)
					; (color-theme-aalto-light)
					; (color-theme-aliceblue)
					; (color-theme-andreas)
    (color-theme-arjen)
					; (color-theme-beige-diff)
    (color-theme-bharadwaj)
    (color-theme-bharadwaj-slate)
    (color-theme-billw)
					; (color-theme-black-on-gray)
					; (color-theme-blippblopp)
    (color-theme-simple-1)
					; (color-theme-blue-erc)
					; (color-theme-blue-gnus)
    (color-theme-blue-mood)
					; (color-theme-blue-sea)
    (color-theme-calm-forest)
    (color-theme-charcoal-black)
    (color-theme-goldenrod)
    (color-theme-clarity)
    (color-theme-classic)
    (color-theme-comidia)
    (color-theme-jsc-dark)
					; (color-theme-jsc-light)
					; (color-theme-jsc-light2)
    (color-theme-dark-blue)
    (color-theme-dark-blue2)
					; (color-theme-dark-green)
    (color-theme-dark-laptop)
    (color-theme-deep-blue)
					; (color-theme-digital-ofs1)
    (color-theme-euphoria)
					; (color-theme-feng-shui)
					; (color-theme-fischmeister)
					; (color-theme-gnome)
    (color-theme-gnome2)
    (color-theme-gray1)
    (color-theme-gray30)
    (color-theme-kingsajz)
					; (color-theme-greiner)
					; (color-theme-gtk-ide)
					; (color-theme-high-contrast)
    (color-theme-hober)
					; (color-theme-infodoc)
					; (color-theme-jb-simple)
    (color-theme-jedit-grey)
    (color-theme-jonadabian)
    (color-theme-jonadabian-slate)
					; (color-theme-katester)
					; (color-theme-late-night)
    (color-theme-lawrence)
    (color-theme-lethe)
    (color-theme-ld-dark)
    (color-theme-marine)
					; (color-theme-matrix)
    (color-theme-marquardt)
    (color-theme-midnight)
					; (color-theme-mistyday)
					; (color-theme-montz)
					; (color-theme-oswald)
    (color-theme-parus)
					; (color-theme-pierson)
    (color-theme-ramangalahy)
					; (color-theme-raspopovic)
    (color-theme-renegade)
    (color-theme-resolve)
					; (color-theme-retro-green)
					; (color-theme-retro-orange)
    (color-theme-robin-hood)
					; (color-theme-rotor)
    (color-theme-ryerson)
					; (color-theme-salmon-diff)
    (color-theme-salmon-font-lock)
					; (color-theme-scintilla)
    (color-theme-shaman)
					; (color-theme-sitaramv-nt)
    (color-theme-sitaramv-solaris)
					; (color-theme-snow)
					; (color-theme-snowish)
					; (color-theme-standard-ediff)
					; (color-theme-standard)
					; (color-theme-emacs-21)
					; (color-theme-emacs-nw)
					; (color-theme-xemacs)
    (color-theme-subtle-blue)
    (color-theme-subtle-hacker)
    (color-theme-taming-mr-arneson)
    (color-theme-taylor)
    (color-theme-tty-dark)
					; (color-theme-vim-colors)
					; (color-theme-whateveryouwant)
    (color-theme-wheat)
    (color-theme-pok-wob)
    (color-theme-pok-wog)
    (color-theme-word-perfect)
    (color-theme-xp)
    (color-theme-tango)
    (color-theme-tangotango)
    (color-theme-solarized-dark)
					; (color-theme-solarized-light)
    (color-theme-cobalt)
    (color-theme-almost-monokai)
					; (give-other-themes-a-chance)
    ))

(defun give-other-themes-a-chance ()
  (funcall (car (nth ( random (length color-themes)) color-themes))))

(defun color-theme-random ()
  (interactive)
  (unless color-theme-random-init (random t))
  (setq color-theme-random-init t)
  (let (selected-theme (weight-so-far 0) weight)
    (dolist (theme my-fav-color-themes)
      (setq weight (nth 1 theme))
      (unless weight (setq weight 1)) ;; Default 1
      (if (>= (random (+ weight weight-so-far)) weight-so-far)
	  (setq selected-theme (car theme)))
      (setq weight-so-far (+ weight-so-far weight)))
    (if selected-theme
	(funcall selected-theme))
    (message (format "Random color theme: %s" (symbol-name selected-theme)))))

(provide 'color-theme-random)
