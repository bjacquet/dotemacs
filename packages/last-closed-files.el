;;; last-closed-files.el --- Lists the previous closed files.

;; Author: world wide web
;; Maintainer: Bruno Jacquet (bruno.jacquet@gmail.com)
;; Version: 0.0.1
;; Keywords: tools convenience

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;

;;; Installation:
;;
;; 

;;; Customize:
;;
;; 


;;; Code:

(defvar closed-files (list))

(defun track-closed-file ()
  (message buffer-file-name)
  (and buffer-file-name
       (add-to-list 'closed-files buffer-file-name)))

(defun track-closed-file ()
  (and buffer-file-name
       (message buffer-file-name)
       (or (delete buffer-file-name closed-files)
	   t)
       (add-to-list 'closed-files buffer-file-name)))

(defun last-closed-files ()
  (interactive)
  (find-file (completing-read "Last closed: " closed-files)))

(add-hook 'kill-buffer-hook 'track-closed-file)

(provide 'last-closed-files)

;; last-closed-files.el EOF
