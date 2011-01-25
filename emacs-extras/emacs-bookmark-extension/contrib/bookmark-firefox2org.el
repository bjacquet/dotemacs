;;; bookmark-firefox2org.el --- Convert html bookmarks to org file

;; Copyright (C) 2009 Thierry Volpiatto, all rights reserved

;; Author: thierry volpiatto
;; Maintainer: thierry volpiatto

;; Created: lun jan 12 11:23:02 2009 (+0100)

;; X-URL: http://mercurial.intuxication.org/hg/bookmark-extensions
;; Keywords: firefox, org

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  ==========
;;
;; Experimental, use with care.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code

;;; Org importation.

(defun bmkext-format-html-bmk-to-org (bmk)
  "Convert an alist entry (title . url) to org link."
  (let ((title (replace-regexp-in-string "^\>" "" (car bmk)))
        (fname (replace-regexp-in-string "\"" "" (cdr bmk))))
    (concat "** [[" fname "][" title "]]\n")))

(defun bmkext-html-bookmarks-to-org (input-file output-file regexp &optional title-page)
  "Convert an html bookmark file to org file."
  (with-current-buffer (find-file-noselect output-file)
    (goto-char (point-max))
    (unless (eq (point-min) (point-max))
      (newline))
    (when title-page
      (insert (concat title-page "\n\n")))
    (loop
       with alist = (bmkext-html-bookmarks-to-alist input-file regexp)
       for i in alist
       do
         (insert
          (bmkext-format-html-bmk-to-org i)))
    (save-buffer)))
  

(defvar bmkext-firefox2org-default-title "* Firefox Bookmarks")
(defvar bmkext-w3m2org-default-title "* W3m Bookmarks")

;;;###autoload
(defun bmkext-firefox2org-create (org-file)
  "Print Firefox bookmarks to ORG-FILE.
That append all bookmarks to file if file exists.
Use `bmkext-firefox2org-sync' instead if you want to update."
  (interactive "sOrgFile: ")
  (bmkext-html-bookmarks-to-org (bmkext-guess-firefox-bookmark-file)
                                org-file
                                bmkext-firefox-bookmark-url-regexp
                                bmkext-firefox2org-default-title))

;;;###autoload
(defun bmkext-w3m2org-create (org-file)
  "Print w3m bookmarks to ORG-FILE.
That append all bookmarks to file if file exists.
Use `bmkext-w3m2org-sync' instead if you want to update."
  (interactive "sOrgFile: ")
  (bmkext-html-bookmarks-to-org w3m-bookmark-file
                                org-file
                                bmkext-w3m-bookmark-url-regexp
                                bmkext-w3m2org-default-title))

(defun bmkext-html-bookmarks-to-org-sync (input-file
                                          output-file
                                          regexp
                                          &optional start-title)
  "Synchronize html INPUT-FILE with Org OUTPUT-FILE.
REGEXP is the regexp to use to match entries in INPUT-FILE (e.g Firefox or w3m).
See `bmkext-firefox-bookmark-url-regexp', `bmkext-w3m-bookmark-url-regexp'.
START-TITLE is the name with star where we will start (e.g * Firefox Bookmarks)."
  (with-current-buffer (find-file-noselect output-file)
    (goto-char (point-min))
    (if start-title
        (progn (search-forward start-title) (forward-line 1))
        (forward-line 1))
    (org-mode)
    (let ((beg (point))
          (end (or (when (re-search-forward "^\*\\{1\\} " nil t)
                     (forward-line -2) (point))
                   (point-max)))
          org-old org-new)
      (goto-char beg)
      (save-restriction
        (narrow-to-region beg end)
        (save-excursion ; List all currents bookmarks in this org node. 
          (while (search-forward "** " nil t)
            (push (concat
                   (buffer-substring-no-properties (point-at-bol) (point-at-eol)) "\n")
                  org-old)))
        (setq org-new (loop ; New html bmks formatted for org
                         with alist = (bmkext-html-bookmarks-to-alist input-file regexp)
                         for i in alist
                         collect (bmkext-format-html-bmk-to-org i)))
        (loop
           for lnk in org-new
           if (member lnk org-old) ; Old bookmark already exists.
           do (search-forward "** " nil t) ; We skip it and go forward.
           else ; Not found in ORG-OLD it's a new one we print it.
           do
             (forward-line 0)
             (insert lnk)
             (search-forward "** " nil t))
        (save-buffer)))))

;;;###autoload
(defun bmkext-firefox2org-sync (org-file)
  "Synchronize Firefox bookmarks with ORG-FILE."
  (interactive "sOrgFile: ")
  (bmkext-html-bookmarks-to-org-sync (bmkext-guess-firefox-bookmark-file)
                                     org-file
                                     bmkext-firefox-bookmark-url-regexp
                                     bmkext-firefox2org-default-title))

;;;###autoload
(defun bmkext-w3m2org-sync (org-file)
  "Synchronize w3m bookmarks with ORG-FILE."
  (interactive "sOrgFile: ")
  (bmkext-html-bookmarks-to-org-sync w3m-bookmark-file
                                     org-file
                                     bmkext-w3m-bookmark-url-regexp
                                     bmkext-w3m2org-default-title))


(provide 'bookmark-firefox2org)

;;; bookmark-firefox2org.el ends here
