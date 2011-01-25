;;; bookmark-firefox-handler.el --- 
;; 
;; Author: 
;; Maintainer: 
;; 
;; Created: <2009-11-17 Mar. 21:35>
;; Version: 
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; It is to bookmark page from firefox to Emacs Bookmarks.
;;
;; It work just like org-annotation-helper.
;;
;; It's a work in process, it is already working here but firefox still
;; difficult to configure in versions > 3, a bug maybe.
;; I will add more info soon ;-)
;;
;; You will find the shell script `emacsbookmark' in this directory.
;;
;; The bookmarklet:
;;
;;-----------------------------------------------------------------------------------------------------------
;; javascript:location.href='emacsbookmark://' + location.href + '::emacsbookmark::' + escape(document.title)
;;-----------------------------------------------------------------------------------------------------------
;;
;; The protocol handlers:(add to your user.js)
;;
;; user_pref("network.protocol-handler.external.emacsbookmark", true);
;; user_pref("network.protocol-handler.app.emacsbookmark", "/home/thierry/bin/emacsbookmark");
;;
;; You will have to hack also the file: mimeTypes.rdf (good luck) due to a bug in firefox-3.*
;;
;; If all went well, you should be able to bookmark a page in firefox with one click on
;; your bookmarklet. :-)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'url)
(autoload 'url-unhex-string "url")

(defvar bmkext-firefox-info nil)
(defun bmkext-get-firefox-bmk (bmk)
  "Bookmark a Firefox page in Standards Emacs bookmarks.
BMK is the value returned by the bookmarklet."
  (interactive)
  (let* ((split (split-string bmk "::emacsbookmark::"))
         (url   (replace-regexp-in-string "emacsbookmark://" "" (car split)))
         (title (cadr split)))
    (setq url (url-unhex-string url t))
    (setq title (url-unhex-string title t))
    (setq bmkext-firefox-info (cons title url))
    (if (and title url (y-or-n-p (format "Bookmark (%s) from Firefox?" title)))
        (progn
          (if (not (member title (bookmark-all-names)))
              (progn
                (setq bookmark-alist
                      (bmkext-bookmark-firefox-page bmkext-firefox-info))
                (bmkext-maybe-save-bookmark)
                (call-interactively #'bookmark-bmenu-list)
                (bmkext-bmenu-goto-bookmark title))
              (message "Bookmark (%s) already exists." title)))
        (message "Abort Firefox bookmarking"))))


(defun bmkext-bookmark-firefox-page (bmk)
  "Return `bookmark-alist' with the firefox bookmark BMK appended to it."
  (append
   (list (bmkext-format-html-bmk bmkext-firefox-info "firefox-record"))
   bookmark-alist))

;; Jump in w3m from firefox
(defun firefox-browse-url-w3m (data)
  "Switch to emacs w3m from firefox.
Use this bookmarklet:

javascript:location.href='browsew3m://' + location.href.

Install protocol `browsew3m' binded to path of browsew3m script."
  (let* ((split (split-string data "browsew3m://"))
         (url (second split)))
    (w3m-browse-url url)))


(provide 'bookmark-firefox-handler)

;;; bookmark-firefox-handler.el ends here
