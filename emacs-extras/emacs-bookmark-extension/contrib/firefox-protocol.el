;;; firefox-protocol.el --- Manage firefox protocols

;; Copyright (C) 2009 Thierry Volpiatto, all rights reserved

;; Author: thierry volpiatto
;; Maintainer: thierry volpiatto

;; Created: lun jan 12 11:23:02 2009 (+0100)

;; X-URL: http://mercurial.intuxication.org/hg/bookmark-extensions
;; Keywords: firefox, protocol

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

(eval-when-compile (require 'cl))

(defvar firefox-default-user-file "user.js")
(defvar firefox-default-mimeTypes-file "mimeTypes.rdf")

(defun firefox-protocol-handler-backup ()
  (let* ((mimeType-fname (concat (bmkext-get-firefox-user-init-dir)
                                 firefox-default-mimeTypes-file))
         (user-fname     (concat (bmkext-get-firefox-user-init-dir)
                                 firefox-default-user-file))
         (new-mimeType-fname (symbol-name
                              (gensym (file-name-sans-extension mimeType-fname))))
         (new-fs-file-fname (symbol-name
                             (gensym (file-name-sans-extension user-fname)))))
    (when (file-exists-p mimeType-fname)
      (copy-file mimeType-fname (concat new-mimeType-fname ".rdf")))
    (when (file-exists-p user-fname)    
      (copy-file user-fname (concat new-fs-file-fname ".js")))))
    
(defun firefox-protocol-installer-install (name path)
  (interactive "sProtocolName: \nsHandlerFileName: ")
  (when (y-or-n-p "Backup original files?")
    (firefox-protocol-handler-backup))
  (let ((regexp-start-section-js  (format "// Section %s" name))
        (regexp-end-section-js    (format "// End Section %s" name))
        (regexp-start-section-rdf (format "<!-- Section %s -->" name))
        (regexp-end-section-rdf   (format "<!-- End Section %s -->" name))
        (mimeType-fname (concat (bmkext-get-firefox-user-init-dir)
                                firefox-default-mimeTypes-file))
        (user-fname     (concat (bmkext-get-firefox-user-init-dir)
                                firefox-default-user-file)))
    ;; Write new protocol or replace an old entry to user.js file.
    (with-current-buffer (find-file-noselect user-fname)
      (goto-char (point-min))
      ;; If entry already exists, remove it and replace by new one.
      ;; Else we go at end of buffer and add new entry.
      (if (re-search-forward regexp-start-section-js nil t)
          (let ((beg (point-at-bol))
                (end (save-excursion
                       (re-search-forward regexp-end-section-js nil t)
                       (point))))
            (delete-region beg end)
            (goto-char beg))
          (goto-char (point-max)))
      ;; Now insert protocols in user.js.
      (insert
       (format "// Section %s" name)
       (format "\nuser_pref(\"network.protocol-handler.app.%s\", \"%s\");\n" name path)
       (format "user_pref(\"network.protocol-handler.external.%s\", true);\n" name)
       (format "// End Section %s" name))
      (save-buffer))
    ;; Notify mimeTypes.rdf about new handler.
    ;; If file doesn't exists, create it and add appropriate headers
    ;; and end balise.
    (unless (file-exists-p mimeType-fname)
      (with-current-buffer (find-file-noselect mimeType-fname)
        (goto-char (point-min))
        (insert
         "<?xml version=\"1.0\"?>\n"
         "<RDF:RDF xmlns:NC=\"http://home.netscape.com/NC-rdf#\"\n"
         "xmlns:RDF=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n"
         "\n</RDF:RDF>\n")
        (nxml-mode)
        (indent-region (point-min) (point-max))
        (save-buffer)))
    (with-current-buffer (find-file-noselect mimeType-fname)
      ;; If entry already exists, remove it and replace by new one.
      ;; Else go to line 3 just after headers.
      (goto-char (point-min))
      (if (re-search-forward regexp-start-section-rdf nil t)
          (let ((beg (point-at-bol))
                (end (save-excursion
                       (re-search-forward regexp-end-section-rdf nil t)
                       (point))))
            (delete-region beg end)
            (goto-char beg))
          (goto-char (point-min))
          (forward-line 3))
      ;; Now insert new handlers mime types.
      (insert (format "\n<!-- Section %s -->\n" name))
      (insert
       (format "<RDF:Description RDF:about=\"urn:scheme:externalApplication:%s\"\n" name)
       (format "NC:prettyName=\"%s\"\n" name)
       (format "NC:path=\"/home/thierry/bin/%s\" />\n" name)
       (format "<RDF:Description RDF:about=\"urn:scheme:%s\"\n" name)
       (format "NC:value=\"%s\">\n" name)
       (format "<NC:handlerProp RDF:resource=\"urn:scheme:handler:%s\"/>\n" name)
       "</RDF:Description>\n"
       (format "<RDF:Description RDF:about=\"urn:handler:local:%s\"\n" path)
       (format "NC:prettyName=\"%s\"\n" name)
       (format "NC:path=\"%s\" />\n" path)
       "\n<RDF:Seq RDF:about=\"urn:schemes:root\">\n"  
       (format "<RDF:li RDF:resource=\"urn:scheme:%s\"/>\n" name)
       "</RDF:Seq>\n"
       (format "<RDF:Description RDF:about=\"urn:scheme:handler:%s\"\n" name)
       "NC:alwaysAsk=\"false\">\n"
       (format "<NC:externalApplication RDF:resource=\"urn:scheme:externalApplication:%s\"/>\n" name)
       (format "<NC:possibleApplication RDF:resource=\"urn:handler:local:%s\"/>\n" path)
       "</RDF:Description>\n"
       (format "<!-- End Section %s -->\n" name))
      (nxml-mode)
      (indent-region (point-min) (point-max))
      (save-buffer))))

;;; Provide
(provide 'firefox-protocol)

;;; firefox-protocol.el ends here
