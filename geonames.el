;;; geonames.el --- GeoNames.org support for Emacs

;; Copyright (C) 2012 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; This file is NOT part of GNU Emacs.

;; geonames.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; geonames.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with osm-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is part of osm-mode.  It is an Emacs interface to
;; http://www.geonames.org/

;;; Code:

(defgroup geonames nil
  "GeoNames."
  :group 'osm-mode)

(defcustom geonames-username nil
  "The GeoNames Username: http://www.geonames.org/manageaccount."
  :group 'geonames
  :type 'string)

(defcustom geonames-url "api.geonames.org"
  "The GeoNames base URL."
  :group 'geonames
  :type 'string)

(defun geonames~retrieve-handle (status url handle no-cache)
  "Handle retrieved data.
Internal function."
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (unless (search-forward "\n\n" nil t)
    (kill-buffer)
    (error "Error in http reply"))
  (let ((headers (buffer-substring (point-min) (point))))
    (unless (string-match-p "^HTTP/1.1 200 OK" headers)
      (kill-buffer)
      (error "Unable to fetch data"))
    (unless no-cache
      (url-store-in-cache (current-buffer))))
  (let ((json (json-read)))
    (kill-buffer)
    (if handle
        (funcall handle url json)
      json)))

(defun geonames-retrieve (url handle &optional no-cache)
  "Fetch URL asynchronously and pass the parsed json to HANDLE."
  (url-retrieve url
                'geonames~retrieve-handle
                (list url handle no-cache)))

(defun geonames-retrieve-s (url &optional no-cache)
  "Fetch URL synchronously and return parsed json data.
Synchronous version of `geonames-retrieve'."
  (let ((buf (url-retrieve-synchronously url)))
    (unless buf (error "Unable to retrieve data"))
    (switch-to-buffer buf)
    (geonames~retrieve-handle t url nil no-cache)))

(defun geonames-search-url (parameters &optional no-add-user)
  "Create url for geonames search query.
PARAMETERS is a list of key value pairs which are added to the geonames url.
If NO-ADD-USER is t or PARAMETERS contains a username field or
`geonames-username' is nil then no \"username\" parameter is automatically
added.
See http://www.geonames.org/export/geonames-search.html for more information.
Example: (geonames-search '((q . \"foo\") (lang . \"de\")))"
  (let* ((no-add-user (null geonames-username))
         (url (loop with url = (concat "http://" geonames-url "/searchJSON?")
                    for par in parameters
                    when (eq (car parameters) 'username) do (setq no-add-user t)
                    do (setq url (concat url
                                         (url-hexify-string (symbol-name (car par)))
                                         "="
                                         (url-hexify-string (cdr par))
                                         "&"))
                    finally return url)))
    (unless no-add-user
      (setq url (concat url "username=" (url-hexify-string geonames-username))))
    url))

(defcustom geonames-search-buffer-name "*Geonames Results*"
  "Name for the buffer to display the `geonames-search' results."
  :group 'geonames
  :type 'string)

(defvar dbg-geonames-json nil)

(defun geonames-search (q)
  "Search geonames for any attribute containing Q.
This is a simple search function for user interaction.
This uses geonames search with the q parameter.
See http://www.geonames.org/export/geonames-search.html for more information."
  (interactive "sSearch on Geonames for: ")
  (switch-to-buffer geonames-search-buffer-name)
  (goto-char (point-max))
  (insert (format "\nGeonames search: %s\n" q))
  (geonames-retrieve
   (geonames-search-url (list (cons 'q q)))
   (lambda (url json)
     (switch-to-buffer geonames-search-buffer-name)
     (goto-char (point-max))
     ; (insert (format "JSON: %s\n" json))
     (when (assoc 'status json)
       ;; See http://www.geonames.org/export/webservice-exception.html
       (let* ((status (cdr (assoc 'status json)))
              (msg (format "Geonames Error %s: %s\n"
                           (cdr (assoc 'value status))
                           (cdr (assoc 'message status)))))
         (insert msg)
         (error msg)))

     (setq dbg-geonames-json json))))


(provide 'geonames)

;;; geonames.el ends here
