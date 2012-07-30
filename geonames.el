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
;; This is part of osm-mode. It is an Emacs interface to
;; http://www.geonames.org/ 

;;; Code:

(defgroup geonames nil
  "GeoNames."
  :group 'osm-mode)

(defcustom geonames-username nil
  "The GeoNames Username: http://www.geonames.org/manageaccount"
  :group 'geonames
  :type 'string)

(defun geonames-retrieve (url handle &optional no-cache)
  "Fetch URL asynchronously and pass the parsed json to HANDLE."
  (url-retrieve url
                (lambda (status url handle no-cache)
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
                  (funcall handle url (json-read))
                  (kill-buffer))
                (list url handle no-cache)))
