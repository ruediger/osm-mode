;;; org-osm --- Support for OSM in org-mode

;; Copyright (C) 2012 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; This file is NOT part of GNU Emacs.

;; org-osm.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; org-osm.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with osm-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is part of osm-mode.

;;; Code:

(require 'org)
(require 'osm-mode)

(defun osm~org-store-link ()
  "Store a link to the current location."
  (when (eq major-mode 'osm-mode)
    (let* ((lat (plist-get osm-params :lat))
           (lon (plist-get osm-params :lon))
           (zoom (plist-get osm-params :zoom))
           (latlon (format "%s;%s" lat lon))
           (link (format "osm:%s;%s" latlon zoom))
           (descr (concat "Visit " latlon " with OpenStreetMap")))
      (org-store-link-props
       :type "osm"
       :link link
       :description descr))))

(defun osm-org-open (link)
  "Open osm type LINK."
  (let ((latlonzoom (split-string link ?\;)))
    (osm-show (car latlonzoom) (cadr latlonzoom) (caddr latlonzoom))))

(org-add-link-type "osm" 'osm-org-open)
(add-hook 'org-store-link-functions 'osm~org-store-link)

(provide 'org-osm)

;;; org-osm.el ends here
