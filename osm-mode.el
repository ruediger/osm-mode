;;; osm-mode.el --- OpenStreetMap (OSM) support for Emacs

;; Copyright (C) 2012 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; This file is NOT part of GNU Emacs.

;; osm-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; osm-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with osm-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 

;;; Code:

(defun osm-latlon2tilexy (lat lon zoom)
  "Convert LAT/LON with ZOOM into tilexy format required by OSM tile server."
  (let ((calc-angle-mode 'deg))
    (cons
     (calc-eval "floor( (2**$$) * ($ + 180)/360 )" 'num lon zoom)
     (calc-eval "floor( (2**$$) * (1 - (log(tan($) + sec($)) / evalv(π))) / 2 )" 'num lat zoom))))

(provide 'osm-mode)

;;; osm-mode.el ends here
