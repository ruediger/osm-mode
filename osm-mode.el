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

(defgroup osm-mode nil
  "OpenStreetMap."
  :group 'comm)

(defcustom osm-key ""
  "API key. Only if required by server (CloudMade)."
  :group 'osm-mode
  :type 'string)

(defcustom osm-server-type 'mapnik
  "Server type to use. See `osm-server'."
  :group 'osm-mode
  :type '(choice (const :tag "Mapnik (default)" mapnik)
                 (const :tag "Cycle" cycle)
                 (const :tag "Transport" transport)
                 (const :tag "CloudMade (Web)" cloudmade1)
                 (const :tag "CloudMade (Fine Line)" cloudmade2)
                 (const :tag "CloudMade (NoName)" cloudmade3)
                 (const :tag "MapQuest" mapquest)
                 (const :tag "MapQuest Open Aerial" mapquestoa)
                 (const :tag "Migurski's Terrain" migurski)))

(defcustom osm-default-zoom 12
  "Default zoom level.  Values are 0-16 but support depends on server type."
  :group 'osm-mode
  :type 'integer)

(defcustom osm-expire-time 604800
  "Cache expire time in seconds.  This should be more than 7 days (604800s)!  Read https://wiki.openstreetmap.org/wiki/Tile_usage_policy before changing it!"
  :group 'osm-mode
  :type 'integer)

(defvar osm-params nil
  "Buffer local OSM parameters.")
(make-variable-buffer-local 'osm-params)

(defun osm-params-put (prop val)
  (setq osm-params (plist-put osm-params prop val)))

(defun osm-set-zoom (zoom)
  (interactive "NZoom:")
  (osm-params-put :zoom zoom)
  (osm-reload)
  zoom)
(defun osm-get-zoom ()
  (or (plist-get osm-params :zoom)
      osm-default-zoom))
(defun osm-zoom+ ()
  (interactive)
  (osm-set-zoom (1+ (osm-get-zoom ))))
(defun osm-zoom- ()
  (interactive)
  (osm-set-zoom (1- (osm-get-zoom ))))

(define-derived-mode osm-mode fundamental-mode "osm-mode"
  "OpenStreetMap mode"
  :group 'osm-mode
  (osm-params-put :zoom (osm-get-zoom))) ; initializes zoom with default zoom if unset

;; See https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
(defvar osm-server '((mapnik . ("http://a.tile.openstreetmap.org/%zoom%/%x%/%y%.png"
                                "http://b.tile.openstreetmap.org/%zoom%/%x%/%y%.png"
                                "http://c.tile.openstreetmap.org/%zoom%/%x%/%y%.png"))
                     (cycle . ("http://a.tile.opencyclemap.org/cycle/%zoom%/%x/%y%.png"
                               "http://b.tile.opencyclemap.org/cycle/%zoom%/%x/%y%.png"
                               "http://c.tile.opencyclemap.org/cycle/%zoom%/%x/%y%.png"))
                     (transport . ("http://a.tile2.opencyclemap.org/transport/%zoom%/%x%/%y%.png"
                                   "http://b.tile2.opencyclemap.org/transport/%zoom%/%x%/%y%.png"
                                   "http://c.tile2.opencyclemap.org/transport/%zoom%/%x%/%y%.png"))
                     (cloudmade1 . ("http://a.tile.cloudmade.com/%key%/1/256/%zoom%/%x%/%y%.png"
                                    "http://b.tile.cloudmade.com/%key%/1/256/%zoom%/%x%/%y%.png"
                                    "http://c.tile.cloudmade.com/%key%/1/256/%zoom%/%x%/%y%.png"))
                     (cloudmade2 . ("http://a.tile.cloudmade.com/%key%/2/256/%zoom%/%x%/%y%.png"
                                    "http://b.tile.cloudmade.com/%key%/2/256/%zoom%/%x%/%y%.png"
                                    "http://c.tile.cloudmade.com/%key%/2/256/%zoom%/%x%/%y%.png"))
                     (cloudmade3 . ("http://a.tile.cloudmade.com/%key%/3/256/%zoom%/%x%/%y%.png"
                                    "http://b.tile.cloudmade.com/%key%/3/256/%zoom%/%x%/%y%.png"
                                    "http://c.tile.cloudmade.com/%key%/3/256/%zoom%/%x%/%y%.png"))
                     (mapquest . ("http://otile1.mqcdn.com/tiles/1.0.0/osm/%zoom%/%x%/%y%.png"
                                  "http://otile2.mqcdn.com/tiles/1.0.0/osm/%zoom%/%x%/%y%.png"
                                  "http://otile3.mqcdn.com/tiles/1.0.0/osm/%zoom%/%x%/%y%.png"
                                  "http://otile4.mqcdn.com/tiles/1.0.0/osm/%zoom%/%x%/%y%.png"))
                     (mapquestoa . ("http://oatile1.mqcdn.com/tiles/1.0.0/osm/%zoom%/%x%/%y%.png"
                                    "http://oatile2.mqcdn.com/tiles/1.0.0/osm/%zoom%/%x%/%y%.png"
                                    "http://oatile3.mqcdn.com/tiles/1.0.0/osm/%zoom%/%x%/%y%.png"
                                    "http://oatile4.mqcdn.com/tiles/1.0.0/osm/%zoom%/%x%/%y%.png"))
                     (migurski . ("http://tile.stamen.com/terrain-background/%zoom%/%x%/%y%.jpg")))
  "List of OSM Tile servers. Replaces %zoom%, %x%, %y% with the tile parameters and %key% if with the API key required.")

(defun osm-latlon2tilexy (lat lon zoom)
  "Convert LAT/LON with ZOOM into tilexy format required by OSM tile server."
  (let ((calc-angle-mode 'deg))
    (cons
     (calc-eval "floor( (2**$$) * ($ + 180)/360 )" 'num lon zoom)
     (calc-eval "floor( (2**$$) * (1 - (log(tan($) + sec($)) / evalv(π))) / 2 )" 'num lat zoom))))

(defun osm-rand-nth (list)
  "Return random element of LIST."
  (nth (random (length list)) list))

(defun osm-replace-var (var val string)
  "Replace VAR with VAL in STRING."
  (replace-regexp-in-string (regexp-quote (concat "%" (symbol-name var) "%")) val string))

(defun osm-url (lat lon zoom &optional server-type)
  "Create URL for OpenStreetMap server."
  (let ((tilexy (osm-latlon2tilexy lat lon zoom))
        (server (osm-rand-nth (cdr (assoc (or server-type osm-server-type) osm-server)))))
    (osm-replace-var 'key osm-key
                     (osm-replace-var 'zoom (number-to-string zoom)
                                      (osm-replace-var 'y (cdr tilexy)
                                                       (osm-replace-var 'x (car tilexy) server))))))

(defvar osm-buffer-name "*OSM*" "osm-mode buffer.")
(defun osm-get-buffer ()
  (with-current-buffer (get-buffer-create osm-buffer-name)
    (goto-char (point-min))
    (delete-region (point-min) (point-max))
    (osm-mode)
    (current-buffer)))

(defun osm-handle-data (url lat lon zoom server-type &optional no-cache)
  "Handle data in current buffer."
  (goto-char (point-min))
  (unless (search-forward "\n\n" nil t)
    (kill-buffer)
    (error "Error in http reply."))
  (let ((headers (buffer-substring (point-min) (point)))
        (data (buffer-substring (point) (point-max))))
    (unless (string-match-p "^HTTP/1.1 200 OK" headers)
      (kill-buffer)
      (error "Unable to fetch data."))
    (unless no-cache
      (url-store-in-cache (current-buffer)))
    (kill-buffer)

    (with-current-buffer (osm-get-buffer)
      ;; TODO real image placement (several tiles!)
      (setq buffer-read-only nil)
      (osm-params-put :zoom zoom)
      (osm-params-put :lat lat)
      (osm-params-put :lon lon)
      (osm-params-put :server-type server-type)
      (insert-image (create-image data 'png t))
      (insert "\n\nData provided by the OpenStreetMap project. Licensed: CC BY-SA.\n")
      (setq buffer-read-only t)
      (switch-to-buffer (current-buffer)))))

(require 'url-cache)

(defun osm-cache-fetch (url lat lon zoom server-type)
  "Fetch URL from the url package cache."
  (with-temp-buffer
    (url-cache-extract (url-cache-create-filename url))
    (osm-handle-data url lat lon zoom server-type)))

(defun osm-retrieve-tile (lat lon zoom &optional server-type expire-time)
  (let ((url (osm-url lat lon zoom server-type)))
    (if (url-cache-expired url (or expire-time osm-expire-time))
        (url-retrieve url
                      (lambda (status url lat lon zoom server-type)
                        (osm-handle-data url lat lon zoom server-type))
                      (list url lat lon zoom server-type))
      (osm-cache-fetch url lat lon zoom server-type))))

;;;###autoload
(defun osm-show (lat lon &optional zoom server-type)
  (interactive "nLat:\nnLong:\nn")
  (osm-retrieve-tile lat lon (or zoom (osm-get-zoom)) server-type))

(defun osm-reload ()
  "Reload current view."
  (osm-show
   (plist-get osm-params :lat)
   (plist-get osm-params :lon)
   (osm-get-zoom)
   (plist-get osm-params :server-type)))

(provide 'osm-mode)

;;; osm-mode.el ends here
