;;; surf.el --- Track surf sessions and conditions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Given a starting and ending time, gathers surf data intended for use in a
;;; logbook detailing sessions and conditions at different breaks.
;;; Intended to be used in an org-capture template.
;;; Requires selecting the nearest NOAA station and NDBC buoy.
;;; TODO: manage multiple profiles of station and buoy pairs to easily switch
;;; between for use when travelling.
;;; Code:

(defgroup surf nil
  "Track surf sessions and conditions.")

(defcustom surf-noaa-station-id nil
  "NOAA station id (wind and tides)."
  :type 'string
  :group 'surf)

(defcustom surf-ndbc-station-id nil
  "NDBC station id (buoy for swell data)."
  :type 'string
  :group 'surf)

(defun meter-to-ft (m)
"Convert M meters to ft."
  (* (string-to-number m) 3.28084))

(defun degree-to-direction (d)
"Convert decimal degree D to major-minor cardinal direction format."
  (let ((nd (mod (+ (string-to-number d) 180) 360)))
    (cond ((< 348 nd)
           "N")
          ((< 326 nd)
           "NNW")
          ((< 303 nd)
           "NW")
          ((< 281 nd)
           "WNW")
          ((< 258 nd)
           "W")
          ((< 236 nd)
           "WSW")
          ((< 213 nd)
           "SW")
          ((< 191 nd)
           "SSW")
          ((< 168 nd)
           "S")
          ((< 146 nd)
           "SSE")
          ((< 123 nd)
           "SE")
          ((< 101 nd)
           "ESE")
          ((< 78 nd)
           "E")
          ((< 56 nd)
           "ENE")
          ((< 33 nd)
           "NE")
          ((< 11 nd)
           "NNE")
          ((< 0 nd)
           "N"))))

(defun walk (node)
"Parse NOAA wave data by recursively visiting relevant nodes given root NODE."
  (when (listp node)
    (cond ((string= "SignificantWaveHeight" (xml-get-attribute node 'name))
          (setq wh `(("overall-wave-height" .
                      ,(meter-to-ft (car (xml-node-children node) ))))))
          ((string= "DominantWavePeriod" (xml-get-attribute node 'name))
          (setq wp `(("overall-wave-period" . ,(car (xml-node-children node) )))))
          ((string= "SwellHeight" (xml-get-attribute node 'name))
          (setq sh `(("swell-height" .
                      ,(meter-to-ft (car (xml-node-children node) ))))))
          ((string= "SwellPeriod" (xml-get-attribute node 'name))
          (setq sp `(("swell-period" . ,(car (xml-node-children node) )))))
          ((string= "SwellWaveDirection" (xml-get-attribute node 'name))
          (setq sd `(("swell-direction" .
                      ,(degree-to-direction (car (xml-node-children node) ))))))
          (t (mapc 'walk (xml-node-children node))))))

(defun parse-wave-data (data)
"Wrapper to return wave stats given response DATA."
  (let (wh wp sh sp sd)
    (walk (car data))
    (append wh wp sh sp sd)))

(defun parse-tide-data (data)
"Parse tide DATA."
  (let* ((tide-array (cdr (assoc 'predictions data)))
         (l (- (length tide-array) 1))
         (t0 (cdr (assoc 'v (aref tide-array 0))))
         (t1 (cdr (assoc 'v (aref tide-array l)))))
    `(("tide-start" . ,(concat t0 " ft")) ("tide-end" . ,(concat t1 " ft")))))

(defun parse-wind-data (data)
"Parse wind DATA."
  (let* ((wind-array (cdr (assoc 'data (cdr data))))
         (l (- (length wind-array) 1))
         (k0 (cdr (assoc 's (aref wind-array 0))))
         (d0 (cdr (assoc 'dr (aref wind-array 0))))
         (k1 (cdr (assoc 's (aref wind-array l))))
         (d1 (cdr (assoc 'dr (aref wind-array l)))))
    `(("wind-knots-start" . ,(string-to-number k0))
      ("wind-direction-start" . ,d0)
      ("wind-knots-end" . ,(string-to-number k1))
      ("wind-direction-end" . ,d1))))

(defun get-tide-data (start end)
"Send request for tide data at START and END time."
  (request
    "https://tidesandcurrents.noaa.gov/api/datagetter"
    :type "GET"
    :params `(("begin_date" . ,start)
            ("end_date" . ,end)
            ("station" . "9410840")
            ("format" . "json")
            ("product" . "predictions")
            ("time_zone" . "lst_ldt")
            ("units" . "english")
            ("datum" . "mllw")
            )
    :parser 'json-read
    :sync t))

(defun get-wind-data (start end)
"Send request for wind data at START and END time."
  (request
    "https://tidesandcurrents.noaa.gov/api/datagetter"
    :type "GET"
    :params `(("begin_date" . ,start)
            ("end_date" . ,end)
            ("station" . "9410840")
            ("format" . "json")
            ("product" . "wind")
            ("time_zone" . "lst_ldt")
            ("units" . "english")
            )
    :parser 'json-read
    :sync t))

(defun get-wave-data (start)
"Send request for wave data at START time."
  (request
    "https://sdf.ndbc.noaa.gov/sos/server.php"
    :type "GET"
    :params `(("request" . "GetObservation")
            ("eventtime" . ,start)
            ("offering" . "urn:ioos:station:wmo:46221")
            ("service" . "SOS")
            ("observedproperty" . "waves")
            ("responseformat" . "text/xml;schema=\"ioos/0.6.1\"")
            ("version" . "1.0.0")
            )
   :parser (lambda () (xml-parse-region (point-min) (point-max)))
   :sync t))


(defun format-date-for-ndbc (time)
  "Return formatted time string for ndbc api from TIME."
  (let* ((minutes (string-to-number (format-time-string "%M" time)))
         (round-minutes (if (> 30 minutes)
                            "00" "30"))
        (time-str-front (format-time-string "%Y-%m-%dT%H:" (time-subtract time (car (current-time-zone)))))
        (time-str-end (format "%s:00Z" round-minutes)))
    (concat time-str-front time-str-end)))

(defun format-date-for-noaa (time)
  "Return formatted time string for noaa api from TIME."
  (format-time-string "%Y%m%d %H:%M" time))

(defun format-date-display (time)
  "Return formatted time string for noaa api from TIME."
  (format-time-string "%a, %B %d %I:%M%p" time))

(defun add-property (cell)
    "Create org property from key and value in CELL."
  (let ((prop (upcase (car cell)))
        (val (if (number-or-marker-p (cdr cell))
                 (number-to-string (cdr cell))
               (cdr cell))))
    (format ":%s: %s" prop val)))

(defun surf-session-template ()
  "Gather user input and build surf session template."
  (let* ((start-date (org-read-date t t nil "Session start: "))
         (length (string-to-number (read-from-minibuffer "Session length: ")))
         (end-date (time-add (* length 3600) start-date))
         (tide (parse-tide-data (request-response-data (get-tide-data
                 (format-date-for-noaa start-date)
                 (format-date-for-noaa end-date)))))
         (winds (parse-wind-data (request-response-data (get-wind-data
                 (format-date-for-noaa start-date)
                 (format-date-for-noaa end-date)))))
         (waves (parse-wave-data (request-response-data (get-wave-data
                 (format-date-for-ndbc start-date))))))
         (mapconcat 'identity
                   `(
                     ,(concat "* " (format-date-display start-date))
                     ":PROPERTIES:"
                     ,(concat ":START-TIME: " (format-time-string "%Y-%m-%dT%T" start-date))
                     ,(concat ":END-TIME: " (format-time-string "%Y-%m-%dT%T" end-date))
                     ,(mapconcat 'add-property tide "\n")
                     ,(mapconcat 'add-property winds "\n")
                     ,(mapconcat 'add-property waves "\n")
                     ":END:"
                     "%^{Break}p %^{Rating}p"
                     "** Summary\n %?"
                     )
                   "\n")))

(provide 'surf)
;;; surf.el ends here
