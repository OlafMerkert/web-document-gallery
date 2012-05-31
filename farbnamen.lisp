(defpackage :farbnamen
  (:use :cl :ol
        :parenscript) ; todo fix symbol collisions
  (:export
   :colour->html
   :html->colour))

(in-package :farbnamen)

(setf *js-string-delimiter* #\")

(defparameter farbnamen-imagefolder
  #P "/home/olaf/farbnamen/")

(defvar uploaded-images
  (make-hash-table :test 'equal))

(defun load-images-from-folder (folder)
  (dolist (image-path (image-folders:images-in-folder folder))
    (let ((parts (split-sequence:split-sequence #\. (pathname-name image-path))))
      (when (equal (second parts) "scaled-1024")
        (setf (gethash (first parts) uploaded-images)
              (make-instance 'colour-image
                             :file-hash (first parts)
                             :preview image-path))))))

;;; provide a page to upload images for colour extraction
(hunchentoot:define-easy-handler (upload-image :uri "/farben/bild-hochladen.html")
    ()
  (web-elements:with-scaffold (stream :title "Farbanalyse - Bild hochladen")
    ;; todo Anwendung beschreiben
    (:form
     :action "/farben/process-bild-hochladen.html"
     :method "post"
     :enctype "multipart/form-data"

     (:table
      (:tr
       (:td (:label :for "image-source" "Zu analysierende Bilddatei:"))
       (:td (:input :type "file" :name "image-source" :size "50")))
     
      (:tr
       (:td (:label :for "api-key" "Kennwort: "))
       (:td (:input :type "text" :name "api-key" :value "")))
     
      (:tr
       (:td)
       (:td (:input :type "submit" :value "Bild hochladen")))))))

(defun file-extension (source)
  (keyw (string-upcase (last1 (split-sequence:split-sequence #\/ (third source))))))

(hunchentoot:define-easy-handler (process-upload-image :uri "/farben/process-bild-hochladen.html")
    (image-source api-key)
  ;; todo check api-key for validity
  (let ((image-folders:thumb-dir farbnamen-imagefolder)
        (image-path (first image-source))) ; todo error handling if no file supplied
    (file-hashes:with-hash (hash image-path)
      (let ((thumb-fname
             (image-folders:scaled-filename hash image-folders:preview-size)))
        (unless (com.gigamonkeys.pathnames:file-exists-p thumb-fname)
          (image-folders:create-scaled-versions image-path (file-extension image-source) image-folders:preview-size))
        (setf (gethash hash uploaded-images)
              (make-instance 'colour-image
                             :file-hash hash
                             :preview thumb-fname))
        (hunchentoot:redirect (web-elements:uri "/farben/bild-betrachten.html" :hash hash))))))

(hunchentoot:define-easy-handler (view-image :uri "/farben/bild-betrachten.html") (hash)
  ;; todo check for presence of image
    (web-elements:with-scaffold (stream :title "Farbe aus dem Bild wählen."
                                        :script analyse-script)
      (:form :class "farbemitteln"
             (:label :for "radius" "Mitteln über Radius (in px):")
             (:select :name "radius"
                      (:option "5")
                      (:option "10")
                      (:option "20")
                      (:option "50")))
      (:div :class "hauptbild"
            (:img :id "clickonit"
                  :src (web-elements:uri "/farben/bild-betrachten.jpg" :hash hash)))))

(defmacro+ps $$ ((selector event-binding &optional event) &body body)
  `((@ ($ ,selector) ,event-binding) (lambda ,(if event (list event)) ,@body)))


(defparameter analyse-script
  (ps ($$ (document ready)
        ($$ ("#clickonit" click e)
          (alert (concatenate 'string (@ e |pageX|) ", " (@ e |pageY|)))))))

(hunchentoot:define-easy-handler (view-image-jpg :uri "/farben/bild-betrachten.jpg") (hash)
  (let ((web-elements:presentable-objects uploaded-images))
    (web-elements:image-present :hash hash :size "preview")))

(defclass/f colour-image ()
  (web-elements:file-hash
   web-elements:preview))

(defmethod web-elements:image-p ((colour-image colour-image))
  t)

(defun parse-integer/web (string &optional (default 0))
  "Parse a positive integer from a STRING, after stripping all
non-digits from it.  If STRING is null or contains no digits, return
DEFAULT."
  ;; return 0 if string is nil
  (if (null string) default
      ;; strip out anything not a digit.
      (let ((digit-string (remove-if-not #'digit-char-p string)))
        (if (zerop (length digit-string))
            default
            (parse-integer digit-string)))))

(hunchentoot:define-easy-handler (analyse-colour :uri "/farben/bild-analyse.html")
    (hash x y radius)
  #d
  (setf x (parse-integer/web x)
        y (parse-integer/web y)
        radius (parse-integer/web radius 10))
  (multiple-value-bind (image present) (gethash hash uploaded-images)
    ;; todo error message if not present
    (let* ((mean-colour (mean-colour (web-elements:preview image) x y radius))
           (next-colours (next-colours mean-colour))
           (mean-farbe (list "Gemessen" (colour->html mean-colour) mean-colour)))
      
      (cl-who:with-html-output-to-string (stream nil :indent t)
        (:table :class "farbtabelle"
                (dolist (farbe (list* mean-farbe next-colours))
                  (dbug "~A" farbe)
                  (cl-who:htm
                   (:tr
                    (:td (cl-who:str (farbe-name farbe)))
                    (:td :style (format nil "width: 100px; background-color: ~A" (hex-code farbe)))))))))))

;;; open an image file and look at the colours around a point
(defun mean-colour (image-path x0 y0 radius)
  "Open the image and compute the mean colour in the circle
  around (X0,Y0) with given RADIUS.  Pixels \"not on the image\" are
  simply ignored.  The result will be a vector of length 3."
  (cl-gd:with-image-from-file* (image-path)
    (let ((w (cl-gd:image-width))
          (h (cl-gd:image-height))
          (valid-pixels 0)
          (red 0) (blue 0) (green 0)
          (radius2 (expt radius 2)))
      (loop for y from (- y0 radius) to (+ y0 radius) do
           (loop for x from (- x0 radius) to (+ x0 radius) do
                (when (and (< -1 x  w)
                           (< -1 y h)
                           (<= (+ (expt (- x x0) 2)
                                  (expt (- y y0) 2))
                               radius2))
                  (let ((color (cl-gd:get-pixel x y)))
                    (incf red (cl-gd:color-component :red color))
                    (incf blue (cl-gd:color-component :blue color))
                    (incf green (cl-gd:color-component :green color))
                    (incf valid-pixels)))))
      (if (zerop valid-pixels)
          #(0 0 0) ; todo maybe handle this case separately
          (vector (floor red valid-pixels)
               (floor green valid-pixels)
               (floor blue valid-pixels))))))

(defun euclidean-distance (a b)
  "Compute the euclidean distance of two sequences."
  (sqrt
   (reduce #'+
           (map 'vector (lambda (x y) (expt (- x y) 2)) a b))))

(bind-multi ((name red green blue)
             (part 0 1 2))
  (defun name (c)
    (aref c part)))

(defun rgb-metric (a b)
  "Colour metric from http://www.compuphase.com/cmetric.htm"
  (let ((rbar (/ (+ (red a) (red b))))
        (dred (expt (- (red a) (red b)) 2))
        (dgreen (expt (- (green a) (green b)) 2))
        (dblue (expt (- (blue a) (blue b)) 2)))
    (sqrt (+
           (* (+ 2 (/ rbar 256))
              dred)
           (* 4 dgreen)
           (* (+ 2 (/ (- 255 rbar) 256))
              dblue)))))

(defun next-colours (colour &optional (N 5) (distance #'rgb-metric))
  "Find the N colours closest to COLOUR in FARBENTABELLE, according to the euclidean distance of the colours."
  (subseq
   (sort (copy-list farbentabelle)
         #'<
         :key (lambda (x)
                (funcall distance colour (rgb-value x))))
   0 n))

;;; give a table full of the known colour names and their rgb values
(defstruct (farbe :conc-name (:type list))
  farbe-name hex-code rgb-value)

(defun table-entries (farbe)
  "Add a vector representing the colour to the FARBE, by extracting
the information from the html hex colour code."
  (append1 farbe
           (html->colour (hex-code farbe))))

(defun colour->html (colour)
  (mkstr #\# (file-hashes:hash-format colour)))

(defun html->colour (html)
  (vector (parse-integer html :start 1 :end 3 :radix 16)
          (parse-integer html :start 3 :end 5 :radix 16)
          (parse-integer html :start 5 :end 7 :radix 16)))

(defparameter farbentabelle
  (mapcar
   #'table-entries
   '(("black"                "#000000")	
     ("maroon"               "#800000")	
     ("green"                "#008000")	
     ("olive"                "#808000")	
     ("navy"                 "#000080")	
     ("purple"               "#800080")	
     ("teal"                 "#008080")	
     ("silver"               "#C0C0C0")
     ("gray"                 "#808080")
     ("red"                  "#FF0000")
     ("lime"                 "#00FF00")
     ("yellow"               "#FFFF00")
     ("blue"                 "#0000FF")
     ("fuchsia"              "#FF00FF")
     ("aqua"                 "#00FFFF")
     ("white"                "#FFFFFF")
     ("aliceblue"            "#F0F8FF")
     ("antiquewhite"         "#FAEBD7")
     ("aquamarine"           "#7FFFD4")
     ("azure"                "#F0FFFF")
     ("beige"                "#F5F5DC")
     ("blueviolet"           "#8A2BE2")
     ("brown"                "#A52A2A")
     ("burlywood"            "#DEB887")
     ("cadetblue"            "#5F9EA0")
     ("chartreuse"           "#7FFF00")
     ("chocolate"            "#D2691E")
     ("coral"                "#FF7F50")
     ("cornflowerblue"       "#6495ED")
     ("cornsilk"             "#FFF8DC")
     ("crimson"              "#DC143C")
     ("darkblue"             "#00008B")
     ("darkcyan"             "#008B8B")
     ("darkgoldenrod"        "#B8860B")
     ("darkgray"             "#A9A9A9")
     ("darkgreen"            "#006400")
     ("darkkhaki"            "#BDB76B")
     ("darkmagenta"          "#8B008B")
     ("darkolivegreen"       "#556B2F")
     ("darkorange"           "#FF8C00")
     ("darkorchid"           "#9932CC")
     ("darkred"              "#8B0000")
     ("darksalmon"           "#E9967A")
     ("darkseagreen"         "#8FBC8F")
     ("darkslateblue"        "#483D8B")
     ("darkslategray"        "#2F4F4F")
     ("darkturquoise"        "#00CED1")
     ("darkviolet"           "#9400D3")
     ("deeppink"             "#FF1493")
     ("deepskyblue"          "#00BFFF")
     ("dimgray"              "#696969")
     ("dodgerblue"           "#1E90FF")
     ("firebrick"            "#B22222")
     ("floralwhite"          "#FFFAF0")
     ("forestgreen"          "#228B22")
     ("gainsboro"            "#DCDCDC")
     ("ghostwhite"           "#F8F8FF")
     ("gold"                 "#FFD700")
     ("goldenrod"            "#DAA520")
     ("greenyellow"          "#ADFF2F")
     ("honeydew"             "#F0FFF0")
     ("hotpink"              "#FF69B4")
     ("indianred"            "#CD5C5C")
     ("indigo"               "#4B0082")
     ("ivory"                "#FFFFF0")
     ("khaki"                "#F0E68C")
     ("lavender"             "#E6E6FA")
     ("lavenderblush"        "#FFF0F5")
     ("lawngreen"            "#7CFC00")
     ("lemonchiffon"         "#FFFACD")
     ("lightblue"            "#ADD8E6")
     ("lightcoral"           "#F08080")
     ("lightcyan"            "#E0FFFF")
     ("lightgoldenrodyellow" "#FAFAD2")
     ("lightgreen"           "#90EE90")
     ("lightgrey"            "#D3D3D3")
     ("lightpink"            "#FFB6C1")
     ("lightsalmon"          "#FFA07A")
     ("lightseagreen"        "#20B2AA")
     ("lightskyblue"         "#87CEFA")
     ("lightslategray"       "#778899")
     ("lightsteelblue"       "#B0C4DE")
     ("lightyellow"          "#FFFFE0")
     ("limegreen"            "#32CD32")
     ("linen"                "#FAF0E6")
     ("mediumaquamarine"     "#66CDAA")
     ("mediumblue"           "#0000CD")
     ("mediumorchid"         "#BA55D3")
     ("mediumpurple"         "#9370DB")
     ("mediumseagreen"       "#3CB371")
     ("mediumslateblue"      "#7B68EE")
     ("mediumspringgreen"    "#00FA9A")
     ("mediumturquoise"      "#48D1CC")
     ("mediumvioletred"      "#C71585")
     ("midnightblue"         "#191970")
     ("mintcream"            "#F5FFFA")
     ("mistyrose"            "#FFE4E1")
     ("moccasin"             "#FFE4B5")
     ("navajowhite"          "#FFDEAD")
     ("oldlace"              "#FDF5E6")
     ("olivedrab"            "#6B8E23")
     ("orange"               "#FFA500")
     ("orangered"            "#FF4500")
     ("orchid"               "#DA70D6")
     ("palegoldenrod"        "#EEE8AA")
     ("palegreen"            "#98FB98")
     ("paleturquoise"        "#AFEEEE")
     ("palevioletred"        "#DB7093")
     ("papayawhip"           "#FFEFD5")
     ("peachpuff"            "#FFDAB9")
     ("peru"                 "#CD853F")
     ("pink"                 "#FFC0CB")
     ("plum"                 "#DDA0DD")
     ("powderblue"           "#B0E0E6")
     ("rosybrown"            "#BC8F8F")
     ("royalblue"            "#4169E1")
     ("saddlebrown"          "#8B4513")
     ("salmon"               "#FA8072")
     ("sandybrown"           "#F4A460")
     ("seagreen"             "#2E8B57")
     ("seashell"             "#FFF5EE")
     ("sienna"               "#A0522D")
     ("skyblue"              "#87CEEB")
     ("slateblue"            "#6A5ACD")
     ("slategray"            "#708090")
     ("snow"                 "#FFFAFA")
     ("springgreen"          "#00FF7F")
     ("steelblue"            "#4682B4")
     ("tan"                  "#D2B48C")
     ("thistle"              "#D8BFD8")
     ("tomato"               "#FF6347")
     ("turquoise"            "#40E0D0")
     ("violet"               "#EE82EE")
     ("wheat"                "#F5DEB3")
     ("whitesmoke"           "#F5F5F5")
     ("yellowgreen"          "#9ACD32"))))