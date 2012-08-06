(defpackage :web-elements
  (:use :cl :ol :who
        :web-utils)
  (:export
   :start-server
   :stream
   :define-html-presentation
   :description-string
   :canonical-url
   :present-html
   :present-file
   :thumb
   :preview
   :original
   :hashed-objects
   :named-objects
   :web-present-html
   :web-present-file
   :error-code
   :uri
   :with-scaffold))

(in-package :web-elements)

;;; general web related stuff


(eval-when (:load-toplevel :execute)
  (setup-static-content
   "/present.css"   #P"~/Projekte/web-document-gallery/style.css"
   "/lib/jquery.js" #P"~/Projekte/web-document-gallery/jquery-1.7.2.min.js"))


(defmacro with-scaffold ((&key (title "Presenting ...") script)
                         &body body)
  `(html/document (:title title
                          :style  "/present.css"
                          ,@(when script
                                  :script "/lib/jquery.js"
                                  :script script))
     (:h1 (esc ,title))
     (:p :class "message" "This site is still in heavy development.")
     ,@body))

;;; define interface for the presentable objects
(defgeneric description-string (object)
  (:documentation "Provide a title/short description for the object.
  This is for instance used as a page heading."))

(defgeneric canonical-url (object)
  (:documentation "Give a canonical url by which this object can be
  accessed."))

(defgeneric present-html (object stream)
  (:documentation "Write a html representation to the given stream."))

(defgeneric present-file  (object format)
  (:documentation "Return a pathname to be served."))

(defmacro define-html-presentation ((class) &body body)
  `(defmethod present-html ((,class ,class) stream)
     (with-html-output (stream)
       ,@body)))

(defparameter known-formats
  '(("thumb"    . thumb)
    ("preview"  . preview)
    ("original" . original)))

(defun parse-format (format)
  "Map the string to symbols, to avoid using user sent data too much."
  (assoc1 format known-formats nil :test #'string-equal))

;;; some default behaviour
(defmethod description-string (object)
  "Presenting ....")

;;; main data storage
(defvar hashed-objects
  (make-hash-table :test 'equal :size 400))

(defvar named-objects
  (make-hash-table :test 'equal :size 30))

;;; present to the web.
(hunchentoot:define-easy-handler (web-present-html :uri "/present.html")
    (hash name)
  (ncond object
    ((or (gethash hash hashed-objects)
         (gethash name named-objects))
     (with-scaffold (:title (description-string object))
       (present-html object xml-output-stream)))
    (t
     (error-code))))

(hunchentoot:define-easy-handler (web-present-file :uri "/present.file")
    (hash name format size)
  (ncond object
    ((or (gethash hash hashed-objects)
         (gethash name named-objects))
     (aif (present-file object (or (parse-format format)
                                   (parse-format size)))
          (hunchentoot:handle-static-file it)
          (error-code)))
    (t
     (error-code))))
