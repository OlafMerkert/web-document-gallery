(defpackage :web-elements
  (:use :cl :ol :who)
  (:export
   :start-server
   :stream
   :define-html-presentation))

(in-package :web-elements)

(defun start-server ()
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor :port 8080)))

(defvar presentable-objects
  (make-hash-table :test 'equal :size 400))

(defvar named-objects
  (make-hash-table :test 'equal :size 30))

(defun error-code (&optional (code hunchentoot:+HTTP-NOT-FOUND+))
  (setf (hunchentoot:return-code*) code)
  (hunchentoot:abort-request-handler))

(hunchentoot:define-easy-handler (present-html :uri "/present.html")
    (hash name)
  (ncond object
    ((or (gethash hash presentable-objects)
         (gethash name named-objects))
     (with-scaffold (stream :title (description-string object))
       (present-html object stream)))
    (t
     (error-code))))

(eval-when (:load-toplevel :execute)
  (push (hunchentoot:create-static-file-dispatcher-and-handler
         "/present.css" #P "~/Projekte/web-document-gallery/style.css")
        hunchentoot:*dispatch-table*))

(defmacro with-scaffold ((stream-var &key (title "Presenting ...")) &body body)
  `(with-html-output-to-string (,stream-var nil :prologue t :indent t)
     (:html
      (:head
       (:title (esc ,title))
       (:link :rel "stylesheet" :type "text/css" :href "/present.css"))
      (:body
       (:h1 (esc ,title))
       (:p :class "message" "This site is still in heavy development.")
       ,@body))))

(defun uri (base &rest parameters)
  "Format an url with BASE and a number of PARAMETERS, given like
keyword parameters to a function.  Possibly add global state parameters."
  (format nil "~A~@[?~{~(~A~)=~A~^&~}~]" base parameters))
;; TODO encoding of the parameter values

(defgeneric description-string (object))

(defmethod description-string (object)
  "Presenting ....")

(defgeneric present-html (object stream)
  (:documentation "Write a html representation to the given stream."))

(defgeneric present-file  (object format)
  (:documentation "Return a pathname to be served."))

(defmacro define-html-presentation ((class) &body body)
  `(defmethod present-html ((,class ,class) stream)
     (with-html-output (stream)
       ,@body)))

(defgeneric canonical-url (object))

(defclass/f image ()
  (file-hash
   original-file
   preview
   thumbnail))

(define-html-presentation (image)
  (:div :class "image-preview"
   (:img :src (uri "/present.file" :hash (file-hash image) :size "preview"))))

(defmethod canonical-url ((image image))
  (uri "/present.html" :hash (file-hash image)))

(hunchentoot:define-easy-handler (present-file :uri "/present.file")
    (hash name format size)
  (ncond object
    ((or (gethash hash presentable-objects)
         (gethash name named-objects))
     (aif (present-file object (or (parse-format format)
                                   (parse-format size)))
          (hunchentoot:handle-static-file it)
          (error-code)))
    (t
     (error-code))))

(defparameter known-formats
  '(("thumb"    . 'thumb)
    ("preview"  . 'preview)
    ("original" . 'original)))

(defun parse-format (format)
  (assoc1 format known-formats nil :test #'string-equal))

(defmethod present-file ((image image) (format (eql 'thumb)))
  (thumbnail image))

(defmethod present-file ((image image) (format (eql 'preview)))
  (preview image))

(defmethod present-file ((image image) (format (eql 'original)))
  (original-file image))

(defmethod description-string ((image image))
  (pathname-name (original-file image)))

(defun path->image (path)
  (file-hashes:with-hash (hash path)
    (make-instance 'image
                   :file-hash hash
                   :original-file path
                   :preview (image-folders:scaled-filename hash image-folders:preview-size)
                   :thumbnail (image-folders:scaled-filename hash image-folders:thumb-size))))

(defun folder-name (folder)
  (or (pathname-name folder)
      (last1 (pathname-directory folder))))

(defun load-images-from (folder)
  (setf (gethash (folder-name folder) named-objects)
        (make-instance 'image-folder
                       :name (folder-name folder)
                       :content-list (mapcar
                                      (compose (lambda (image)
                                                 (setf (gethash (file-hash image)
                                                                presentable-objects)
                                                       image))
                                               #'path->image)
                                      (image-folders:images-in-folder folder)))))

(defclass/f image-folder ()
  (name
   content-list))

(defmethod description-string ((image-folder image-folder))
  (name image-folder))

(define-html-presentation (image-folder)
  (:ul :class "imagelist"
   (dolist (image (content-list image-folder))
     (htm
      (:li
       (:a :href (canonical-url image)
           (:img :src (uri "/present.file" :hash  (file-hash image) :size "thumb")
                 :alt (description-string image))))))))

(defmethod canonical-url ((image-folder image-folder))
  (uri "/present.html" :name (name image-folder)))
