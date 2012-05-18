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

(hunchentoot:define-easy-handler (standard-present :uri "/present.html") (hash)
  (multiple-value-bind (object present) (gethash hash presentable-objects)
    (if present
        (with-scaffold (stream :title (description-string object))
          (present-html object stream))
        (error "Object with hash ~A not found." (escape-string hash)))))

(defmacro with-scaffold ((stream-var &key (title "Presenting ...")) &body body)
  `(with-html-output-to-string (,stream-var nil :prologue t :indent t)
     (:html
      (:head
       (:title ,title))
      (:body
       (:h1 ,title)
       (:p :style "color: red;" "This site is still in heavy development.")
       ,@body))))

(defgeneric description-string (object))

(defmethod description-string (object)
  "Presenting ....")

(defgeneric present-html (object stream))

(defmacro define-html-presentation ((class) &body body)
  `(defmethod present-html ((,class ,class) stream)
     (with-html-output (stream)
       ,@body)))

(defclass/f image ()
  (file-hash
   original-file
   thumbnail))

(define-html-presentation (image)
  (:h2 (pathname-name (original-file image)))
  (:img :src (format nil "/present.jpg?hash=~A&size=thumb" (image-folders:hash-format (file-hash image)))))

(hunchentoot:define-easy-handler (image-present :uri "/present.jpg") (hash size)
  (when (string= size "thumb")
    (multiple-value-bind (image present) (gethash hash presentable-objects)
      (when (and present (typep image 'image))
        (hunchentoot:handle-static-file (thumbnail image))))))

(defmethod description-string ((image image))
  "Presenting image ...")

(defun path->image (path)
  (make-instance 'image
                 :file-hash (image-folders:file-hash path)
                 :original-file path
                 :thumbnail (image-folders:scaled-filename path 100)))

(defun load-images-from (folder)
  (mapc (compose (lambda (image)
                   (setf (gethash (image-folders:hash-format (file-hash image))
                                  presentable-objects)
                         image))
                 #'path->image)
        (image-folders:images-in-folder folder)))
