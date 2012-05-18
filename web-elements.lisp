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

(defvar named-folders
  (make-hash-table :test 'equal :size 30))

(hunchentoot:define-easy-handler (standard-present :uri "/present.html") (hash)
  (multiple-value-bind (object present) (gethash hash presentable-objects)
    (if present
        (with-scaffold (stream :title  (description-string object))
          (present-html object stream))
        (error "Object with hash ~A not found." (escape-string hash)))))

(hunchentoot:define-easy-handler (folder-present :uri "/present/") (name)
  (multiple-value-bind (folder present) (gethash name named-folders)
    (if present
        (with-scaffold (stream :title (description-string folder))
          (present-html folder stream))
        (error "Folder with name ~A not found." (escape-string name)))))

(defmacro with-scaffold ((stream-var &key (title "Presenting ...")) &body body)
  `(with-html-output-to-string (,stream-var nil :prologue t :indent t)
     (:html
      (:head
       (:title (esc ,title)))
      (:body
       (:h1 (esc ,title))
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

(defgeneric canonical-url (object))

(defclass/f image ()
  (file-hash
   original-file
   thumbnail))

(define-html-presentation (image)
  (:h2 (esc (pathname-name (original-file image))))
  (:img :src (format nil "/present.jpg?hash=~A&size=thumb" (image-folders:hash-format (file-hash image)))))

(defmethod canonical-url ((image image))
  (format nil "/present.html?hash=~A" (image-folders:hash-format (file-hash image))))

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

(defun folder-name (folder)
  (or (pathname-name folder)
      (last1 (pathname-directory folder))))

(defun load-images-from (folder)
  (setf (gethash (folder-name folder) named-folders)
        (make-instance 'image-folder
                       :name (folder-name folder)
                       :content-list (mapcar
                                      (compose (lambda (image)
                                                 (setf (gethash (image-folders:hash-format (file-hash image))
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
  (:ul
   (dolist (image (content-list image-folder))
     (htm
      (:li
       (:a :href (canonical-url image)
        (present-html image stream)))))))

(defmethod canonical-url ((image-folder image-folder))
  (format nil "/present/?name=~A" (name image-folder)))
