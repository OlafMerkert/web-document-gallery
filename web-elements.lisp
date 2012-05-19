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
   preview
   thumbnail))

(define-html-presentation (image)
  (:img :src (uri "/present.jpg" :hash (file-hash image) :size "preview")))

(defmethod canonical-url ((image image))
  (uri "/present.html" :hash (file-hash image)))

(hunchentoot:define-easy-handler (image-present :uri "/present.jpg") (hash size)
  (multiple-value-bind (image present) (gethash hash presentable-objects)
    (when (and present (typep image 'image))
      (cond ((string= size "thumb")
             (hunchentoot:handle-static-file (thumbnail image)))
            ((string= size "preview")
             (hunchentoot:handle-static-file (preview image)))
            ((string= size "original")
             (hunchentoot:handle-static-file (original-file image)))))))
;; TODO error handling in the IMAGE-PRESENT handler

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
  (setf (gethash (folder-name folder) named-folders)
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
           (:img :src (uri "/present.jpg" :hash  (file-hash image) :size "thumb")
                 :alt (description-string image))))))))

(defmethod canonical-url ((image-folder image-folder))
  (uri "/present/" :name (name image-folder)))
