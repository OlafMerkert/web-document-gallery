(defpackage :web-image-elements
  (:use :cl :ol :who
        :web-elements))

(in-package :web-image-elements)

;;; single images
(defclass/f image ()
  (file-hash
   original-file
   preview
   thumbnail))

(defmethod description-string ((image image))
  (pathname-name (original-file image)))

;; todo this should be generalisable to hashed vs named objects
(defmethod canonical-url ((image image))
  (uri "/present.html" :hash (file-hash image)))

(define-html-presentation (image)
  (:div :class "image-preview"
   (:img :src (uri "/present.file" :hash (file-hash image) :size "preview"))))

(defmethod present-file ((image image) (format (eql 'thumb)))
  (thumbnail image))

(defmethod present-file ((image image) (format (eql 'preview)))
  (preview image))

(defmethod present-file ((image image) (format (eql 'original)))
  (original-file image))

(defun path->image (path)
  "Create an image object to represent the image with given path.
Assumes thumbnail and preview have already been generated."
  (file-hashes:with-hash (hash path)
    (make-instance 'image
                   :file-hash hash
                   :original-file path
                   :preview (image-folders:scaled-filename hash image-folders:preview-size)
                   :thumbnail (image-folders:scaled-filename hash image-folders:thumb-size))))

;;; named folders of images
(defclass/f image-folder ()
  (name
   content-list))

(defmethod description-string ((image-folder image-folder))
  (name image-folder))

(defmethod canonical-url ((image-folder image-folder))
  (uri "/present.html" :name (name image-folder)))

(define-html-presentation (image-folder)
  (:ul :class "imagelist"
   (dolist (image (content-list image-folder))
     (htm
      (:li
       (:a :href (canonical-url image)
           (:img :src (uri "/present.file" :hash  (file-hash image) :size "thumb")
                 :alt (description-string image))))))))

;; load an entire folder of images into the database
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
                                                                hashed-objects)
                                                       image))
                                               #'path->image)
                                      (image-folders:images-in-folder folder)))))
