(defpackage :image-folders
  (:use :cl :ol
        :com.gigamonkeys.pathnames)
  (:export
   :hash=
   :hash-format
   :file-hash
   :image-file-p
   :images-in-folder))
;;; TODO maybe migrate to cl-fad

(in-package :image-folders)

(defparameter example-gallery-folder
  #P "~/beispiel-galerie/")

(defparameter thumb-dir
  #P "~/beispiel-galerie/thumbs/")

;;; get a listing of all the image files in a folder
(defun images-in-folder (folder)
  (remove-if-not #'image-file-p (list-directory folder)))

(defun image-file-p (file)
  "check for the file ending, and a proper file."
  ;; TODO perhaps check the file contents for additional security
  (and (file-p file)
       (in (pathname-type file)
           ("jpg" "png" "gif")
           :test string-equal)))

;;; calculate hashes and display them nicely
(defun file-hash (file)
  (ironclad:digest-file 'ironclad:sha1 file))

(defparameter hex-chars "0123456789abcdef")

(defun hash-format (hash-vector)
  (with-output-to-string (stream)
    (map 'nil
         (lambda (h)
           (multiple-value-bind (q r) (floor h 16)
             (write-char (char hex-chars q) stream)
             (write-char (char hex-chars r) stream)))
         hash-vector)))

(defun hash= (a b)
  (equalp a b))

;;; create smaller versions of images
(defun thumb-filename (file &optional (ending "thumb"))
  (let ((hash (hash-format (file-hash file))))
    (make-pathname
     :name (format nil "~A.~A" hash ending)
     :type "jpg"
     :defaults thumb-dir)))

(defun resize-to-long-side (w h l)
  "Divide W and H by d, such that the larger of W/d and H/d equals L."
  (if (<= w h)
      (values (round (* w l) h) l)
      (values l (round (* h l) w))))

;; TODO seems to take an awful lot of memory
(defun create-thumbnail (file &optional (long-side 100) (ending 'thumb))
  (let ((image (ch-image:read-image-file file)))
    (multiple-value-bind (width height)
        (resize-to-long-side (ch-image:image-width image)
                             (ch-image:image-height image)
                             long-side)
      (ch-image:write-jpeg-file
       (thumb-filename file (format nil "~(~A~)" ending))
       (ch-image:resize-image image width height)))))
