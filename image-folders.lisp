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
