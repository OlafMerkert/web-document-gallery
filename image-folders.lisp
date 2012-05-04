(defpackage :image-folders
  (:use :cl :ol
        :com.gigamonkeys.pathnames
        :zpb-exif)
  (:export
   :hash=
   :hash-format
   :file-hash
   :image-file-p
   :images-in-folder
   :create-thumbnail
   :thumb-filename))
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
(defun scaled-filename (file size &optional (ending "scaled"))
  "Generate the pathname for a scaled version of the FILE."
  (let ((hash (hash-format (file-hash file))))
    (make-pathname
     :name (format nil "~A.~A-~A" hash ending size)
     :type "jpg"
     :defaults thumb-dir)))

(defun resize-to-long-side (w h l)
  "Divide W and H by d, such that the larger of W/d and H/d equals L."
  (if (<= w h)
      (values (round (* w l) h) l)
      (values l (round (* h l) w))))

(defun create-scaled-version (file &rest long-sides)
  "Take the image at FILE and generate scaled versions for each of
LONG-SIDES."
  (unless long-sides
    (setf long-sides (list 100)))
  (let (filenames)
    (cl-gd:with-image-from-file (image file)
      (dolist (long-side long-sides)
        (multiple-value-bind (width height)
            (resize-to-long-side (cl-gd:image-width image)
                                 (cl-gd:image-height image)
                                 long-side)
          (cl-gd:with-image* (width height)
            ;; TODO account for exif rotation
            (cl-gd:copy-image image cl-gd:*default-image*
                              0 0 0 0
                              (cl-gd:image-width image) (cl-gd:image-height image)
                              :dest-width width
                              :dest-height height
                              :resize t)
            ;; TODO check for existing thumbnails??
            (cl-gd:write-image-to-file (first (push (scaled-filename file long-side)
                                                    filenames))
                                       :if-exists :supersede)))))
    (nreverse filenames)))

(defun image-orientation (file)
  "determine the flipping and rotation operation required to get the
image FILE to standard orientation.  Returns two values: first value
designates a flipping operation (nil, :vertical or :horizontal), the
second values the degrees of rotation to be applied after the
flipping."
  (ecase (exif-value "Orientation"
                     (make-exif file))
    (1 (values nil 0))
    (2 (values :vertical 0))
    (3 (values nil 180))
    (4 (values :horizontal 0))
    (5 (values :vertical 90))
    (6 (values nil 270))
    (7 (values :vertical 270))
    (8 (values nil 90))))
