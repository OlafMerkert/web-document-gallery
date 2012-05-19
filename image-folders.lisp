(defpackage :image-folders
  (:use :cl :ol
        :com.gigamonkeys.pathnames
        :cl-gd
        :zpb-exif
        :file-hashes)
  (:export
   :image-file-p
   :images-in-folder
   :create-thumbnail
   :thumb-filename
   :scaled-filename))
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

;;; create smaller versions of images
(defun scaled-filename (file size &optional (ending "scaled"))
  "Generate the pathname for a scaled version of the FILE."
  (with-hash (hash file)
    (make-pathname
     :name (format nil "~A.~A-~A" hash ending size)
     :type "jpg"
     :defaults thumb-dir)))

(defun resize-to-long-side (w h l)
  "Divide W and H by d, such that the larger of W/d and H/d equals L."
  (if (<= w h)
      (values (round (* w l) h) l)
      (values l (round (* h l) w))))

(defun flip/destroy (image direction)
  "Flip an gd image :HORIZONTAL or :VERTICAL or not at all, return the
flipped copy and destroy the original image."
  (if direction
      (let* ((w (image-width  image))
             (h (image-height image))
             (new-image (create-image w h )))
        (case direction
          (:horizontal
           (copy-image image new-image 0 0 (- h 1) 0 w h))
          (:vertical
           (copy-image image new-image 0 0 0 (- w 1) w h)))
        (destroy-image image)
        new-image)
      image))

(defun rotate/destroy (image angle)
  "Rotate a gd image by 0, 90, 180 or 270 degrees, return the rotated
  copy and destroy the original image."
  (if (not (zerop angle))
      (let* ((w (image-width  image))
             (h (image-height image))
             (l (zerop (mod angle 180))) ; no aspect change
             (n-w (if l w h))
             (n-h (if l h w))
             (new-image (create-image n-w n-h)))
        (copy-image image new-image 0 0 (round n-w 2) (round n-h 2) w h
                    :rotate t :angle angle)
        (destroy-image image)
        new-image)
      image))


(defun create-scaled-versions (file &rest long-sides)
  "Take the image at FILE and generate scaled versions for each of
LONG-SIDES."
  (unless long-sides
    (setf long-sides (list 100)))
  (multiple-value-bind (flip rotation) (image-orientation file)
    (flet ((fix-orientation (image)
             (rotate/destroy (flip/destroy image flip) rotation)))
     (with-image-from-file (image file)
       (mapcar (lambda (long-side)
                 (aprog1 (scaled-filename file long-side)
                   (create-scaled-version it image long-side #'fix-orientation)))
               long-sides)))))

(defun create-scaled-version (filename image long-side fix-orientation)
  "helper function for CREATE-SCALED-VERSIONS."
  (multiple-value-bind (width height)
      (resize-to-long-side (image-width image)
                           (image-height image)
                           long-side)
    (with-image* (width height)
      (copy-image image *default-image*
                        0 0 0 0
                        (image-width image) (image-height image)
                        :dest-width width
                        :dest-height height
                        :resample t
                        :resize t)
      ;; TODO check for existing thumbnails??
      (setf *default-image* (funcall fix-orientation *default-image*))
      (write-image-to-file filename :if-exists :supersede))))

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

;; TODO resizing is not very pretty yet.
