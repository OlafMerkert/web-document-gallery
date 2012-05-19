(defpackage :file-hashes
  (:use :cl :ol)
  (:export
   :file-hash
   :hash-format
   :hash=
   :with-hash))

(in-package :file-hashes)

;;; calculate hashes and display them nicely
(defun file-hash (file)
  (hash-format (ironclad:digest-file 'ironclad:sha1 file)))

(defparameter hex-chars "0123456789abcdef")

(defun hash-format (hash-vector)
  (if (stringp hash-vector)
      hash-vector
      (with-output-to-string (stream)
        (map 'nil
             (lambda (h)
               (multiple-value-bind (q r) (floor h 16)
                 (write-char (char hex-chars q) stream)
                 (write-char (char hex-chars r) stream)))
             hash-vector))))

(defun hash= (a b)
  (string= a b))

(defmacro! with-hash ((hash o!file) &body body)
  `(let ((,hash (if (hash-p ,g!file) ,g!file
                    (file-hash ,g!file))))
     ,@body))

(defun hash-p (possible-hash)
  (and (stringp possible-hash)
       (every (lambda (x) (position x hex-chars :test #'char-equal))
              possible-hash)))