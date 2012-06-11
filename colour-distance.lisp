(defpackage :colour-distance
  (:use :cl :ol)
  (:export
   :delta-e/rgb))

(in-package :colour-distance)

(defun euclidean-distance (a b)
  "Compute the euclidean distance of two sequences."
  (sqrt
   (reduce #'+
           (map 'vector (lambda (x y) (expt (- x y) 2)) a b))))

(defun euclidean-norm (a)
  "Compute the euclidean norm of a sequence."
  (sqrt
   (reduce #'+
           (map 'vector (lambda (x) (expt x 2)) a))))

(defun matrix-product (matrix vector)
  "Compute the left action of a MATRIX on a VECTOR."
  (map 'vector
       (lambda (i)
         (loop
            for x across vector
            for j from 0
            sum (* (aref matrix i j)
                   x)))
       (lrange matrix)))

(defclass/f colour ()
  ())

(defclass/f rgb (colour)
  (red
   green
   blue))

(defclass/f xyz (colour)
  (x
   y
   z))

(defclass/f lab (colour)
  (l a b))

(defun rgb->luv (colour))

(defun luv->rgb (colour))

(defparameter *critical-distance* 1)

(defun delta-e/luv (colour1 colour2)
  (euclidean-distance colour1 colour2))

(defun delta-e/rgb (colour1 colour2)
  (delta-e/luv
   (rgb->luv colour1)
   (rgb->luv colour2)))

(defgeneric colour-distance (method sample reference)
  (:documentation "Calculate the Delta E difference of colour against
  reference colour using the desired method."))

(defmacro minkowski (a b &optional c (sign '-))
  `(sqrt
    (+
     (expt ,a 2)
     (expt ,b 2)
     ,@(if c
        `((,sign (expt ,c 2)))))))

(defmacro coeff/quot (x y z)
  `(/ (* ,x ,z)
     (+ 1 (* ,y ,z))))

;; from http://www.brucelindbloom.com/index.html?ColorDifferenceCalc.html
(defmethod colour-distance ((method (eql 'cmc))
                            (sample lab)
                            (reference lab))
  (let ((l 1) (c 1)) ; parameters
   (let ((l1 (l reference)) (a1 (a reference)) (b1 (b reference))
         (l2 (l sample))    (a2 (a sample))    (b2 (b sample)))
     (let* ((c1 (minkowski a1 b1))
            (c2 (minkowski a2 b2))
            (dC (- c1 c2))
            (dH (minkowski (- a1 a2) (- b1 b2) dC))
            (dL (- l1 l2)))
       (let* ((sl (if (< l1 16)
                      0.511
                      (coeff/quot 0.040975 0.01765 l1)))
              (sc (+ (coeff/quot 0.0638 0.0131 c1) 0.638))
              (h1 (atan b1 a1))      ; should be in degrees
              (tt (if (<= (deg->rad 164) h1 (deg->rad 345))
                      (+ 0.56 (abs (* 0.2 (cos (+ h1 (deg->rad 168))))))
                      (+ 0.36 (abs (* 0.4 (cos (+ h1 (deg->rad 35))))))))
              (ff (sqrt (/ (expt c1 4) (+ (expt c1 4) 1900))))
              (sh (* sc (- (* ff tt) -1 ff))))
         ;; final formula
         (minkowski (/ dL l sl)
                    (/ dC c sc)
                    (/ dH sh) +))))))

(defun deg->rad (deg)
   (/ (* pi deg) 180))