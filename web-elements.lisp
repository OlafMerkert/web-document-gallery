(defpackage :web-elements
  (:use :cl :ol)
  (:export
   :start-server))

(in-package :web-elements)

(defun start-server ()
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor :port 8080)))

