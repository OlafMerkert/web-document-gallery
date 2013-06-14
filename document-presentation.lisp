(defpackage :document-presentation
  (:nicknames :doc-pres)
  (:use :cl :ol
        :web-utils
        :document-hierarchy)
  (:export
   :present
   :define-present-method))

(in-package :document-presentation)

(defgeneric present (output-spec object context))

(defun spec-p (reference spec)
  (search (symbol-name reference)
          (symbol-name spec)
          :test #'char-equal))

(defmacro define-present-method (spec class &body body)
  `(defmethod present ((spec (eql ',spec))
                       (,class ,class)
                       context)
     (declare (ignorable spec context ,class))
     ,@(cond ((spec-p 'html spec)
              `((html/node ,@body)))
             (t body))))

