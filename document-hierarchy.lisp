(defpackage :document-hierarchy
  (:nicknames :doc-hier)
  (:use :cl :ol )
  (:export :ref
           :hashed-ref
           :named-ref
           :hash
           :name
           :hashed-p
           :named-p
           :ref-stream
           :ref-list
           :ref-elt))

(in-package :document-hierarchy)

;;; interface for objects

(defclass ref  ()
  ()
  (:documentation "the basic entity representing a referencable
  object."))

(defclass hashed-ref (ref)
  ((hash :initarg :hash
         :reader hash))
  (:documentation "interface for anything adressable by an immutable
  HASH."))

(defclass named-ref (ref)
  ((name :initarg :name
         :accessor name))
  (:documentation "interface for anything adressable by a mutable NAME."))

(defun hashed-p (object)
  "test whether object is identified by a hash."
  (typep object 'hashed-ref))

(defun named-p (object)
  "test whether object is identified by a name."
  (typep object 'named-ref))

;;; interface for streams/lists of objects

(defclass ref-stream ()
  ((ref-list :initarg :ref-list
             :initform nil
             :accessor ref-list))
  (:documentation "interface for a list of refs."))

(defmethod ref-elt ((ref-stream ref-stream) (index integer))
  "Find a document in a stream given by an index."
  (elt ref-stream index))

(defun fand (&rest functions)
  "`AND` for functions (of same parameter spec)."
  (lambda (&rest args)
    (every (lambda (f) (apply f args)) functions)))

(defmethod ref-elt ((ref-stream ref-stream) (hash-or-name string))
  "Find a document in a stream given by a hash or a name. First, look
  for a hash matching HASH-OR-NAME, then for a name."
  (or (find-if (fand #'hashed-p (lambda (x) (string-equal (hash x) hash-or-name)))
               (ref-list ref-stream))
      ;; TODO make name comparison case sensitive
      (find-if (fand #'named-p  (lambda (x) (string-equal (name x) hash-or-name)))
               (ref-list ref-stream))))
