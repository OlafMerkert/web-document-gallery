(defpackage :document-hierarchy
  (:nicknames :doc-hier)
  (:use :cl :ol )
  (:export))

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

;;; interface for streams/lists of objects

(defclass ref-stream ()
  ((ref-list :initarg :ref-list
             :initform nil
             :accessor ref-list))
  (:documentation "interface for a list of refs."))
