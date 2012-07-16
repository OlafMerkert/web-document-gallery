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
           :ref-elt
           :parentable-ref
           :parent
           :parentable-p
           :find-ancestors
           :build-stream/set-parent))

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

(defgeneric ref-elt (collection identifier)
  (:documentation "Search for a document described by IDENTIFIER in a
  COLLECTION."))

(defmethod ref-elt ((ref-stream ref-stream) (index integer))
  "Find a document in a stream given by an index."
  (elt ref-stream index))


(defmethod ref-elt ((ref-stream ref-stream) (hash-or-name string))
  "Find a document in a stream given by a hash or a name. First, look
  for a hash matching HASH-OR-NAME, then for a name."
  (or (find-if (lambda (x) (and (hashed-p x)
                           (string-equal (hash x) hash-or-name)))
               (ref-list ref-stream))
      ;; make name comparison case sensitive
      (find-if (lambda (x) (and (named-p x)
                           (string= (name x) hash-or-name)))
               (ref-list ref-stream))))

;;; interface for objects with parent
(defclass parentable-ref (ref)
  ((parent :initarg :parent
           :initform nil
           :accessor parent))
  (:documentation "a ref with a pointer to a canonical parent, for use
  if one desires a bidirectional tree structure."))

(defun parentable-p (object)
  "test whether object has a parent"
  (typep object 'parentable-ref))

(defun find-ancestors (object &optional children)
  "Return a list of all the ancestors of the given object, starting
with the very first ancestor found."
  (if (parentable-p object)
      (find-ancestors (parent object) (list* object children))
      (if object ; object might be nil, which we don't care about
          (list* object children)
          children)))

(defun build-stream/set-parent (elements)
  "Create a simple ref-stream with the given ELEMENTS, and set it as
  their parent, if possible."
  (aprog1 (make-instance 'ref-stream :ref-list elements)
    (dolist (el elements)
      (when (parentable-p el)
       (setf (parent el) it)))))
