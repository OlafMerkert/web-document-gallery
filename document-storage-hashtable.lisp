(defpackage :document-storage-hashtable
  (:nicknames :document-storage)
  (:use :cl :ol :document-hierarchy)
  (:export
   :ref-elt))

(in-package :document-storage-hashtable)

(defvar hashed-documents
  (make-hash-table :test 'equal :size 1000))

(defvar named-documents
  (make-hash-table :test 'equal :size 1000))

(defmethod ref-elt ((stream (eql 'global)) (hash-or-name string))
  "Find a document in the global document storage. First search for a
  matching hash, if none is found, search for a matching name."
  (or (gethash hash-or-name hashed-documents)
      (gethash hash-or-name named-documents)))

(defun add-to-storage (object)
  "Add a given object to the main database. If it has a hash, put it
  in HASHED-DOCUMENTS, if it has a name, put it in NAMED-DOCUMENTS."
  (when (hashed-p object)
    (setf (gethash (hash object) hashed-documents) object))
  (when (named-p  object)
    (setf (gethash (name object) named-documents)  object))
  (unless (or (hashed-p object)
              (named-p  object))
    (error "Cannot add object ~A to storage without hash or name." object)))
