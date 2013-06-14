(defpackage :image-documents
  (:nicknames :imdoc)
  (:use :cl :ol
        :document-hierarchy
        :document-presentation)
  (:export))

(in-package :image-documents)

(defclass image (hashed-ref
                 parentable-ref)
  ((original :initarg :original
             :reader original)
   (preview  :initarg :preview
             :reader preview)
   (thumb    :initarg :thumb
             :reader thumb)))

(define-present-method original-file image
  (original image))

(define-present-method preview-file image
  (preview image))
(define-present-method thumb-file image
  (thumb image))

