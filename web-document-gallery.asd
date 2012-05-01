(defsystem :web-document-gallery
    :depends-on (ol-utils
                 ol-data-utils
                 cl-who
                 hunchentoot
                 com.gigamonkeys.pathnames
                 ironclad
                 cl-gd)
    :serial t
    :components ((:file "web-elements")
                 (:file "image-folders")))
