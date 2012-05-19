(defsystem :web-document-gallery
    :depends-on (ol-utils
                 ol-data-utils
                 cl-who
                 hunchentoot
                 com.gigamonkeys.pathnames
                 ironclad
                 cl-gd
                 zpb-exif)
    :serial t
    :components ((:file "file-hashes")
                 (:file "image-folders")
                 (:file "web-elements")))
