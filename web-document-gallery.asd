(defsystem :web-document-gallery
    :depends-on (ol-utils
                 ol-data-utils
                 cl-who
                 hunchentoot
                 web-utils
                 com.gigamonkeys.pathnames
                 ironclad
                 cl-gd
                 zpb-exif
                 split-sequence
                 parenscript)
    :serial t
    :components ((:file "document-hierarchy")
                 (:file "document-storage-hashtable")
                 (:file "file-hashes")
                 (:file "image-folders")
                 (:file "web-elements")))
