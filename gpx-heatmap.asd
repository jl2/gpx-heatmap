;;;; gpx-heatmap.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:gpx-heatmap
  :description "Describe gpx-heatmap here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:clgl
               #:gpxtools
               #:kdtree
               #:utm)
  :serial t
  :components ((:file "package")
               (:file "gpx-heatmap")))

