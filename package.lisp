;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:gpx-heatmap
  (:use #:cl #:alexandria #:gpxtools #:3d-vectors)
  (:export  #:find-segments
            #:create-heatmap
            #:create-heatmap-from-segments))


