;;;; gpx-heatmap.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:gpx-heatmap)

(defun bounding-box (points)
  (loop for pt across points
     minimizing (vx pt) into min-lat
     maximizing (vx pt) into max-lat
     minimizing (vy pt) into min-lon
     maximizing (vy pt) into max-lon
     minimizing (vz pt) into min-ele
     maximizing (vz pt) into max-ele
     finally (return (values (vec3 min-lat min-lon min-ele) (vec3 max-lat max-lon max-ele)))))

(defun bounding-box-segments (segments)
  (loop for seg across segments
     minimizing (min (vx (car seg)) (vx (cdr seg))) into min-lat
     maximizing (max (vx (car seg)) (vx (cdr seg))) into max-lat
     minimizing (min (vy (car seg)) (vy (cdr seg))) into min-lon
     maximizing (max (vy (car seg)) (vy (cdr seg))) into max-lon
     minimizing (min (vz (car seg)) (vz (cdr seg))) into min-ele
     maximizing (max (vz (car seg)) (vz (cdr seg))) into max-ele
     finally (return (values (vec3 min-lat min-lon min-ele) (vec3 max-lat max-lon max-ele)))))

(defun find-segments (segments tolerance)
  (let ((kdt (kdtree:create-kd-tree))
        (new-segments nil))
    (format t "Total number of points: ~a~%" (length segments))
    (loop for pt across segments 
       when (and (> (kdtree:pt-count kdt) 0) (let ((near (kdtree:nearest kdt (car pt))))
                                               (format t "point ~a near ~a~%" near (car pt))
                                               (> (vlength (v- (car pt) near)) tolerance)))
       do
         (push pt new-segments)
         (kdtree:add-point kdt (car pt)))
    (make-array (length new-segments) :initial-contents new-segments)))


(defun create-heatmap (directory &key (style :points) (color (vec4 0 1 0 1.0)))
  (let* ((gpx-pts (make-instance 'clgl:primitives)))
    (cond ((eq style :points)
           (let ((all-points (gpxtools:read-directory-to-points directory)))
             (multiple-value-bind (min-pt max-pt) (bounding-box all-points )
               (loop for gp across all-points
                  do
                    (clgl:add-point gpx-pts
                                    (clgl:map-pt gp min-pt max-pt)
                                    color)))))
          ((eq style :lines)
           (let ((all-segments (gpxtools:read-directory-to-segments directory)))
             (multiple-value-bind (min-pt max-pt) (bounding-box-segments all-segments)
               (loop for gp across all-segments
                  do
                    (clgl:add-line gpx-pts
                                   (clgl:map-pt (car gp) min-pt max-pt)
                                   (clgl:map-pt (cdr gp) min-pt max-pt)
                                   color))))))
    gpx-pts))

(defun create-heatmap-from-segments (segments &key (color (vec4 0 1 0 1.0)))
  (let ((gpx-pts (make-instance 'clgl:primitives)))
    (multiple-value-bind (min-pt max-pt) (bounding-box-segments segments)
      (loop for gp across segments
         do
           (clgl:add-line gpx-pts
                          (clgl:map-pt (car gp) min-pt max-pt)
                          (clgl:map-pt (cdr gp) min-pt max-pt)
                          color)))
    gpx-pts))
