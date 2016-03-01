(cl:in-package #:common-lisp-user)

(defpackage #:clim3-rendering
  (:use #:common-lisp)
  (:export
   #:render-trapezoids
   #:render-polygons
   #:render-paths
   #:trapezoids-from-polygons))
