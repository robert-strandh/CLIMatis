(cl:in-package #:clim3-paint)

(defgeneric clim3-ext:paint (zone))

(defmethod clim3-ext:paint ((zone clim3:input))
  nil)

(defmethod clim3-ext:paint ((zone clim3-ext:compound-mixin))
  (clim3-ext:map-over-children-bottom-to-top
   (lambda (child)
     (clim3:with-zone child
       (clim3-ext:paint child)))
   zone))

