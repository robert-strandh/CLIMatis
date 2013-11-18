(cl:in-package #:clim3-paint)

(defgeneric new-paint (zone))

(defmethod new-paint ((zone clim3:input))
  nil)

(defmethod new-paint ((zone clim3-ext:compound-mixin))
  (clim3-ext:map-over-children-bottom-to-top
   (lambda (child)
     (clim3:with-zone child
       (new-paint child)))
   zone))

