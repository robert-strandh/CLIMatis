(cl:in-package #:clim3-gadgets-default)

(defclass theme (clim3-gadgets:theme)
  ()
  (:default-initargs
   :background-color (clim3:make-color 0.7 0.7 0.7)
   :foreground-color (clim3:make-color 0.8 0.8 0.9)
   :highlight-opacity 0.3
   :highlight-color (clim3:make-color 1.0 1.0 1.0)
   :raise-border-thickness 3
   :raise-border-opacity 0.3
   :sink-border-thickness 2
   :sink-border-opacity 0.3
   :gadget-text-style (clim3:text-style :free :sans :roman 12)
   :gadget-text-color (clim3:make-color 0.0 0.0 0.0)
   :icon-border-thickness 5))

(setf clim3-gadgets:*theme* (make-instance 'theme))

