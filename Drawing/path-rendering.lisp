(cl:in-package #:clim3-rendering)

(defun render-path (path)
  (values (make-array (list 1 1)
                      :element-type 'double-float
                      :initial-element 0d0) 0 0))
