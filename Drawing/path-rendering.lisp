(cl:in-package #:clim3-rendering)

(defun 2point (cons)
  "Converts a (x . y) cons to a paths:point coercing to float."
  (paths:make-point (coerce (car cons) 'float)
                    (coerce (cdr cons) 'float)))

(defun mask-of-paths (width height paths)
  "Returns a mask of dimension (height width) with paths drawn in it."
  (flet ((convert-path-to-knots (path)
           (let* ((dpath (paths:make-discrete-path path))
                  (iterator (paths:path-iterator dpath)))
             (paths:path-iterator-reset iterator)
             (loop with end = nil
                   collect (multiple-value-bind (interpolation knot end-p)
                               (paths:path-iterator-next iterator)
                             (declare (ignore interpolation))
                             (setf end end-p)
                             knot)
                   until end))))
    (let ((state (aa:make-state))
          (knot-paths (mapcar #'convert-path-to-knots paths)))
      (loop for path in knot-paths
            do (loop for (p1 p2) on path
                     until (null p2)
                     do (aa:line-f state
                                   (paths:point-x p1) (paths:point-y p1)
                                   (paths:point-x p2) (paths:point-y p2))))
      (let* ((mask (make-array (list height width)
                               :element-type 'double-float
                               :initial-element 0d0)))
        (aa:cells-sweep state #'(lambda (x y alpha)
                                  (when (and (>= x 0) (< x width)
                                             (>= y 0) (< y height))
                                    (setf alpha (min 256 (max 0 alpha)))
                                    (setf (aref mask y x) (/ alpha 256d0)))))
        mask))))

(defun render-path (path width height stroke-width)
  (let ((paths (paths:stroke-path (paths:make-simple-path (mapcar #'2point path)) stroke-width)))
    (mask-of-paths width height paths)))
