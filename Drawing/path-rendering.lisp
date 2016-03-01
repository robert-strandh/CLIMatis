(cl:in-package #:clim3-rendering)

(defun 2point (cons)
  "Converts a (x . y) cons to a paths:point."
  (paths:make-point (car cons) (cdr cons)))

(defun path-map-line (path function)
  "Iterate over all the line on the contour of the path."
  (loop with iterator = (paths:path-iterator-segmented path)
        for previous-knot = nil then knot
        for (interpolation knot end-p) = (multiple-value-list (paths:path-iterator-next iterator))
        while knot
        when previous-knot
          do (funcall function previous-knot knot)
        until end-p
        finally (when knot
                  (funcall function knot (nth-value 1 (paths:path-iterator-next iterator))))))

(defun mask-of-paths (width height paths)
  "Returns a mask of dimension (height width) with paths drawn in it."
  (let ((state (aa:make-state)))
    (flet ((do-line (p1 p2)
             (aa:line-f state
                        (paths:point-x p1) (paths:point-y p1)
                        (paths:point-x p2) (paths:point-y p2))))
      (loop for path in paths
            do (path-map-line path #'do-line)))
    (let ((mask (make-array (list height width)
                            :element-type 'double-float
                            :initial-element 0d0)))
      (aa:cells-sweep state #'(lambda (x y alpha)
                                (when (and (>= x 0) (< x width)
                                           (>= y 0) (< y height))
                                  (setf alpha (min 256 (max 0 alpha)))
                                  (setf (aref mask y x) (/ alpha 256d0)))))
      mask)))

(defun render-paths (paths width height stroke-width)
  (let ((stroke-paths (list)))
    (dolist (path paths)
      (setf stroke-paths (append stroke-paths
                                 (paths:stroke-path
                                  (paths:make-simple-path (mapcar #'2point path)) stroke-width))))
    (mask-of-paths width height stroke-paths)))
