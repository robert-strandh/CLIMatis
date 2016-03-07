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

(defun compute-pixel (r g b background alpha)
  ;; first normalize alpha (see cl-vectors aa.lisp comments)
  (let ((alpha (min 255 (abs alpha))))
    (cond ((<= alpha 1) background)
          ((>= alpha 254) (+ (ash r 16) (ash g 8) b))
          (t
           (let* ((back-r (logand 255 (ash background -16)))
                  (back-g (logand 255 (ash background -8)))
                  (back-b (logand 255 background))
                  (inv-alpha (- 255 alpha))
                  (new-r (truncate (+ (* r alpha) (* back-r inv-alpha)) 255))
                  (new-g (truncate (+ (* g alpha) (* back-g inv-alpha)) 255))
                  (new-b (truncate (+ (* b alpha) (* back-b inv-alpha)) 255)))
             (+ (ash new-r 16) (ash new-g 8) new-b))))))

(defun render-paths (paths pixel-array xpos ypos xend yend stroke-width color)
  (let ((stroke-paths (list)))
    (dolist (path paths)
      (setf stroke-paths (append stroke-paths
                                 (paths:stroke-path
                                  (paths:make-simple-path (mapcar #'2point path)) stroke-width))))
    (let ((state (aa:make-state))
          (width (array-dimension pixel-array 1))
          (height (array-dimension pixel-array 0))
          (r (round (* (clim3:red color) 255)))
          (g (round (* (clim3:green color) 255)))
          (b (round (* (clim3:blue color) 255))))
      (flet ((do-line (p1 p2)
               (aa:line-f state
                          (paths:point-x p1) (paths:point-y p1)
                          (paths:point-x p2) (paths:point-y p2)))
             (put-color (x y alpha)
               (let ((x (+ x xpos))
                     (y (+ y ypos)))
                 (when (and (>= x (max 0 xpos)) (< x (min xend width))
                            (>= y (max 0 ypos)) (< y (min yend height)))
                   (setf (aref pixel-array y x)
                         (compute-pixel r g b (aref pixel-array y x) alpha))))))
        (loop for path in stroke-paths
              do (path-map-line path #'do-line))
        (aa:cells-sweep state #'put-color)))))
