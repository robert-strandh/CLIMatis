(cl:in-package #:clim3-rendering)

(defun translate-trapezoid (trapezoid dx dy)
  (destructuring-bind (yt yb xtl xbl xtr xbr) trapezoid
    (list (+ yt dy) (+ yb dy) (+ xtl dx) (+ xbl dx) (+ xtr dx) (+ xbr dx))))

(defun render-polygons (polygons)
  (let* ((trapezoids (trapezoids-from-polygons polygons))
	 (min-y (reduce #'min trapezoids :key #'first))
	 (max-y (reduce #'max trapezoids :key #'second))
	 (min-x (min (reduce #'min trapezoids :key #'third)
		     (reduce #'min trapezoids :key #'fourth)))
	 (max-x (max (reduce #'max trapezoids :key #'fifth)
		     (reduce #'max trapezoids :key #'sixth)))
	 ;; FIXME: get rid of the 1+ by fixing elsewhere
	 (opacities (make-array (list (1+ (- (ceiling max-y) (floor min-y)))
				      (1+ (- (ceiling max-x) (floor min-x))))
				:element-type 'double-float
				:initial-element 0d0)))
    (loop for trapezoid in trapezoids
	  do (apply #'big-trapezoid opacities
		    (translate-trapezoid trapezoid (- (floor min-x)) (- (floor min-y)))))
    (values opacities (floor min-x) (floor min-y))))

(defun print-mask (mask)
  (loop for c from 0 below (array-dimension mask 0)
	do (loop for r from 0 below (array-dimension mask 1)
		 do (format t "~5,3f " (aref mask c r)))
	   (format t "~%")))
		    
(defparameter *b-outer*
  '((1 . 1) (4 . 1) (5.5 . 2) (6 . 3.5) (5.5 . 5) (4.5 . 5.5)
    (5.5 . 6) (6 . 7.5) (5.5 . 9) (4 . 10) (1 . 10)))

(defparameter *b-inner-upper*
  '((2 . 2) (3.5 . 2) (4.5 . 2.5) (5 . 3.5) (4.5 . 4.5) (3.5 . 5) (2 . 5)))

(defparameter *b-inner-lower*
  '((2 . 6) (3.5 . 6) (4.5 . 6.5) (5 . 7.5) (4.5 . 8.5) (3.5 . 9) (2 . 9)))