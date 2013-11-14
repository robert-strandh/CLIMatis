(cl:in-package #:clim3-rendering)

(defun render-polygons (polygons)
  (render-trapezoids (trapezoids-from-polygons polygons)))

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
