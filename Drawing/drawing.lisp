(defpackage #:drawing
  (:use #:common-lisp)
  (:export #:draw-convex-polygon))

(in-package #:drawing)

;;; Assume clockwise order. 
(defun horizontally-convex-polygon (opacities points)
  (let* ((min (reduce #'min points :key #'cdr))
	 (minpos (position min points :key #'cdr))
	 (max (reduce #'max points :key #'cdr))
	 (maxpos (position max points :key #'cdr)))
    (let ((left (reverse 
		 (if (< minpos maxpos)
		     (append (subseq points maxpos)
			     (subseq points 0 minpos))
		     (subseq points maxpos minpos))))
	  (right (if (< minpos maxpos)
		     (subseq points (1+ minpos) (1+ maxpos))
		     (append (subseq points (1+ minpos))
			     (subseq points 0 (1+ maxpos)))))
	  (y (cdr (elt points minpos)))
	  (xl (car (elt points minpos)))
	  (xr (car (elt points minpos))))
      (loop until (and (null left) (null right))
	    do (cond ((nearly-equal (cdar right) (cdar left))
		      (unless (nearly-equal y (cdar left))
			(big-trapezoid opacities
				       y (cdar left)
				       xl (caar left) xr (caar right)))
		      (setf xl (caar left))
		      (setf xr (caar right))
		      (setf y (max (cdar left) (cdar right)))
		      (pop left)
		      (pop right))
		     ((nearly-equal y (cdar left))
		      (setf xl (caar left))
		      (pop left))
		     ((nearly-equal y (cdar right))
		      (setf xr (caar right))
		      (pop right))
		     ((< (cdar left) (cdar right))
		      (let ((new-xr (+ xr (* (- (cdar left) y)
					     (/ (- (caar right) xr)
						(- (cdar right) y))))))
			(big-trapezoid opacities
				       y (cdar left)
				       xl (caar left) xr new-xr)
			(setf xr new-xr)
			(setf y (cdar left)))
		      (pop left))
		     (t
		      (let ((new-xl (+ xl (* (- (cdar right) y)
					     (/ (- (caar left) xl)
						(- (cdar left) y))))))
			(big-trapezoid opacities
				       y (cdar right)
				       xl new-xl xr (caar right))
			(setf xl new-xl)
			(setf y (cdar right)))
		      (pop right)))))))
