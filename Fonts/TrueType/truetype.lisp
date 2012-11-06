(defpackage #:clim3-truetype
  (:use #:common-lisp)
  (:export
   ))

(in-package #:clim3-truetype)

(defclass segment ()
  ((%start :initarg :start :reader start)))

(defclass line-segment (segment) ())

(defmethod print-object ((object line-segment) stream)
  (format stream "[line-segment :start ~s]" (start object)))

(defclass spline-segment (segment)
  ((%control :initarg :control :reader control)))

(defmethod print-object ((object spline-segment) stream)
  (format stream "[spline-segment start: ~s :control ~s]"
	  (start object) (control object)))

(defgeneric scale-segment (segment factor))

(defmethod scale-segment ((segment line-segment) factor)
  (destructuring-bind (x . y) (start segment)
    (make-instance
     'line-segment
     :start (cons (* x factor) (* y factor)))))

(defmethod scale-segment ((segment spline-segment) factor)
  (destructuring-bind (xs . ys) (start segment)
    (destructuring-bind (xc . yc) (control segment)
      (make-instance
       'spline-segment
       :start (cons (* xs factor) (* ys factor))
       :control (cons (* xc factor) (* yc factor))))))

(defun element-count (circular-list)
  (1+ (loop for element in (cdr circular-list)
	    until (eq element (car circular-list))
	    count t)))

(defun create-contour (ttf-contour)
  (let ((result '()))
    (zpb-ttf:do-contour-segments (start control end) ttf-contour
      (push (if (null control)
		(make-instance
		 'line-segment
		 :start (cons (coerce (zpb-ttf:x start) 'double-float)
			      (coerce (zpb-ttf:y start) 'double-float)))
		(make-instance
		 'spline-segment
		 :start (cons (coerce (zpb-ttf:x start) 'double-float)
			      (coerce (zpb-ttf:y start) 'double-float))
		 :control (cons (coerce (zpb-ttf:x control) 'double-float)
				(coerce (zpb-ttf:y control) 'double-float))))
	    result))
    (setf result (nreverse result))
    (setf (cdr (last result)) result)
    result))

(defun create-contours (ttf-contours)
  (loop for contour across ttf-contours
	collect (create-contour contour)))

	  
(defun scale-contour (contour factor)
  (let ((result (loop repeat (element-count contour)
		      for segment in contour
		      collect (scale-segment segment factor))))
    (setf (cdr (last result)) result)
    result))

(defun triangle-area (a b c)
  (destructuring-bind (xa . ya) a
    (destructuring-bind (xb . yb) b
      (destructuring-bind (xc . yc) c
	(* 0.5d0 (abs (- (* (- xa xc) (- yb ya)) (* (- xa xb) (- yc ya)))))))))
    
(defun interpolate-spline (start control end)
  (if (< (triangle-area start control end) 0.1d0)
      (list start)
      (destructuring-bind (xa . ya) start
	(destructuring-bind (xb . yb) control
	  (destructuring-bind (xc . yc) end
	    (let* ((xp1 (+ (* 0.5d0 xa) (* 0.5d0 xb)))
		   (yp1 (+ (* 0.5d0 ya) (* 0.5d0 yb)))
		   (xp2 (+ (* 0.5d0 xb) (* 0.5d0 xc)))
		   (yp2 (+ (* 0.5d0 yb) (* 0.5d0 yc)))
		   (xp3 (+ (* 0.5d0 xp1) (* 0.5d0 xp2)))
		   (yp3 (+ (* 0.5d0 yp1) (* 0.5d0 yp2))))
	      (append (interpolate-spline start (cons xp1 yp1) (cons xp3 yp3))
		      (interpolate-spline (cons xp3 yp3) (cons xp2 yp2) end))))))))

(defun interpolate-contour (contour)
  (let* ((count (element-count contour))
	 (result 
	   (loop repeat count
		 for (segment next) on contour
		 append (if (typep segment 'line-segment)
			    (list (start segment))
			    (interpolate-spline (start segment)
						(control segment)
						(start next))))))
    result))
