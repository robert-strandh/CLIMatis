(cl:in-package #:clim3-truetype)

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

(defgeneric flip-segment (segment))

(defmethod flip-segment ((segment line-segment))
  (destructuring-bind (x . y) (start segment)
    (make-instance
     'line-segment
     :start (cons x (- y)))))

(defmethod flip-segment ((segment spline-segment))
  (destructuring-bind (xs . ys) (start segment)
    (destructuring-bind (xc . yc) (control segment)
      (make-instance
       'spline-segment
       :start (cons xs (- ys))
       :control (cons xc (- yc))))))

(defun flip-contour (contour)
  (let ((result (loop repeat (element-count contour)
		      for segment in contour
		      collect (flip-segment segment))))
    (setf (cdr (last result)) result)
    result))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating instances of a TrueType font at a particular size.

(defparameter +points-per-inch+ 72)

;;; The concept of screen resolution is a tricky one.
;;; Here, we just estimate it in terms of the number of
;;; pixels per inch in a typical monitor. 
(defparameter *screen-resolution* 100)

(defclass glyph-instance ()
  (;; The distance from the reference point to the left edge of the
   ;; mask.  When positive, the left edge of the mask is to the right
   ;; of the reference point. 
   (%x-offset :initarg :x-offset :reader x-offset)
   ;; The distance from the reference point to the top edge of the
   ;; mask.  When positive, the top edge of the mask is below the
   ;; reference point (which almost never happens).
   (%y-offset :initarg :y-offset :reader y-offset)
   ;; The amount to be added to the x value of the reference point to
   ;; get the reference point of the glyph to follow.
   (%advance-width :initarg :advance-width :reader advance-width)
   ;; A 2-dimensional array of double floats, representing opacity
   ;; values between 0 and 1.
   (%mask :initarg :mask :reader mask)))

(defun instantiate-glyph (units/em glyph point-size resolution)
  (let* ((scale-factor (coerce (/ (* point-size resolution 1/72) units/em) 'double-float))
	 (contours (create-contours (zpb-ttf:contours glyph)))
	 (flipped-contours (mapcar #'flip-contour contours))
	 (scaled-contours (loop for contour in flipped-contours
				collect (scale-contour contour scale-factor)))
	 (polygons (mapcar #'interpolate-contour scaled-contours))
	 (advance-width (ceiling (* scale-factor (zpb-ttf:advance-width glyph)))))
    (multiple-value-bind (mask min-x min-y)
	(clim3-rendering:render-polygons polygons)
      (make-instance 'glyph-instance
		     :x-offset min-x
		     :y-offset min-y
		     :advance-width advance-width
		     :mask mask))))

(defclass font-instance ()
  ((%font-loader :initarg :font-loader :reader font-loader)
   (%size :initarg :size :reader size)
   (%ascender :initarg :ascender :reader ascender)
   (%descender :initarg :descender :reader descender)
   ;; A hash table in which the keys are the glyphs of the
   ;; TrueType font, and the values are the glyph instances
   (%glyph-instances :initform (make-hash-table :test #'eq)
		     :reader glyph-instances)))

(defun instantiate-font (font-loader size)
  (let* ((resolution *screen-resolution*)
	 (units/em (zpb-ttf:units/em font-loader))
	 (factor (coerce (/ (* size resolution 1/72) units/em) 'double-float)))
    (make-instance 'font-instance
       :font-loader font-loader
       :size size
       :ascender (floor (* (- (zpb-ttf:ascender font-loader)) factor))
       :descender (ceiling (* (- (zpb-ttf:descender font-loader)) factor)))))

(defun find-glyph-instance (font-instance character)
  (let* ((font-loader (font-loader font-instance))
	 (glyph (zpb-ttf:find-glyph character font-loader)))
    (or (gethash glyph (glyph-instances font-instance))
	(setf (gethash glyph (glyph-instances font-instance))
	      (instantiate-glyph (zpb-ttf:units/em font-loader)
				 glyph
				 (size font-instance)
				 *screen-resolution*)))))