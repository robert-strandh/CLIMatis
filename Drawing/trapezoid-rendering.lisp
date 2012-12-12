(cl:in-package #:clim3-rendering)

;;; Compute the area of a trapezoid.
(defun trapezoid-area (base1 base2 height)
  (declare (type double-float base1 base2 height))
  (* (+ base1 base2)
     height
     0.5d0))

;;; Compute the area of a triangle.
(defun triangle-area (base height)
  (declare (type double-float base height))
  (* base height 0.5d0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; OPACITIES is an array of opacity values which are double floats
;;; between 0d0 and 1d0.  ROW is the index of a row into the OPACITIES
;;; array, representing a scanline.  The rectangle to be rendered is
;;; entirely contained in that scanline.  HEIGHT is the height of that
;;; rectangle, which is then necessarily less than or equal to 1.  X1
;;; and X2 are the left and right x coordinates of the rectangles
;;; respectively.
;;;
;;; Two cases are distinguished.  Either X1 and X2 are in the same
;;; pixel in which case the full area of the rectangle contributes to
;;; that pixel.  If X1 and X2 are not in the same pixel, we handle the
;;; first and last pixels separately.  All "fully coverd"
;;; (horizontally) pixels get the same contribution which is the
;;; height of the rectangle multiplied by 1, the width of a pixel.
;;;
;;;                   x1                            x2
;;;                ___|_____________________________|_____
;;;              _|   .   |       |       |       | .     |
;;;             | |       |       |       |       |       |
;;;      height | |       |       |       |       |       |
;;;             |_|   .   |       |       |       | .     |
;;;               |_______|_______|_______|_______|_______|
;;;

(defun rectangle (opacities row height x1 x2)
  (if (<= x2 (+ (ffloor x1) 1d0))
      ;; It is all in one pixel
      (incf (aref opacities row (floor x1)) (* height (- x2 x1)))
      (progn
	;; do the first pixel.
	(incf (aref opacities row (floor x1))
	      (* (- (+ (ffloor x1) 1d0) x1)
		 height))
	;; do the pixels that are completely covered
	(loop for x from (1+ (floor x1)) below (floor x2)
	      do (incf (aref opacities row x) height))
	;; do the last pixel
	(incf (aref opacities row (floor x2))
	      (* height (- x2 (ffloor x2)))))))
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The trapezoid here is such that the left and right edges are
;;; parallel, so the two bases of the trapezoid are vertical. 
;;;
;;; OPACITIES is an array of opacity values which are double floats
;;; between 0d0 and 1d0.  ROW is the index of a row into the OPACITIES
;;; array, representing a scanline.  The trapezoid to be rendered is
;;; entirely contained in that scanline.  HEIGHT1 is the height at the
;;; left end of the trapezoid and HEIGHT2 is the height at the right
;;; end of the trapezoid, and the heights are then necessarily less
;;; than or equal to 1.  X1 and X2 are the left and right x
;;; coordinates of the trapezoid respectively.
;;;
;;; Two cases are distinguished.  Either X1 and X2 are in the same
;;; pixel in which case the full area of the trapezoid contributes to
;;; that pixel.  If X1 and X2 are not in the same pixel, we handle the
;;; first and last pixels separately.  The "fully coverd"
;;; (horizontally) pixels get a contribution that varies
;;; proportionally to the distance from x1.
;;;
;;; HEIGHT1 or HEIGHT2 might be 0 in which case the trapezoid
;;; degenerates into a triangle.
;;;
;;;                   x1                            x2
;;;                ___|_____________________________|_____
;;;              _|   .   |       |       |       |       |
;;;             | |       |       |       |       | .     |_
;;;     height1 | |       |       |       |       | .     |_| height2
;;;             |_|   .   |       |       |       |       |
;;;               |_______|_______|_______|_______|_______|
;;;

(defun small-trapezoid (opacities row height1 height2 x1 x2)
  (if (<= x2 (+ (ffloor x1) 1d0))
      ;; It is all in one pixel
      (incf (aref opacities row (floor x1))
	    (trapezoid-area height1 height2 (- x2 x1)))
      (let ((slope (/ (- height2 height1) (- x2 x1)))
	    (xx2 (+ (ffloor x1) 1d0)))
	;; start with the first pixel
	(incf (aref opacities row (floor x1))
	      (trapezoid-area height1 (+ height1 (* slope (- xx2 x1)))
			      (- xx2 x1)))
	;; do the pixels that are completely covered
	(loop for x from (1+ (floor x1)) below (floor x2)
	      for xx from (- (+ (ffloor x1) 1.5d0) x1) by 1d0
	      do (incf (aref opacities row x)
		       (+ height1 (* slope xx))))
	;; do the last pixel
	(incf (aref opacities row (floor x2))
	      (trapezoid-area (+ height1 (* slope (- (ffloor x2) x1)))
			      height2
			      (- x2 (ffloor x2)))))))

(defun nearly-equal (x1 x2)
  (< (abs (- x2 x1)) 1d-6))

(defun scanline-trapezoid (opacities row height x1 x2 x3 x4)
  (cond ((nearly-equal x1 x2)
	 (cond ((nearly-equal x3 x4)
		(rectangle opacities row height x1 x3))
	       ((< x3 x4)
		(rectangle opacities row height x1 x3)
		(small-trapezoid opacities row height 0d0 x3 x4))
	       (t
		(rectangle opacities row height x1 x4)
		(small-trapezoid opacities row height 0d0 x4 x3))))
	((< x2 x1)
	 ;; flip up and down.
	 (scanline-trapezoid opacities row height x2 x1 x4 x3))
	((nearly-equal x3 x4)
	 (small-trapezoid opacities row 0d0 height x1 x2)
	 (rectangle opacities row height x2 x3))
	((< x3 x2)
	 (let* ((slope1 (/ height (- x2 x1)))
		(height1 (* slope1 x3))
		(slope2 (/ height (- x4 x3)))
		(height2 (* slope2 (- x4 x2))))
	   (small-trapezoid opacities row 0d0 height1 x1 x3)
	   (small-trapezoid opacities row height1 height2 x3 x2)
	   (small-trapezoid opacities row height2 0d0 x2 x4)))
	((< x3 x4)
	 (small-trapezoid opacities row 0d0 height x1 x2)
	 (rectangle opacities row height x2 x3)
	 (small-trapezoid opacities row height 0d0 x3 x4))
	(t
	 (small-trapezoid opacities row 0d0 height x1 x2)
	 (rectangle opacities row height x2 x4)
	 (small-trapezoid opacities row height 0d0 x4 x3))))

(defun big-trapezoid (opacities yt yb xtl xbl xtr xbr)
  (if (<= yb (+ (ffloor yt) 1d0))
      ;; it is all in one scanline
      (scanline-trapezoid opacities (floor yt)
			  (- yb yt) xtl xbl xtr xbr)
      (let ((left-slope (/ (- xbl xtl) (- yb yt)))
	    (right-slope (/ (- xbr xtr) (- yb yt))))
	(flet ((xl (y) (+ xtl (* left-slope (- y yt))))
	       (xr (y) (+ xtr (* right-slope (- y yt)))))
	  ;; handle first scanline
	  (let* ((yyb (+ (ffloor yt) 1d0))
		 (height (-  yyb yt))
		 (xxbl (xl yyb))
		 (xxbr (xr yyb)))
	    (scanline-trapezoid opacities (floor yt)
				height xtl xxbl xtr xxbr))
	  ;; handle scanlines that are completely covered
	  (loop for y from (1+ (floor yt)) below (floor yb)
		for yy from (+ (ffloor yt) 1d0) by 1d0
		do (scanline-trapezoid opacities y
				       1d0
				       (xl yy) (xl (+ yy 1d0))
				       (xr yy) (xr (+ yy 1d0))))
	  ;; handle the last scanline
	  (scanline-trapezoid opacities (floor yb)
			      (- yb (ffloor yb))
			      (xl (ffloor yb)) (xl yb)
			      (xr (ffloor yb)) (xr yb))))))

(defun translate-trapezoid (trapezoid dx dy)
  (destructuring-bind (yt yb xtl xbl xtr xbr) trapezoid
    (list (+ yt dy) (+ yb dy) (+ xtl dx) (+ xbl dx) (+ xtr dx) (+ xbr dx))))

(defun render-trapezoids (trapezoids)
  (let* ((min-y (reduce #'min trapezoids :key #'first))
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
    ;; Make sure no value is outside the range
    (loop for r from 0 below (array-dimension opacities 0)
	  do (loop for c from 0 below (array-dimension opacities 1)
		   do (setf (aref opacities r c)
			    (min 1d0 (max 0d0 (aref opacities r c))))))
    (values opacities (floor min-x) (floor min-y))))

