(in-package #:clim3-icons)

(defgeneric size (font))

(defclass font ()
  ((%stroke-width :initarg :stroke-width :reader stroke-width)
   (%stroke-height :initarg :stroke-height :reader stroke-height)
   (%size :initarg :size :reader size)
   (%masks :initform (make-hash-table) :reader masks)))

(defun make-mask (size paths)
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
      (let ((mask (make-array (list size size)
			      :element-type 'double-float
			      :initial-element 0d0)))
	(aa:cells-sweep state (lambda (x y alpha)
				(setf alpha (min 256 (max 0 alpha)))
				(setf (aref mask y x)
				      (/ alpha 256d0))))
	mask))))

;;;    0     1
;;;    \    /
;;;     ****
;;;       ****
;;;         ****
;;;           ****
;;;             ****
;;;               ****
;;;             5 - **** - 2
;;;               ****
;;;             ****
;;;           ****
;;;         ****
;;;       ****
;;;     ****
;;;    /    \
;;;   4      3

(defun make-mask-right (font)
  (with-accessors ((size size)
		   (stroke-width stroke-width))
      font
    (let* ((hmargin (round (* 0.2 size)))
	   (vmargin (round (* 0.2 size)))
	   (hdelta (* stroke-width 2.0))
	   (p0 (complex hmargin (- size vmargin)))
	   (p1 (+ p0 hdelta))
	   (p2 (complex (- size hmargin) (/ size 2)))
	   (p4 (complex hmargin vmargin))
	   (p3 (+ p4 hdelta))
	   (p5 (- p2 hdelta)))
      (make-mask size
		 (list (mf p0 -- p1 -- p2 -- p3 -- p4 -- p5 -- cycle))))))

;;;                0     1
;;;                \    /
;;;                 ****
;;;               ****
;;;             ****
;;;           ****
;;;         ****
;;;       ****
;;; 5 - **** - 2
;;;       ****
;;;         ****
;;;           ****
;;;             ****
;;;               ****
;;;                 ****
;;;                /    \
;;;               4      3

(defun make-mask-left (font)
  (with-accessors ((size size)
		   (stroke-width stroke-width))
      font
    (let* ((hmargin (round (* 0.2 size)))
	   (vmargin (round (* 0.2 size)))
	   (hdelta (* stroke-width 2.0))
	   (p5 (complex hmargin (/ size 2)))
	   (p2 (+ p5 hdelta))
	   (p1 (complex (- size hmargin) (- size vmargin)))
	   (p0 (- p1 hdelta))
	   (p3 (complex (- size hmargin) vmargin))
	   (p4 (- p3 hdelta)))
      (make-mask size
		 (list (mf p0 -- p1 -- p2 -- p3 -- p4 -- p5 -- cycle))))))

;;;  
;;;                          2
;;;                         /    
;;;  -                     *
;;; |                    ***         **
;;; |        0     1   *****     **   **
;;; |         \     \*******      **   **
;;; |       -  *************  **   **   **
;;; |      |   *************   **   **  **
;;; |h2    |h1 *************    **  **  **
;;; |      |   *************   **   **  **
;;; |       -  *************  **   **  **
;;; |         /     /*******      **  **
;;; |        5     4   *****     **  **
;;; |                    ***        ** 
;;;  -                     *
;;;            |_w1_|       \             
;;;                          3  
;;;            |_____w2_____|
;;;       

(defun make-mask-speaker (font)
  (with-accessors ((size size)
		   (stroke-width stroke-width))
      font
    (make-mask
     size
     (cons (let*  ((left-margin (round (* 0.1 size)))
		   (w2 (round (* 0.3 size)))
		   (w1 (* 0.4 w2))
		   (h1/2 (round (* 0.15 size)))
		   (h2/2 (* 0.4 size))
		   (p0 (complex left-margin (+ (/ size 2) h1/2)))
		   (p1 (+ p0 w1))
		   (p5 (complex left-margin (- (/ size 2) h1/2)))
		   (p4 (+ p5 w1))
		   (p2 (complex (+ left-margin w2) (+ (/ size 2) h2/2)))
		   (p3 (complex (+ left-margin w2) (- (/ size 2) h2/2))))
	     (mf p0 -- p1 -- p2 -- p3 -- p4 -- p5 -- cycle))
	   (let ((center (complex (round (* size 0.2)) (round (/ size 2))))
		 (thickness (max 1 (round (/ stroke-width 2))))
		 (angle (* pi 0.2))
		 (min-radius (round (* size 0.3))))
	     (flet ((wave (radius)
		      (format t "~a ~a ~a ~a~%"
			      center
			      (* radius (exp (* #c(0 -1) angle)))
			      (* radius (exp (* #c(0 1) angle)))
			      thickness)
		      (mf
			(+ center (* radius (exp (* #c(0 -1) angle))))
			++
			(+ center radius)
			++
			(+ center (* radius (exp (* #c(0 1) angle))))
			--
			(+ center (* (+ radius thickness) (exp (* #c(0 1) angle))))
			++
			(+ center (+ radius thickness))
			++
			(+ center (* (+ radius thickness) (exp (* #c(0 -1) angle))))
			--
			cycle)))
	       (loop for radius from min-radius by (* 2 thickness)
		     while (< (+ (realpart center)
				 radius
				 (* 2 thickness))
			      size)
		     collect (wave radius))))))))

(defun add-mask (name masks mask)
  (setf (gethash name masks) mask))

(defun make-icons (size)
  (let* ((stroke-width (round (* 0.1 size)))
	 (font (make-instance 'font
			      :size size
			      :stroke-width stroke-width
			      :stroke-height stroke-width))
	 (masks (masks font)))
    (add-mask :right masks (make-mask-right font))
    (add-mask :left masks (make-mask-left font))
    (add-mask :speaker masks (make-mask-speaker font))
    font))

(defun find-icon (font name)
  (gethash name (masks font)))
