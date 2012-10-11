(in-package :mf)

(defclass basic-path-join () ())

(defclass concatenate-path-join (basic-path-join) ())

(defparameter *concatenate-path-join* (make-instance 'concatenate-path-join))

(defclass controls (basic-path-join)
  ((a :initarg :a)
   (b :initarg :b)))

(defclass tensions (basic-path-join)
  ((a :initarg :a)
   (b :initarg :b)))

(defclass direction-specifier () ())

(defclass curl (direction-specifier)
  ((curl :initarg :curl)))

(defclass direction (direction-specifier)
  ((direction :initarg :direction)))

(defclass cycle () ())

(defparameter *cycle* (make-instance 'cycle))

(defun flatten (list)
  (loop for elem in list
	append (if (consp elem)
		   (flatten elem)
		   (list elem))))

(defclass context ()
  ((%neighbor :initform nil :initarg :neighbor :accessor neighbor)
   (%curl :initform nil :initarg :curl :accessor curl)
   (%direction :initform nil :initarg :direction :accessor direction)
   (%tension :initform nil :initarg :tension :accessor tension)
   (%control :initform nil :initarg :control :accessor control)
   (%angle :initform nil :initarg :angle :accessor angle)))

(defclass point ()
  ((%point :initarg :point :reader point)
   (%rank :initform nil :accessor rank)))

(defclass left-context-point (point)
  ((%left-context :initform (make-instance 'context)
		  :initarg :left-context :reader left-context)))

(defclass right-context-point (point)
  ((%right-context :initform (make-instance 'context)
		   :initarg :right-context :reader right-context)))

(defclass left-endpoint (right-context-point) ())

(defclass right-endpoint (left-context-point) ())

(defclass corner-point (left-context-point right-context-point) ())

(defclass interior-point (left-context-point right-context-point) ())

(defun remove-concatenates (path)
  (loop until (null path)
	if (and (not (null (cddr path))) ; path has at least 3 elements
		(eq (second path) *concatenate-path-join*))
	collect (prog1 (make-instance 'corner-point :point (car path))
		  (setf path (cdddr path)))
	else collect (pop path)))

(defun check-start-end (path)
  (assert (numberp (car path))
	  ()
	  "the path must start with a point, but ~s was found" (car path))
  (assert (or (numberp (car (last path))) (eq (car (last path)) *cycle*))
	  ()
	  "the path must end with a point, but ~s was found" (car (last path))))

(defun check-cycle (path)
  ;; check that there is no `cycle' other than at
  ;; the end of the path.
  (assert (not (member-if (lambda (x) (eq x *cycle*)) (butlast path)))
	  ()
	  "only the last element of a path can be CYCLE"))

(defun check-syntax (path)
  (check-start-end path)
  (check-cycle path)
  (loop for (x y z) on path
	;; check that each direction specifier is surrounded
	;; by a point on one side and a tensions object on the other
	;; or possibly a cycle object on the right
	do (when (typep y 'direction-specifier)
	     (assert (or (and (typep x 'tensions)
			      (or (numberp z) (eq z *cycle*)))
			 (and (numberp x)
			      (typep z 'tensions)))
		     ()
		     "a direction specifier must have a point on one side and a tensions object on the other, but ~a and ~a were found" x z))
	;; check that each controls object and each
	;; concatenate path join is surrounded by points
	;; or possibly a cycle object on the right
	do (when (or (eq y *concatenate-path-join*)
		     (typep y 'controls))
	     (assert (numberp x)
		     ()
		     "a concatenate path join or a controls object must have a point to the left, but ~a was found" x)
	     (assert (or (numberp z) (eq z *cycle*))
		     ()
		     "a concatenate path join or a controls object must have a point or `cycle' to the right, but ~a was found" z))
	;; check that each tensions object is
	;; surrounded by a direction specifier or a point
	;; or possibly a cycle on the right
	do (when (typep y 'tensions)
	     (assert (typep x '(or number direction-specifier))
		     ()
		     "a tension object must have a direction specifier or a point to the left, but ~a was found" x)
	     (assert (or (typep z '(or number direction-specifier)) (eq z *cycle*))
		     ()
		     "a tension object must have a direction specifier, a point, or `cycle' to the right, but ~a was found" z))
	;; check that each point or cycle object is surrounded by a
	;; direction specifier or a basic path join
	do (when (or (numberp y) (eq y *cycle*))
	     (assert (typep x '(or direction-specifier basic-path-join))
		     ()
		     "a point or a cycle object must have a direction specifier or a basic path joint to the left, but ~a was found" x)
	     (assert (or (null z) (typep z '(or direction-specifier basic-path-join)))
		     ()
		     "a point must have a direction specifier or a basic path joint to the right, but ~a was found" z))))

(defun propagate-direction-specifiers (path)
  (loop for (x y z) on path
	do (when (typep y 'direction-specifier)
	     (if (typep x 'point)
		 (if (typep y 'curl)
		     (setf (curl (right-context x)) (slot-value y 'curl))
		     (setf (direction (right-context x)) (slot-value y 'direction)))
		 (if (typep y 'curl)
		     (setf (curl (left-context (if (eq z *cycle*) (car path) z)))
			   (slot-value y 'curl))
		     (setf (direction (left-context (if (eq z *cycle*) (car path) z)))
			   (slot-value y 'direction)))))))

(defun propagate-tensions-controls (path)
  (loop for (x y z) on path
	do (typecase y
	     (tensions (setf (tension (right-context x)) (slot-value y 'a))
		       (setf (tension (left-context (if (eq z *cycle*)
							(car path)
							z)))
			     (slot-value y 'b)))
	     (controls (setf (control (right-context x)) (slot-value y 'a))
		       (setf (control (left-context (if (eq z *cycle*)
							(car path)
							z)))
			     (slot-value y 'b))))))

(defun link-and-rank-points (path)
  (when (eq (car (last path)) *cycle*)
    (nbutlast path)
    (setf (neighbor (right-context (car (last path)))) (car path))
    (setf (neighbor (left-context (car path))) (car (last path))))
  (loop for (x y) on path
	for i from 0
	do (setf (rank x) i)
	until (null y)
	do (setf (neighbor (right-context x)) y
		 (neighbor (left-context y)) x)))

(defun propagate-directions (path)
  (flet ((possibly-fill-in-curl (context)
	   (when (and (null (control context))
		      (null (direction context))
		      (null (curl context)))
	     (setf (curl context) 1.0))))
    (loop for point in path
	  do (typecase point
	       (left-endpoint
		(possibly-fill-in-curl (right-context point)))
	       (right-endpoint
		(possibly-fill-in-curl (left-context point)))
	       (corner-point
		(possibly-fill-in-curl (right-context point))
		(possibly-fill-in-curl (left-context point)))
	       (interior-point
		(let ((lc (left-context point))
		      (rc (right-context point)))
		  (when (and (null (curl lc))
			     (null (direction lc))
			     (null (control lc)))
		    (cond ((not (null (curl rc)))
			   (setf (curl lc)
				 (curl rc)))
			  ((not (null (direction rc)))
			   (setf (direction lc)
				 (direction rc)))
			  ((not (null (control rc)))
			   (setf (direction lc)
				 (- (control rc) (point point))))))
		  (when (and (null (curl rc))
			     (null (direction rc))
			     (null (control rc)))
		    (cond ((not (null (curl lc)))
			   (setf (curl rc)
				 (curl lc)))
			  ((not (null (direction lc)))
			   (setf (direction rc)
				 (direction lc)))
			  ((not (null (control lc)))
			   (setf (direction rc)
				 (- (point point) (control lc))))))))))))

(defun solve (system)
  (let* ((width (array-dimension system 1))
	 (height (array-dimension system 0))
	 (rows (loop for i from 0 below height collect i)))
    (flet ((eliminate (rows column)
	     (flet ((eliminate-row (row1 row2)
		      (let ((factor (/ (aref system row2 column)
				       (aref system row1 column))))
			(loop for i from column below width
			      do (decf (aref system row2 i)
				       (* factor (aref system row1 i)))))))
	       (let ((pivot-row (member-if (lambda (row)
					     (not (zerop (aref system row column))))
					   rows)))
		 (rotatef (car pivot-row) (car rows))
		 (loop for row in (cdr rows)
		       do (eliminate-row (car rows) row))))))
      (loop for column from 0 below (- width 2)
	    for remaining-rows on rows
	    do (eliminate remaining-rows column))
      (let ((last-column (1- width)))
	(loop for rev-rows on (reverse rows)
	      for column downfrom (- width 2)
	      do (loop with row1 = (car rev-rows)
		       for row2 in (cdr rev-rows)
		       do (decf (aref system row2 last-column)
				(* (/ (aref system row2 column)
				      (aref system row1 column))
				   (aref system row1 last-column)))
		       do (setf (aref system row2 column) 0.0)))
	(loop for column from 0
	      for row in rows
	      do (setf (aref system row last-column)
		       (/ (aref system row last-column)
			  (aref system row column))))))
    (let ((solution (make-array height)))
      (loop for i from 0 below height
	    do (setf (aref solution i) (aref system (elt rows i) (1- width))))
      solution)))

(defun solve-angles (path)
  (let* ((open-p (typep (car path) 'left-endpoint))
	 (length (length path))
	 (nb-variables (- (* 2 (length path))
			  (if open-p 2 0)))
	 (width (1+ nb-variables))
	 (matrix (make-array (list nb-variables width) :initial-element 0.0))
	 (equation-number -1))
    (labels ((out (i) (* 2 i))
	     (in (i) (1- (* 2 (if (zerop i) length i))))
	     (handle-right-context (point context)
	       (cond ((not (null (control context)))
		      (let ((out-angle (phase (/ (- (control context)
						    (point point))
						 (- (point (neighbor context))
						    (point point))))))
			(setf (aref matrix (incf equation-number) (out (rank point)))
			      1.0)
			(setf (aref matrix equation-number (1- width))
			      out-angle)))
		     ((not (null (direction context)))
		      (let ((out-angle (phase (/ (direction context)
						 (- (point (neighbor context))
						    (point point))))))
			(setf (aref matrix (incf equation-number) (out (rank point)))
			      1.0)
			(setf (aref matrix equation-number (1- width))
			      out-angle)))
		     ((not (null (curl context)))
		      (let* ((a0 (tension context))
			     (b1 (tension (left-context (neighbor context))))
			     (g0 (curl context))
			     (c1 (- (* a0 a0 a0 (- 1 (* 3.0 b1)))
				    (* g0 b1 b1 b1)))
			     (c2 (+ (* a0 a0 a0)
				    (- (* g0 b1 b1 b1))
				    (* 3.0 a0))))
			(setf (aref matrix (incf equation-number) (out (rank point)))
			      c1)
			(setf (aref matrix equation-number (in (rank (neighbor context))))
			      c2)))))
	     (handle-left-context (point context)
	       (cond ((not (null (control context)))
		      (let ((in-angle (phase (/ (- (point point)
						   (point (neighbor context)))
						(- (point point)
						   (control context))))))
			(setf (aref matrix (incf equation-number) (in (rank point)))
			      1.0)
			(setf (aref matrix equation-number (1- width))
			      in-angle)))
		     ((not (null (direction context)))
		      (let ((in-angle (phase (/ (- (point point)
						   (point (neighbor context)))
						(direction context)))))
			(setf (aref matrix (incf equation-number) (in (rank point)))
			      1.0)
			(setf (aref matrix equation-number (1- width))
			      in-angle)))
		     ((not (null (curl context)))
		      (let* ((bn (tension context))
			     (an-1 (tension (right-context (neighbor context))))
			     (gn (curl context))
			     (c1 (+ (* bn bn bn)
				    (- (* gn an-1 an-1 an-1))
				    (* 3.0 bn)))
			     (c2 (- (* bn bn bn (- 1 (* 3.0 an-1)))
				    (* gn an-1 an-1 an-1))))
			(setf (aref matrix (incf equation-number) (out (rank (neighbor context))))
			      c1)
			(setf (aref matrix equation-number (in (rank point)))
			      c2))))))
      (loop for point in path
	    do (typecase point
		 (left-endpoint
		  (handle-right-context point (right-context point)))
		 (right-endpoint
		  (handle-left-context point (left-context point)))
		 (corner-point
		  (handle-right-context point (right-context point))
		  (handle-left-context point (left-context point)))
		 (interior-point
		  (let ((lc (left-context point))
			(rc (right-context point)))
		    (if (and (null (curl lc)) (null (direction lc)) (null (control lc))
			     (null (curl rc)) (null (direction rc)) (null (control rc)))
			(let* ((ak-1 (tension (right-context (neighbor lc))))
			       (bk (tension lc))
			       (ak (tension rc))
			       (bk+1 (tension (left-context (neighbor rc))))
			       (lk (abs (- (point point) (point (neighbor lc)))))
			       (lk+1 (abs (- (point (neighbor rc)) (point point))))
			       (c1 (* bk bk bk+1 lk))
			       (c2 (* bk bk bk+1 lk+1 (- 1.0 (* 3.0 ak-1))))
			       (c3 (- (* ak ak ak-1 lk (- 1.0 (* 3.0 bk+1)))))
			       (c4 (- (* ak ak ak-1 lk))))
			  (setf (aref matrix
				      (incf equation-number)
				      (out (rank (neighbor lc))))
				c1)
			  (setf (aref matrix equation-number (in (rank point)))
				c2)
			  (setf (aref matrix equation-number (out (rank point)))
				c3)
			  (setf (aref matrix
				      equation-number
				      (in (rank (neighbor rc))))
				c4)
			  (setf (aref matrix (incf equation-number) (out (rank point)))
				1.0)
			  (setf (aref matrix equation-number (in (rank point)))
				1.0)
			  (setf (aref matrix equation-number (1- width))
				(- (phase (/ (- (point (neighbor rc)) (point point))
					     (- (point point) (point (neighbor lc))))))))
			(progn (handle-left-context point (left-context point))
			       (handle-right-context point (right-context point))))))))
      (let ((solution (solve matrix)))
	(loop for point in path
	      do (when (typep point 'left-context-point)
		   (setf (angle (left-context point)) (aref solution (in (rank point)))))
	      do (when (typep point 'right-context-point)
		   (setf (angle (right-context point)) (aref solution (out (rank point))))))))))

(defun hobby (theta phi)
  (/ (+ 2.0
	(* #.(sqrt 2.0)
	   (- (sin theta) (* 1/16 (sin phi)))
	   (- (sin phi) (* 1/16 (sin theta)))
	   (- (cos theta) (cos phi))))
     (* 3.0
	(+ 1.0
	   (* #.(* 0.5 (- (sqrt 5.0) 1.0))
	      (cos theta))
	   (* #.(* 0.5 (- 3.0 (sqrt 5.0)))
	      (cos phi))))))

(defun handle-point-pair (p0 p1 tr tl theta phi)
  (values 
   (+ p0
      (/ (* (exp (* #c(0.0 1.0) theta))
	    (- p1 p0)
	    (hobby theta phi))
	 tr))
   (- p1
      (/ (* (exp (* #c(0.0 -1.0) phi))
	    (- p1 p0)
	    (hobby phi theta))
	 tl))))

(defun assign-control-points (path)
  (loop for (p0 p1) on path
	until (null p1)
	do (let* ((rc (right-context p0))
		  (lc (left-context p1))
		  (theta (angle rc))
		  (phi (angle lc)))
	     (when (null (control rc))
	       (multiple-value-bind (c0 c1)
		   (handle-point-pair (point p0) (point p1)
				      (tension rc) (tension lc) theta phi)
		 (setf (control rc) c0
		       (control lc) c1)))))
  (unless (typep (car path) 'left-endpoint)
    (let* ((p0 (car (last path)))
	   (p1 (car path))
	   (rc (right-context p0))
	   (lc (left-context p1))
	   (theta (angle rc))
	   (phi (angle lc)))
      (when (null (control rc))
	(multiple-value-bind (c0 c1)
	    (handle-point-pair (point p0) (point p1)
			       (tension rc) (tension lc) theta phi)
	  (setf (control rc) c0
		(control lc) c1))))))

(defun point-to-complex (point)
  "convert a point to a complex number"
  (complex (paths:point-x point) (paths:point-y point)))

(defun complex-to-point (complex)
  "convert a complex number to a point"
  (paths:make-point (realpart complex) (imagpart complex)))

(defun make-mf-path (&rest body)
  (let ((path (mapcar (lambda (x)
			(if (consp x) ; that's how a point is represented in cl-vectors
			    (point-to-complex x)
			    x))
		      (flatten body))))
    (check-syntax path)
    ;; replace each sequence of type `p & p' by a corner point
    (setf path (remove-concatenates path))
    ;; replace the end points if path is not a cycle    
    (unless (eq (car (last path)) *cycle*)
      (setf (car path) (make-instance 'left-endpoint :point (car path)))
      (setf (car (last path)) (make-instance 'right-endpoint :point (car (last path)))))
    ;; replace all other points by interior points
    (setf path (loop for element in path
		     collect (if (numberp element)
				 (make-instance 'interior-point :point element)
				 element)))
    ;; propagate direction specifiers to their respective points
    (propagate-direction-specifiers path)
    ;; remove all direction specifiers
    (setf path (remove-if (lambda (x) (typep x 'direction-specifier)) path))
    ;; propagate tensions and controls to their respective points
    (propagate-tensions-controls path)
    ;; remove all tensions and controls objects
    (setf path (remove-if (lambda (x) (typep x '(or tensions controls))) path))
    ;; link and rank the points of the path, remove the cycle object
    (link-and-rank-points path)
    ;; now the path contains only point objects
    (propagate-directions path)
    (solve-angles path)
    (assign-control-points path)
    (if (typep (car path) 'left-endpoint)
	(let ((result (paths:create-path :open-polyline)))
	  (paths:path-reset result (complex-to-point (point (car path))))
	  (loop for point in (butlast path)
		do (let ((rc (right-context point)))
		     (paths:path-extend
		      result
		      (paths:make-bezier-curve
		       (list (complex-to-point (control rc))
			     (complex-to-point (control (left-context (neighbor rc))))))
		      (complex-to-point (point (neighbor rc))))))
	  result)
	(let ((result (paths:create-path :polygon)))
	  (paths:path-reset result (complex-to-point (point (car path))))
	  (loop for point in path
		do (let ((rc (right-context point)))
		     (paths:path-extend
		      result
		      (paths:make-bezier-curve
		       (list (complex-to-point (control rc))
			     (complex-to-point (control (left-context (neighbor rc))))))
		      (complex-to-point (point (neighbor rc))))))
	  result))))

(defparameter *infinity* 4095.99998) ;see the MF book

(defmacro mf (&body body)
  `(flet ((control (a)
	    (make-instance 'controls :a a :b a))
	  (controls (a b)
	    (make-instance 'controls :a a :b b))
	  (tension (a)
	    (assert (>= a 0.75)
		    ()
		    "tension values must be greater than 0.75: ~a"
		    a)
	    (make-instance 'tensions :a a :b a))
	  (tensions (a b)
	    (assert (>= (min a b) 0.75)
		    ()
		    "tension values must be greater than 0.75: ~a"
		    (min a b))
	    (make-instance 'tensions :a a :b b))
	  (direction (d)
	    (make-instance 'direction :direction d))
	  (curl (c)
	    (make-instance 'curl :curl c)))
     (declare (ignorable (function control)
			 (function controls)
			 (function tension)
			 (function tensions)
			 (function direction)
			 (function curl)))
     (let* ((++ (tension 1.0))
	    (+++ (tension 1.0)) ; this is not right
	    (& *concatenate-path-join*)
	    (--- (tension *infinity*))
	    (-- (list (make-instance 'curl :curl 1)
		      (make-instance 'tensions :a 2 :b 2) ; should be 1 rather than 2
		      (make-instance 'curl :curl 1)))
	    (cycle *cycle*)
	    (up (direction #c(0 1)))
	    (down (direction #c(0 -1)))
	    (left (direction #c(-1 0)))
	    (right (direction #c(1 0))))
       (declare (ignorable +++ & --- -- cycle up down left right))
       (make-mf-path
	 ,@body))))

(defun part-way (p0 p1 alpha)
  (+ (* (- 1 alpha) p0) (* alpha p1)))
