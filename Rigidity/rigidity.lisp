(in-package #:rigidity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

;;; Check whether an object is a proper list.  We expect the list to
;;; be relatively short, so there is no point in doing anything fancy.
;;; Loop over the CONS cells of the list until either the end of the
;;; list is found, or we see a cell that we have already seen.  If se
;;; see a cell we have already seen, or we reach the end of the list
;;; and it is not NIL, then we do not have a proper list.  If we reach
;;; NIL at the end of the slist without having seen a cell twice, then
;;; we have a proper list.
(defun proper-list-p (object)
  (let ((cells '())
	(rest object))
    (loop until (atom rest)
	  do (if (member rest cells :test #'eq)
		 (return-from proper-list-p nil)
		 (progn (push rest cells)
			(pop rest))))
    (null rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rigidity function.

;;;; A rigidity function gives a force as a function of a size.  
;;;;
;;;; The size is a non-negative integer.  The force is a rational
;;;; number.
;;;;
;;;; A rigidity function is represented as a list with at least two
;;;; elements.  The last element is a positive rational number, and it
;;;; indicates a slope of the rigidity for large values of the size.
;;;; Every element except the last is a CONS where the CAR is a size
;;;; (a non-negative rational) and the CDR is the force at that size.
;;;; The CAR of the first CONS of a rigidity function is always 0.
;;;; The CDR of the first CONS of a rigidity function is a rational
;;;; that is 0 or negative.  The CONSes of a rigidity function are in
;;;; strictly increasing order with respect to the size.  The function
;;;; is strictly increasing, so that the CONSes of a rigidity function
;;;; are in strictly increasing order also with respect to the forces.
;;;; A negative force means a force toward expansion, and a positive
;;;; force means a force toward contraction.

;;; Accessors for rigidity functions.

(defun pointp (item)
  (and (consp item)
       (rationalp (car item))
       (not (minusp (car item)))
       (rationalp (cdr item))))

(defun make-point (size force)
  (cons size force))

(defun slopep (item)
  (and (rationalp item)
       (plusp item)))

(defun size (point)
  (car point))

(defun force (point)
  (cdr point))

;;; Check that some object is a rigidity function.  We handle circular
;;; lists without going into an infinite computation, by checking that
;;; two consecutive conses represent strictly increasing values.  This
;;; can not be the case if the list is circular.  
(defun rigidity-function-p (rigidity-function)
  ;; The argument to the auxilary function is always a CONS.  In
  ;; addition, the CAR of that CONS is known to be a valid point, in
  ;; that the size is a non-netagive rational number, and the force is
  ;; a rational number. 
  (labels ((aux (fun)
	     (and (consp (cdr fun))
		  (let ((second (second fun)))
		    (or (and (slopep second)
			     (null (cddr fun)))
			(let ((first (first fun)))
			  (and (pointp second)
			       (> (size second) (size first))
			       (> (force second) (force first))
			       (aux (cdr fun)))))))))
    ;; Check that we have a CONS, that the CAR of that CONS is a valid
    ;; first point, and then call the auxiliary function to check the
    ;; rest.
    (and (consp rigidity-function)
	 (let ((first (first rigidity-function)))
	   (pointp first)
	   (zerop (size first))
	   (not (plusp (force first)))
	   (aux rigidity-function)))))

;;; For a given rigidity-function and a given size, return the force
;;; at that size.  The size is a non-negative rational number.  We do
;;; not expect this function to be used in an inner loop, so we take
;;; the freedom to check that we have a valid rigidity-function.
(defun force-at-size (rigidity-function size)
  (assert (rigidity-function-p rigidity-function))
  (assert (rationalp size))
  (assert (not (minusp size)))
  (loop until (or (slopep (second rigidity-function))
		  (> (size (second rigidity-function)) size))
	do (pop rigidity-function))
  (let* ((s0 (size (first rigidity-function)))
	 (f0 (force (first rigidity-function)))
	 (slope (if (slopep (second rigidity-function))
		    (second rigidity-function)
		    (let* ((s1 (size (second rigidity-function)))
			   (f1 (force (second rigidity-function))))
		      (/ (- f1 f0) (- s1 s0))))))
    (+ f0 (* (- size s0) slope))))

;;; For a given rigidity-function and a given force, return the size
;;; at that force.  The force is a rational number.  We do not expect
;;; this function to be used in an inner loop, so we take the freedom
;;; to check that we have a valid rigidity-function.
(defun size-at-force (rigidity-function force)
  (assert (rigidity-function-p rigidity-function))
  (assert (rationalp force))
  (if (<= force (force (first rigidity-function)))
      0
      (progn 
	(loop until (or (slopep (second rigidity-function))
			(> (force (second rigidity-function)) force))
	      do (pop rigidity-function))
	(let* ((s0 (size (first rigidity-function)))
	       (f0 (force (first rigidity-function)))
	       (slope (if (slopep (second rigidity-function))
			  (second rigidity-function)
			  (let* ((s1 (size (second rigidity-function)))
				 (f1 (force (second rigidity-function))))
			    (/ (- f1 f0) (- s1 s0))))))
	  (+ s0 (/ (- force f0) slope))))))

;;; The natural size of a rigidity function is the size it takes 
;;; on when the force is 0. 
(defun natural-size (rigidity-function)
  (size-at-force rigidity-function 0))

;;; For sufficiently large values (size or force), every rigidity
;;; function degenerates into the form ((SIZE . FORCE) SLOPE).  When
;;; that is the case, the function can be expressed in one of two
;;; ways.  The first way is FORCE = SLOPE * SIZE + ZERO-SIZE-FORCE.
;;; This way of expressing it is handy for expressing the parallel
;;; combination of two or more functions, because then the combined
;;; function can be expressed as the sum of the parameters SLOPE and
;;; ZERO-SIZE-FORCE for each individual function.  The second way of
;;; expressing it is SIZE = ISLOPE * FORCE + ZERO-FORCE-SIZE.  When
;;; functions are combined serially, then this way is preferred,
;;; because then, again, the parameters of each indificual function
;;; can just be summed up.

;;; Use side effects to remove the first point of a rigidity function.
(defun eliminate-first-point (rigidity-function)
  (setf (first rigidity-function) (second rigidity-function))
  (setf (rest rigidity-function) (rest (rest rigidity-function))))

(defun combine-in-parallel (rigidity-functions)
  (assert (proper-list-p rigidity-functions))
  (assert (>= (length rigidity-functions) 1))
  (assert (every #'rigidity-function-p rigidity-functions))
  (let (;; Slope of combination of all degenerate functions.
	(slope 0)
	;; Force at size 0 for combination of all degenerate
	;; functions.
	(zero-size-force 0)
	;; We build the resulting function from the end, and then
	;; reverse it right before returning it.
	(reverse-result '()))
    ;; Make a copy of everything, because we side-effect the functions
    ;; as we process them.
    (let ((funs (copy-tree rigidity-functions)))
      ;; Create the first point of the result as the sum of the forces
      ;; at size 0 of each function.  Since every function starts with
      ;; a point at size 0, we can compute this value as the sum of
      ;; the forces of the first point of each function.
      (push (make-point 0 (reduce #'+ funs :key (lambda (fun) (force (first fun)))))
	    reverse-result)
      ;; Pull out and combine all the degenerate functions. 
      (setf funs
	    (loop for fun in funs
		  if (slopep (second fun))
		    do (let* ((first (first fun))
			      (size (size first))
			      (force (force first)))
			 (incf slope (second fun))
			 (incf zero-size-force (- force (* (second fun) size))))
		  else
		    collect fun))
      ;; Now, the second item of every function is a point.  Sort the
      ;; remaining functions by increasing size of the second point.
      (setf funs (sort funs #'< :key (lambda (fun) (size (second fun)))))
      ;; Invariants: 1.  The next point to be determined has a size
      ;; that is strictly greater than the size of the first point of
      ;; each function.  2. The second item of each function is a
      ;; point.  3. The functions are in increasing order with respect
      ;; to the size of the second point.
      (loop until (null funs)
	    do (let* ((size (size (second (car funs))))
		      (force (loop for fun in funs
				   sum (let* ((s0 (size (first fun)))
					      (f0 (force (first fun)))
					      (s1 (size (second fun)))
					      (f1 (force (second fun)))
					      (slope (/ (- f1 f0) (- s1 s0))))
					 (+ f0 (* (- size s0) slope))))))
		 ;; We have computed another point, and store it.  
		 (let ((dforce (+ (* slope size) zero-size-force)))
		   (push (make-point size (+ force dforce)) reverse-result))
		 ;; Now an arbitrary number of functions that are
		 ;; first on the list of remaining functions can have
		 ;; second point with a size that we just handled.
		 ;; Clearly, the first such function is one of them,
		 ;; but there can be more.  Since the list is sorted,
		 ;; they are all first.  We process each such function
		 ;; by removing the first point.  If by doing that,
		 ;; the result is a degenerate function, we remove it
		 ;; from the list, and add it to the global
		 ;; parameters.  If the result is not degenerate, we
		 ;; merge it into the list so as to preserve invariant
		 ;; number 3.
		 (loop until (or (null funs)
				 (> (size (second (car funs))) size))
		       do (let ((fun (pop funs)))
			    (eliminate-first-point fun)
			    (if (slopep (second fun))
				(let* ((first (first fun))
				       (size (size first))
				       (force (force first)))
				  (incf slope (second fun))
				  (incf zero-size-force
					(- force (* (second fun) size))))
				(setf funs (merge 'list (list fun) funs #'<
						  :key (lambda (fun)
							 (size (second fun))))))))))
      ;; When we come here, the list of non-degenerate functions is empty.
      (push slope reverse-result)
      (nreverse reverse-result))))

(defun combine-in-series (rigidity-functions)
  (assert (proper-list-p rigidity-functions))
  (assert (>= (length rigidity-functions) 1))
  (assert (every #'rigidity-function-p rigidity-functions))
  (let ((result '()))
    ;; Make a copy of everything, because we side-effect the functions
    ;; as we process them.
    (let ((funs (copy-tree rigidity-functions)))
      ;; Reverse the list representing each each function.
      (setf funs (mapcar #'reverse funs))
      ;; Determine the slope for large values of the size
      (push (/ (loop for fun in funs sum (/ (car fun)))) result)
      ;; Sort by decreasing value of the force of the last point.
      ;; Recall that the last point is the second element of the list
      ;; representing the function.
      (setf funs (sort funs #'> :key (lambda (fun) (force (second fun)))))
      (loop until (null funs)
	    do (let* ((force (force (second (car funs))))
		      (size (loop for fun in funs
				  sum (let ((s (size (second fun)))
					    (f (force (second fun)))
					    (slope (car fun)))
					(+ s (/ (- force f) slope))))))
		 ;; We now have the sum of the sizes for the largest
		 ;; force.  We make another point out of that. 
		 (push (make-point size force) result)
		 ;; Now we need to remove the last point of every
		 ;; function where the force of that point is the
		 ;; force that we just handled.  If the last point is
		 ;; also the first point, then remove the function
		 ;; altogether, because it can not longer contribute
		 ;; to the size.  If the last point is not the first
		 ;; point, then replace the slope of the function by
		 ;; the slope between the last and the next-to-last
		 ;; point, and merge the function with the others to
		 ;; preserve the order of the functions.
		 (loop until (or (null funs)
				 (< (force (second (car funs))) force))
		       do (let ((fun (pop funs)))
			    (unless (null (rest (rest fun)))
			      (let ((s1 (size (second fun)))
				    (f1 (force (second fun)))
				    (s0 (size (third fun)))
				    (f0 (force (third fun))))
				(setf (first fun)
				      (/ (- f1 f0) (- s1 s0)))
				(setf (rest fun) (rest (rest fun))))
			      (setf funs (merge 'list (list fun) funs #'>
						:key (lambda (fun)
						       (force (second fun)))))))))))
    result))

;;; Helper function.  Return the difference between the largest and
;;; the smallest element in a list. 
(defun max-min-diff (list)
  (- (reduce #'max list) (reduce #'min list)))

;;; Helper function. Given a list of rigidity functions and a list of
;;; sizes, return a list of forces, one for each function at its size.
(defun compute-forces (rigidity-functions sizes)
  (mapcar #'force-at-size rigidity-functions sizes))

;;; Helper function. Take a list of rigidity functions and a list of
;;; sizes for each one, and compute the total badness of the
;;; combination as the difference between the maximum force of any
;;; rigidity function at its size and the minimum force of any
;;; rigidity function at its size.
(defun badness (rigidity-functions sizes)
  (max-min-diff (compute-forces rigidity-functions sizes)))

;;; Helper function.  Take a list and a position of that list, and
;;; return a new list, which is like the one passed as an argument,
;;; except that the element in the position passed as the second
;;; argument has been incremented.
(defun add-one-to-pos (list pos)
  (loop for i from 0
	for element in list
	collect (+ element (if (= i pos) 1 0))))

;;; Helper function.  Take a list of rigidity functions and a list of
;;; current sizes, one for each rigidity function.  Return a list that
;;; is the same as the second one passed as an argument, except that
;;; one position has been incremented by one in such a way that the
;;; resulting list respresents the best individual sizes for each
;;; rigidity function, given that the sum of the individual sizes of
;;; each rigidity function is the sum of the elements of the resulting
;;; list.
(defun add-one (rigidity-functions sizes)
  (let ((min-badness (badness rigidity-functions (cons (1+ (car sizes)) (cdr sizes))))
	(min-pos 0))
    (loop for pos from 1 below (length sizes)
	  do (let ((new-badness (badness rigidity-functions (add-one-to-pos sizes pos))))
	       (when (< new-badness min-badness)
		 (setf min-badness new-badness)
		 (setf min-pos pos))))
    (add-one-to-pos sizes min-pos)))

;;; Take a list of rigidity functions considered to be combined in
;;; series, and a total size for all of those functions.  Return a
;;; list of non-negative integers, one for each function, in the same
;;; order as the initial functions, with the best size for each
;;; function.  The sum of the integers in the resulting list is equal
;;; to the total size given as an argument.
;;;
;;; Warning, this function is currently extremely inefficient.  
(defun sizes-in-series (rigidity-functions size)
  (let ((sizes (make-list (length rigidity-functions) :initial-element 0)))
    (loop repeat size
	  do (setf sizes (add-one rigidity-functions sizes)))
    sizes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Elementary functions.

(defparameter *rigid* 1000000)
(defparameter *elastic* (/ *rigid*))

(defun little-rigid (&key (factor 1))
  (assert (rationalp factor))
  (assert (plusp factor))
  `((0 . 0) ,(* *elastic* factor)))

(defun very-rigid (natural-size &key (factor 1))
  (assert (rationalp natural-size))
  (assert (plusp natural-size))
  (assert (rationalp factor))
  (assert (plusp factor))
  (let* ((rigidity (* *rigid* factor))
	 (zero-size-force (- (* rigidity natural-size))))
  `((0 . ,zero-size-force) ,rigidity)))

(defun rigid-contract (natural-size &key (expand-factor 1) (contract-factor 1))
  (assert (rationalp natural-size))
  (assert (plusp natural-size))
  (assert (rationalp expand-factor))
  (assert (plusp expand-factor))
  (assert (rationalp contract-factor))
  (assert (plusp contract-factor))
  (let* ((contract-rigidity (* *rigid* contract-factor))
	 (expand-rigidity (* *elastic* expand-factor))
	 (zero-size-force (- (* contract-rigidity natural-size))))
    `((0 . ,zero-size-force) (,natural-size . 0) ,expand-rigidity)))

(defun rigid-expand (natural-size &key (expand-factor 1) (contract-factor 1))
  (assert (rationalp natural-size))
  (assert (plusp natural-size))
  (assert (rationalp expand-factor))
  (assert (plusp expand-factor))
  (assert (rationalp contract-factor))
  (assert (plusp contract-factor))
  (let* ((contract-rigidity (* *elastic* contract-factor))
	 (expand-rigidity (* *rigid* expand-factor))
	 (zero-size-force (- (* contract-rigidity natural-size))))
    `((0 . ,zero-size-force) (,natural-size . 0) ,expand-rigidity)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test.

(defun random-non-negative-rational (n)
  (let ((denominator (1+ (random 100))))
    (/ (random (* n denominator)) denominator)))

(defun random-positive-rational (n)
  (let ((denominator (1+ (random 100))))
    (/ (1+ (random (* n denominator))) denominator)))

(defun random-rigidity-function ()
  (let ((f (- (random-non-negative-rational 100)))
	(s 0))
    `(,(make-point s f)
      ,@(loop repeat (random 5)
	      collect (make-point (incf s (random-positive-rational 100))
				  (incf f (random-positive-rational 100))))
      ,(random-positive-rational 100))))

(defun test-little-rigid ()
  (format t "little-rigid...")
  (force-output)
  (loop repeat 100000
	do (let* ((factor (random-positive-rational 100))
		  (fun (little-rigid :factor factor))
		  (size (random-non-negative-rational 100)))
	     (assert (= (force-at-size fun size)
			(* size factor *elastic*)))))
  (format t "~%"))

(defun test-very-rigid ()
  (format t "very-rigid...")
  (force-output)
  (loop repeat 100000
	do (let* ((factor (random-positive-rational 100))
		  (natural-size (random-positive-rational 100))
		  (fun (very-rigid natural-size :factor factor))
		  (size (random-non-negative-rational 100))
		  (zero-size-force (- (* natural-size factor *rigid*))))
	     (assert (= (force-at-size fun size)
			(+ (* size factor *rigid*) zero-size-force)))))
  (format t "~%"))

(defun test-rigid-contract ()
  (format t "rigid-contract...")
  (force-output)
  (loop repeat 100000
	do (let* ((expand-factor (random-positive-rational 100))
		  (contract-factor (random-positive-rational 100))
		  (natural-size (random-positive-rational 100))
		  (fun (rigid-contract natural-size
				       :expand-factor expand-factor
				       :contract-factor contract-factor))
		  (fun1 (very-rigid natural-size :factor contract-factor))
		  (fun2 (little-rigid :factor expand-factor))
		  (size (random-non-negative-rational 100)))
	     (if (> size natural-size)
		 (assert (= (force-at-size fun size)
			    (force-at-size fun2 (- size natural-size))))
		 (assert (= (force-at-size fun size)
			    (force-at-size fun1 size))))))
  (format t "~%"))

(defun test-rigid-expand ()
  (format t "rigid-expand...")
  (force-output)
  (loop repeat 100000
	do (let* ((expand-factor (random-positive-rational 100))
		  (contract-factor (random-positive-rational 100))
		  (natural-size (random-positive-rational 100))
		  (fun (rigid-expand natural-size
				     :expand-factor expand-factor
				     :contract-factor contract-factor))
		  (fun1 (little-rigid :factor contract-factor))
		  (fun2 (very-rigid natural-size :factor expand-factor))
		  (size (random-non-negative-rational 100)))
	     (if (> size natural-size)
		 (assert (= (force-at-size fun size)
			    (force-at-size fun2 size)))
		 (assert (= (- (force-at-size fun size)
			       (force-at-size fun 0))
			    (force-at-size fun1 size))))))
  (format t "~%"))

(defun test-size-at-force ()
  (format t "size-at-force...")
  (force-output)
  (loop repeat 100000
	do (let ((fun (random-rigidity-function))
		 (size (random-non-negative-rational 100)))
	     (let ((force (force-at-size fun size)))
	       (assert (= size
			  (size-at-force fun force))))))
  (format t "~%"))  
  

(defun test-combine-in-parallel ()
  (format t "combine-in-parallel...")
  (force-output)
  (loop repeat 100000
	do (let* ((n (1+ (random 10)))
		  (funs (loop repeat n collect (random-rigidity-function)))
		  (combined (combine-in-parallel funs))
		  (max-size (reduce #'max funs :key (lambda (fun)
						      (size (car (last fun 2))))))
		  (size (random-non-negative-rational (+ 2 (* 2 (ceiling max-size))))))
	     (assert (= (force-at-size combined size)
			(reduce #'+ funs :key (lambda (fun) (force-at-size fun size)))))))
  (format t "~%"))  

(defun test-combine-in-series ()
  (format t "combine-in-series...")
  (force-output)
  (loop repeat 100000
	do (let* ((n (1+ (random 10)))
		  (funs (loop repeat n collect (random-rigidity-function)))
		  (combined (combine-in-series funs))
		  (max-force (reduce #'max funs :key (lambda (fun)
						       (force (car (last fun 2))))))
		  (min-force (reduce #'min funs :key (lambda (fun)
						       (force (first fun)))))
		  (force (+ min-force (random-non-negative-rational
				       (1+ (round (- max-force min-force)))))))
	     (assert (= (size-at-force combined force)
			(reduce #'+ funs :key (lambda (fun) (size-at-force fun force)))))))
  (format t "~%"))  

(defun run-test ()
  (test-little-rigid)
  (test-very-rigid)
  (test-rigid-contract)
  (test-rigid-expand)
  (test-size-at-force)
  (test-combine-in-parallel)
  (test-combine-in-series))
   
  
  
