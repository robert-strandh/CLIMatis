(cl:in-package #:clim3-rendering)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generating trapezoids from a collection of polygons. 
;;;
;;; Each polygon in the collection must be simple.  Furthermore, two
;;; polygons can only overlap in very simple ways.  One polygon can be
;;; entirely contained in another polygon, and two polygons can touch,
;;; but their permiters can not cross.
;;;
;;; Initially, each polygon is represented as a list of points
;;; represented as CONS cells of the form (x . y) where x and y are
;;; the coordinates of the point in the form of real numbers.  The
;;; order of the points in the list is the order of the points around
;;; the permieter of the polygon, but the direction currently does not
;;; matter.  The last point in the list is considered to be next to
;;; the firsts point in the list.
;;;
;;; To compute the trapezoids, we introduce the concept of a CHAIN and
;;; a CHAIN PAIR.  A chain is a sequence of points with non-decreasing
;;; y coordinates.  A further restriction of a chain is that the last
;;; two points of the chain have different y coordinates.  A chain
;;; pair, is an ordered pair of chains such that first point of each
;;; chain has the same y coordinate, and the x coordinate of the first
;;; point in the LEFT chain of the chain pair is less than or equal to
;;; the x coordinate of the first point of the RIGHT chain of the
;;; chain pair.

;;; To represent a chain, we use the class POINT, which also contains
;;; a reference to the next point in the chain, or NIL if this is the
;;; last point of the chain.
(defclass point ()
  ((%x :initarg :x :accessor x)
   (%y :initarg :y :accessor y)
   (%next :initform nil :initarg :next :accessor next)))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "x: ~a  y: ~a next: ~s"
	    (x object) (y object) (next object))))

;;; Coerce the list of points so that the coordinates are double floats.
(defun coerce-points (points)
  (loop for (x . y) in points
	collect (cons (coerce x 'double-float) (coerce y 'double-float))))

;;; Return true if and only if there are at least three distinct points.
(defun three-distinct-points-p (points)
  (>= (length (remove-duplicates points :test #'equal)) 3))

;;; Return true if and only if there are at least two distinct y-values.
(defun two-distinct-y-values-p (point)
  (>= (length (remove-duplicates point :key #'cdr :test #'equal)) 2))

;;; Once we have the points of a polygon as a cleaned-up list of CONS
;;; cells of double floats, we turn that list into a circular list, so
;;; as to make it easier to process pairs or tripples of adjacent
;;; points.  
(defun make-circular (list)
  (setf (cdr (last list)) list))

;;; To avoid infinite loops, we need to know how many distinct
;;; elements there are in a circular list of points.  The method used
;;; here assumes that the first element is part of the cycle, so that
;;; we disallow a prefix of the list which is not part of the cycle. 
;;;
;;; Notice that we can not use the FIND or the POSITION families of
;;; functions, because they require their arguments to be proper
;;; sequences, and a circular list is not a proper sequence.
(defun element-count (circular-list)
  (1+ (loop for rest = (cdr circular-list) then (cdr rest)
	    until (eq rest circular-list)
	    count t)))

;;; Eliminate multiple consecuitive points.  This is one of the
;;; cleanups that we do before some more serious processing.  Without
;;; this cleanup, we might run the risk of dividing by 0 to compute
;;; line slopes.
(defun eliminate-multiple-consecutive-points (points)
  (loop with rest = points
	repeat (element-count points)
	do (if (equal (car rest) (cadr rest))
	       (setf (cdr rest) (cddr rest))
	       (setf rest (cdr rest)))
	finally (return rest)))
	     
;;; When there are three or more consecutive points with the same
;;; y-value, only keep the first and the last.  This cleanup is
;;; crucial to apply before further processing, because we rely on the
;;; fact that any horizontal line segment is preceded and followed by
;;; line segments that are not horizontal.
(defun merge-consecutive-horizontal-lines (points)	       
  (loop with rest = points
	repeat (element-count points)
	do (if (and (= (cdr (car rest))
		       (cdr (cadr rest))
		       (cdr (caddr rest))))
	       (setf (cdr rest) (cddr rest))
	       (setf rest (cdr rest)))
	finally (return rest)))

;;; Take the sequence of points of a polygon, and return a list of
;;; chain pairs.  The chain pairs returned are in no particular order.
;;; When there is a extreme point of the polygon that is surronded by
;;; two points with strictly greater y coordinates, then the first
;;; point of each chain pair will have the coordinates of that extreme
;;; point, so in some sense it is duplicated.  If, on the other hand,
;;; we have an extreme PLATEU, i.e, there are two extreme points p1
;;; and p2 connected with the same y coordinates, such that the
;;; predecessor of p1 and the successor of p2 both have y coordinate
;;; that are strictly greater than that of p1 and p2, then the
;;; coordinates of p1 will be in one chain of the chain pair and the
;;; coordinates of p2 will be in the other.  By doing it this way, we
;;; guarantee that the second point of each chain pair has a y
;;; coordinate that is strictly greater than the y coordinate of the
;;; first point. of the chain. 
(defun points-to-chain-pairs (points)
  (unless (three-distinct-points-p points)
    (return-from points-to-chain-pairs '()))
  (unless (two-distinct-y-values-p points)
    (return-from points-to-chain-pairs '()))
  (make-circular points)
  (setf points (eliminate-multiple-consecutive-points points))
  (setf points (merge-consecutive-horizontal-lines points))
  ;; For a while, we are going to keep the number of points in the
  ;; circular list intact, so we avoid traversing the cycle several
  ;; times by computing the number of elements once and for all. 
  (let ((count (element-count points)))
    ;; Step one is to convert the points represented as CONS cells of
    ;; the form (x . y) into instance of the POINT class, each one
    ;; with a NEXT slot of NIL.
    (loop for rest = points then (cdr rest)
	  repeat count
	  do (setf (car rest)
		   (make-instance 'point :x (car (car rest)) :y (cdr (car rest)))))
    ;; Step two is to take two consecutive points p1 and p2 and make
    ;; the NEXT slot of p2 point to p1 if p1 has a y coordinate that
    ;; is strictly greater than that of p2.  
    (loop for (p1 p2) on points
	  repeat count
	  do (when (> (y p1) (y p2))
	       (setf (next p2) p1)))
    ;; Step three is to take two consecutive points p1 and p2 and make
    ;; the NEXT slot of p1 point to p2 if p2 has a y coordinate that
    ;; is strictly greater than that of p1.  Notice that this step
    ;; will undo some work done in step 1.  A point that is surrounded
    ;; by points that both have a larger y coordinate will have
    ;; pointed to its predecessor after step two, and now it will
    ;; point to its successor.
    (loop for (p1 p2) on points
	  repeat count
	  do (when (> (y p2) (y p1))
	       (setf (next p1) p2)))
    ;; After the previous steps, if a point p has a successor with a y
    ;; coordinate that is stricly greater than that of p, then the
    ;; NEXT slot of p points to the successor.  OTHERWISE, if p has a
    ;; predecessor with a y coordinate that is stricly greater than
    ;; that of p, then the NEXT slot of p points to the predecessor. 
    ;;
    ;; After the previous steps, When a point p has a NEXT slot of
    ;; NIL, then at least one of the surrounding points has a y
    ;; coordinate that is strictly smaller than that of p.  The other
    ;; surrounding point has a y coordinate that might be strictly
    ;; smaller, or it might be equal to that of p.  Because we
    ;; eliminated subsequences of three consecutive points with the
    ;; same y coordinate, it can not be the case that a point p with a
    ;; NEXT field of NIL is surrounded by two points with the same y
    ;; coordinate.
    ;;
    ;; The next step takes each pair of consecuitve points p1 and p2
    ;; with the same y coordinate and makes the final assignment of
    ;; the NEXT fields as follow.  If the next field of p1 is NIL but
    ;; the next field of p2 is not, then the predecessor of p1 must
    ;; have a y coordinate that is less than that of p1 (otherwise, it
    ;; would be greater, and then p1's NEXT field would point to it),
    ;; so then we set the NEXT field of p1 to point to p2.  If the
    ;; next field of p2 is NIL but the next field of p1 is not, then
    ;; the successor of p2 must have a y coordinate that is less
    ;; than that of p2 (otherwise, it would be greater, and then p2's
    ;; NEXT field would point to it), so then we set the NEXT field of
    ;; p2 to point to p1.
    (loop for (p1 p2) on points
	  repeat count
	  do (when (= (y p1) (y p2))
	       (when (and (null (next p1)) (not (null (next p2))))
		 (setf (next p1) p2))
	       (when (and (null (next p2)) (not (null (next p1))))
		 (setf (next p2) p1))))
    ;; Now, we have the following situation.  First, no chain is ended
    ;; by two points with the same y coordinate.  
    ;;
    ;; Two consecutive points p1 and p2 with the same y coordinate,
    ;; where the NEXT field of p2 is not NIL and it does not point to
    ;; p1 (so it points to the successor of p2), and the NEXT field of
    ;; p1 does not point to p2 (it is either NIL or it points to the
    ;; predecessor of p1) represent an extreme PLATEAU.  We turn them
    ;; into a chain pair.  We check the x coordinates to respect the
    ;; restriction on chain pairs.
    ;;
    ;; Two consecutive points p1 and p2 where p1 has a y coordinate
    ;; that is strictly greater than that of p2, by the NEXT field of
    ;; p2 does not point to p1 represent an extreme point in p2.  We
    ;; construct a chain pair by duplicating p2 and making the NEXT
    ;; field of the copy point to p1.  We check the x coordinates to
    ;; respect the restriction on chain pairs.
    (let ((chain-pairs '()))
      (loop for (p1 p2) on points
	    repeat count
	    do (cond ((and (= (y p1) (y p2))
			   (not (null (next p2)))
			   (not (eq (next p2) p1))
			   (not (eq (next p1) p2)))
		      (push (if (< (x p1) (x p2))
				(cons p1 p2)
				(cons p2 p1))
			    chain-pairs))
		     ((and (> (y p1) (y p2))
			   (not (eq (next p2) p1)))
		      (let ((pp2 (make-instance 'point
						:x (x p2) :y (y p2) :next p1)))
			(push (if (< (x p1) (x (next p2)))
				  (cons pp2 p2)
				  (cons p2 pp2))
			      chain-pairs)))
		     (t nil)))
      chain-pairs)))
	
;;; We guarantee that the second point of each chain has a y
;;; coordinate that is strictly greater than the y coordinate of the
;;; first point of the chain.  We must guarantee this so that the
;;; slopes compted are always finite.
;;;
;;; Furthermore, the y coordinate given as an argument is less than or
;;; equal to the y coordinate of the second point of both the left and
;;; the right chain of the chain pair.  
;;;
;;; One could imagine optimizing this routine a bit in case the y
;;; coordinate given as agument is equal to that of the second point
;;; of either the left or the right chain, because then, we would not
;;; need to compute the slopes, and we could just take the coordinates
;;; of the second point.
;;;
;;; The trapezoid returned is of the form (yt yb xtl xbl xtr xbr). 
(defun trapezoid-from-chain-pair (chain-pair y)
  (let* ((left (car chain-pair))
	 (right (cdr chain-pair))
	 (left-slope (/ (- (x (next left)) (x left))
			(- (y (next left)) (y left))))
	 (right-slope (/ (- (x (next right)) (x right))
			 (- (y (next right)) (y right)))))
    (list (y left) y
	  (x left) (+ (x left) (* left-slope (- y (y left))))
	  (x right) (+ (x right) (* right-slope (- y (y right)))))))
	   
;;; Generate a list of trapezoids, one for each chain pair in the
;;; argument given.
(defun generate-trapezoids (active-chain-pairs y)
  (loop for chain-pair in active-chain-pairs
	collect (trapezoid-from-chain-pair chain-pair y)))

;;; A chain pair has expired for a particular y coordinate if each
;;; chain of the pair has exactly two points and the the y coordinate
;;; of the second point of each chain of the pair is the y passed as
;;; an argument.  Then either the second point of each chain is
;;; identical, or the two points are connected by a horizontal line in
;;; the original polygon. 
;;;
;;; A chain pair that is expired can no longer generate any trapezoids
;;; so it is removed from the set of active chain pairs.
(defun chain-pair-expired-p (chain-pair y)
  (let ((left (car chain-pair))
	(right (cdr chain-pair)))
    (and (null (next (next left)))
	 (null (next (next right)))
	 (= y (y (next left)))
	 (= y (y (next right))))))

;;; Return a new list of chain pairs with the expired chain pairs
;;; removed.
(defun remove-expired-chain-pairs (chain-pairs y)
  (remove-if (lambda (chain-pair) (chain-pair-expired-p chain-pair y)) chain-pairs))

;;; Two consecuitive chain pairs cp1 and cp2 must be merged when the
;;; right chain of cp1 and the left chain of cp2 both reach their
;;; ends.  This will happend when the right chain of cp1 and the left
;;; chain of cp1 both contain exactly two points, and the y
;;; coordinates of the second point of each chain is the same as the y
;;; passed as an argument.
;;;
;;; We merge by destructivel modifying the list of chain pairs so that
;;; the right chain of cp1 is replaced by the left chain of cp2, and
;;; then removing cp2 from the list. 
(defun merge-chain-pairs (chain-pairs y)
  (loop for rest = chain-pairs then (cdr rest)
	until (or (null rest) (null (cdr rest)))
	do (when (and (null (next (next (cdr (car rest)))))
		      (= y (y (next (cdr (car rest)))))
		      (null (next (next (car (cadr rest)))))
		      (= y (y (next (car (cadr rest))))))
	     (setf (cdr (car rest)) (cdr (cadr rest)))
	     (setf (cdr rest) (cdr (cdr rest))))))

;;; After expired chain pairs have been removed, and consecutive chain
;;; pairs have been merged when appropriate, we modify each chain pair
;;; so that the first point of both the left and the right chain has
;;; the y value passed as an argument.
;;;
;;; It is possible that the second point of one of the chains has
;;; exactly that y value, in which case the first point is eliminated.
;;; Otherwise, the first point is modified accordinly, taking into
;;; account the slope of the line segment. 
;;;
;;; While it is not possible for the first and the second point of a
;;; chain to have the same y coordinate (because of the way we define
;;; a chain), this restriction does not apply to the second and the
;;; third point.  Therefore, when the first point is eliminated
;;; entirely, we must restore the restriction by eliminating an
;;; additional point when the original second and third points had the
;;; same y coordinate.  We only have to check once, because we
;;; guarantee that no three consecutive points have the same y
;;; coordinate.
(defun advance-to-y (chain-pair y)
  (let ((left (car chain-pair))
	(right (cdr chain-pair)))
    (if (= y (y (next left)))
	(progn (setf (car chain-pair) (next left))
	       (when (and (not (null (next (car chain-pair))))
			  (= y (y (next (car chain-pair)))))
		 (setf (car chain-pair) (next (car chain-pair)))))
	(let ((left-slope (/ (- (x (next left)) (x left))
			     (- (y (next left)) (y left)))))
	  (setf (x left)
		(+ (x left) (* left-slope (- y (y left)))))
	  (setf (y left) y)))
    (if (= y (y (next right)))
	(progn (setf (cdr chain-pair) (next right))
	       (when (and (not (null (next (cdr chain-pair))))
			  (= y (y (next (cdr chain-pair)))))
		 (setf (cdr chain-pair) (next (cdr chain-pair)))))
	(let ((right-slope (/ (- (x (next right)) (x right))
			     (- (y (next right)) (y right)))))
	  (setf (x right)
		(+ (x right) (* right-slope (- y (y right)))))
	  (setf (y right) y)))))
    
;;; For each chain pair of a list of chain pairs, advance it to the
;;; new y coordinate.  Expired chain pairs have already been removed,
;;; and consecuitive chain pairs have been merged when appropriate.
(defun advance-chain-pairs-to-y (chain-pairs y)
  (mapc (lambda (chain-pair) (advance-to-y chain-pair y)) chain-pairs))
	      
(defun introduce-chain-pair (active-chain-pairs new-chain-pair)
  (let ((pos (position-if (lambda (chain-pair)
			    (and (> (x (car new-chain-pair)) (x (car chain-pair)))
				 (< (x (cdr new-chain-pair)) (x (cdr chain-pair)))))
			  active-chain-pairs)))
    (if (not (null pos))
	(let ((old-chain-pair (elt active-chain-pairs pos)))
	  (append (subseq active-chain-pairs 0 pos)
		  (list (cons (car old-chain-pair) (car new-chain-pair))
			(cons (cdr new-chain-pair) (cdr old-chain-pair)))
		  (subseq active-chain-pairs (1+ pos))))
	(merge 'list active-chain-pairs (list new-chain-pair)
	       #'< :key (lambda (chain-pair) (x (car chain-pair)))))))

;;; We are given a list of chain pairs, sorted by increasing y
;;; coordinate, and a list of y coordinates of the points of the
;;; original polygons.  There are no duplicates in the list of y
;;; coordinates, and the list is sorted in increasing order.
;;;
;;; We initiate the iterative process by taking all the chain pairs
;;; with the smallest y coordinate and putting them into a list of
;;; ACTIVE chain pairs.  We then elminate the first y coordinate of
;;; the list.
;;;
;;; In each iteration, we take another y off the list and use as follows:
;;;   * We generate trapezoids from the active chain pairs.
;;;   * We then remove expired chain pairs.
;;;   * Next, we merge consecutive chain pairs when appropriate.
;;;   * Then, we advance the resulting active chain pairs to the y.
;;;   * Finally, we introduce any new chain pair that starts with
;;;     the new y coordinate into the list of active chain pairs.
(defun trapezoids-from-chain-pairs (chain-pairs y-coordinates)
  (let ((active-chain-pairs '()))
    (loop while (and (not (null chain-pairs))
		     (= (car y-coordinates) (y (car (car chain-pairs)))))
	  do (push (pop chain-pairs) active-chain-pairs))
    ;; sort the active chain-pairs by x coordinate
    (setf active-chain-pairs
	  (sort active-chain-pairs #'<
		:key (lambda (chain-pair) (x (car chain-pair)))))
    (pop y-coordinates)
    (let ((trapezoids '()))
      (loop for y in y-coordinates
	    do (setf trapezoids
		     (append trapezoids (generate-trapezoids active-chain-pairs y)))
	       (setf active-chain-pairs
		     (remove-expired-chain-pairs active-chain-pairs y))
	       (merge-chain-pairs active-chain-pairs y)
	       (advance-chain-pairs-to-y active-chain-pairs y)
	       (loop while (and (not (null chain-pairs))
				(= y (y (car (car chain-pairs)))))
		     do (setf active-chain-pairs
			      (introduce-chain-pair active-chain-pairs (pop chain-pairs)))))
      trapezoids)))

;;; The argument is a polygon, which is a list of points of the form
;;; (x . y) where x and y are both double floats.  The result is a
;;; list of the y coordinates of all the points, in any old order, and
;;; possibly with duplicates.
(defun y-coordinates-of-polygon (polygon)
  (mapcar #'cdr polygon))

;;; The argument is a list of polygons.  Each polygon is a list of
;;; points of the form (x . y) where x and y are both double floats.
;;; The return value is a list of all the y coordinates of the points
;;; in all polygons.  The values are in any order and there may be
;;; duplicates.
(defun y-coordinates-of-polygons (polygons)
  (reduce #'append (mapcar #'y-coordinates-of-polygon polygons)))

;;; The argument is a list of polygons.  Each polygon is a list of
;;; points of the form (x . y) where x and y are real numbers.  Each
;;; polygon must be simple.  The line segments of two polygons must
;;; not cross, but they can touch.  It is allowed for one polygon to
;;; be entirely inside another polygon.  The result is a list of
;;; trapezoids of the form (yt yb xtl xbl xtr xbr) where yt and yb are
;;; the top and bottom y coordinates of the trapezoid, with yt < yb,
;;; xtl and xbl are the top and bottom left x coordinates of the
;;; trapezoid and xtr and xbr are the top and bottom right x
;;; coordinates.  xtl <= xtr and xbl <= xbr.  The trapezoid may be
;;; degenerate so that xtl = xtr or xbr = rbr or both. 
(defun trapezoids-from-polygons (polygons)
  (let* ((float-polygons (mapcar #'coerce-points polygons))
	 (y-coordinates (y-coordinates-of-polygons float-polygons))
	 (chain-pairs (mapcar #'points-to-chain-pairs float-polygons))
	 (unique-ys (sort (remove-duplicates y-coordinates) #'<)))
    (trapezoids-from-chain-pairs
     (sort (reduce #'append chain-pairs)
	   #'< :key (lambda (chain-pair) (y (car chain-pair))))
     unique-ys)))
