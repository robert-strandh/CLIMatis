(in-package #:clim3-sprawl)

(defclass sprawl ()
  ((%min-size :initform 0 :initarg :min-size :reader min-size)
   (%size :initform 0 :initarg :size :reader size)
   (%max-size :initform nil :initarg :max-size :reader max-size)))

(defun sprawl (min-size size max-size)
  (check-type min-size (integer 0))
  (check-type size (integer 0))
  (check-type max-size (or null (integer 0)))
  (assert (<= min-size size))
  (assert (or (null max-size) (<= size max-size)))
  (make-instance 'sprawl :min-size min-size :size size :max-size max-size))

(defun hugep (sprawl)
  (null (max-size sprawl)))

(defun combine-in-series (sprawls)
  (make-instance 'sprawl
     :min-size (reduce #'+ sprawls :key #'min-size)
     :size (reduce #'+ sprawls :key #'size)
     :max-size (if (some #'hugep sprawls)
		   nil
		   (reduce #'+ sprawls :key #'max-size))))

(defun combine-in-parallel (sprawls)
  (let ((min-size (reduce #'max sprawls :key #'min-size))
	(size (round (reduce #'+ sprawls :key #'size) (length sprawls)))
	(max-size (if (every #'hugep sprawls)
		      nil
		      (reduce #'min (remove-if #'hugep sprawls)
			      :key #'max-size))))
    (cond ((null max-size)
	   (make-instance 'sprawl
			  :min-size min-size
			  :size (max min-size size)
			  :max-size nil))
	  ((<= max-size min-size)
	   (let ((size (round (+ max-size min-size) 2)))
	     (make-instance 'sprawl
			    :min-size size
			    :size size
			    :max-size size)))
	  (t
	   (make-instance 'sprawl
			  :min-size min-size
			  :size (max min-size (min max-size size))
			  :max-size max-size)))))
	   
;;; When the size that we are given is smaller than the combined
;;; minimum sizes, we need to assign sizes that are smaller than the
;;; min size of each sprawl.  We do this by calculating a shrink
;;; factor which is the size that we are given divided by the combined
;;; minimum sizes.  We then assign as size that is the min-size of
;;; each sprawl multiplied by the shrink factor, everything rounded
;;; nicely to integers.
(defun sizes-in-series-less-than-min (size sprawls)
  (let* ((factor (/ size (reduce #'+ sprawls :key #'min-size)))
	 ;; Build a list where each entry is a list of three things:
	 ;; The assigned size, the sprawl, and its position in the
	 ;; original list.
	 (tagged (loop for sprawl in sprawls
		       for i from 0
		       collect (list (ceiling (* factor (min-size sprawl)))
				     sprawl
				     i)))
	 (diff (- (reduce #'+ tagged :key #'car) size)))
    ;; We may need to decrease the assigned sizes a bit.  Start by
    ;; sorting the entries by decreasing assigned sizes.
    (setf tagged (sort tagged #'> :key #'car))
    (loop repeat diff
	  do ;; Decrease the size of the largest one by one
	     ;; unit.
	     (decf (car (car tagged)))
	     ;; Reestablish the order by decreasing size.
	     (setf tagged
		   (merge 'list (cdr tagged) (list (car tagged))
			  #'> :key #'car)))
    (mapcar #'car (sort tagged #'< :key #'caddr))))

;;; FIXME: comment is wrong.
;;; When the size that we are given is smaller than the combined
;;; preferred sizes, but larger than the combined minimum sizes, we
;;; need to assign sizes that are between the minimum size and the
;;; preferred size of each sprawl.  We do this by calculating a shrink
;;; factor which is the size that we are given divided by the
;;; difference between the combined preffered sizes and the combined
;;; minimum sizes.  We then assign as size to each sprawl that is the
;;; min-size plus the shrink factor multipled by the difference
;;; between between the preferred and the minimum size. 
(defun sizes-in-series-less-than-preferred (size sprawls)
  (let* ((factor (/ (- size (reduce #'+ sprawls :key #'min-size))
		    (- (reduce #'+ sprawls :key #'size)
		       (reduce #'+ sprawls :key #'min-size))))
	 ;; Build a list where each entry is a list of three things:
	 ;; The assigned size, the sprawl, and its position in the
	 ;; original list.
	 (tagged (loop for sprawl in sprawls
		       for i from 0
		       collect (list (ceiling
				      (+ (min-size sprawl)
					 (* factor (- (size sprawl)
						      (min-size sprawl)))))
				     sprawl
				     i)))
	 (diff (- (reduce #'+ tagged :key #'car) size)))
    ;; We may need to decrease the assigned sizes a bit.  Start by
    ;; sorting the entries by decreasing assigned sizes.
    (setf tagged (sort tagged #'> :key #'car))
    (loop repeat diff
	  do ;; Decrease the size of the largest one by one
	     ;; unit.
	     (decf (car (car tagged)))
	     ;; Reestablish the order by decreasing size.
	     (setf tagged
		   (merge 'list (cdr tagged) (list (car tagged))
			  #'> :key #'car)))
    (mapcar #'car (sort tagged #'< :key #'caddr))))

(defun sizes-in-series-preferred-and-stretch (size sprawls)
  (let* ((tagged (loop for sprawl in sprawls
		       collect (list (size sprawl) sprawl)))
	 (huge (remove-if-not #'hugep tagged :key #'cadr))
	 (assigned-size (reduce #'+ sprawls :key #'size)))
    (multiple-value-bind (quotient remainder)
	(floor (- size assigned-size) (length huge))
      (loop for entry in huge
	    do (incf (car entry) quotient))
      (loop for entry in huge
	    repeat remainder
	    do (incf (car entry))))
    (mapcar #'car tagged)))

(defun sizes-in-series-less-than-max (size sprawls)
  (let* ((factor (/ (- size (reduce #'+ sprawls :key #'size))
		    (- (reduce #'+ sprawls :key #'max-size)
		       (reduce #'+ sprawls :key #'size))))
	 ;; Build a list where each entry is a list of three things:
	 ;; The assigned size, the sprawl, and its position in the
	 ;; original list.
	 (tagged (loop for sprawl in sprawls
		       for i from 0
		       collect (list (ceiling
				      (+ (size sprawl)
					 (* factor (- (max-size sprawl)
						      (size sprawl)))))
				     sprawl
				     i)))
	 (diff (- (reduce #'+ tagged :key #'car) size)))
    ;; We may need to decrease the assigned sizes a bit.  Start by
    ;; sorting the entries by decreasing assigned sizes.
    (setf tagged (sort tagged #'> :key #'car))
    (loop repeat diff
	  do ;; Decrease the size of the largest one by one
	     ;; unit.
	     (decf (car (car tagged)))
	     ;; Reestablish the order by decreasing size.
	     (setf tagged
		   (merge 'list (cdr tagged) (list (car tagged))
			  #'> :key #'car)))
    (mapcar #'car (sort tagged #'< :key #'caddr))))

(defun sizes-in-series-greater-than-max (size sprawls)
  (let* ((factor (/ size (reduce #'+ sprawls :key #'max-size)))
	 ;; Build a list where each entry is a list of three things:
	 ;; The assigned size, the sprawl, and its position in the
	 ;; original list.
	 (tagged (loop for sprawl in sprawls
		       for i from 0
		       collect (list (ceiling (* factor (max-size sprawl)))
				     sprawl
				     i)))
	 (diff (- (reduce #'+ tagged :key #'car) size)))
    ;; We may need to decrease the assigned sizes a bit.  Start by
    ;; sorting the entries by decreasing assigned sizes.
    (setf tagged (sort tagged #'> :key #'car))
    (loop repeat diff
	  do ;; Decrease the size of the largest one by one
	     ;; unit.
	     (decf (car (car tagged)))
	     ;; Reestablish the order by decreasing size.
	     (setf tagged
		   (merge 'list (cdr tagged) (list (car tagged))
			  #'> :key #'car)))
    (mapcar #'car (sort tagged #'< :key #'caddr))))

(defun sizes-in-series (size sprawls)
  (let ((combination (combine-in-series sprawls)))
    (cond ((= size (size combination))
	   (mapcar #'size sprawls))
	  ((= size (min-size combination))
	   (mapcar #'min-size sprawls))
	  ((< size (min-size combination))
	   (sizes-in-series-less-than-min size sprawls))
	  ((< size (size combination))
	   (sizes-in-series-less-than-preferred size sprawls))
	  ((null (max-size combination))
	   (sizes-in-series-preferred-and-stretch size sprawls))
	  ((< size (max-size combination))
	   (sizes-in-series-less-than-max size sprawls))
	  ((= size (max-size combination))
	   (mapcar #'max-size combination))
	  (t
	   (sizes-in-series-greater-than-max size sprawls)))))
