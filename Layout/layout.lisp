(in-package #:clim3-layout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vbox 

(defclass vbox (clim3-zone:compound-sequence-zone
		clim3-zone:dependent-gives-mixin
		clim3-zone:any-number-of-children-mixin)
  ())

(defmethod clim3-zone:combine-child-gives ((zone vbox))
  (setf (clim3-zone:vgive zone)
	(rigidity:combine-in-series
	 (mapcar #'clim3-zone:vgive (clim3-zone:children zone))))
  (setf (clim3-zone:hgive zone)
	(rigidity:combine-in-parallel
	 (mapcar #'clim3-zone:hgive (clim3-zone:children zone)))))
  
(defmethod clim3-zone:impose-layout ((zone vbox) hpos vpos width height)
  (declare (ignore hpos vpos))
  (let* ((children (clim3-zone:children zone))
	 (vertical-gives (mapcar #'clim3-zone:vgive children))
	 (heights (rigidity:sizes-in-series vertical-gives height)))
    (loop for vpos = 0 then (+ vpos height)
	  for height in heights
	  for child in children
	  do (clim3-zone:impose-layout child 0 vpos width height))))

(defun vbox (children)
  (make-instance 'vbox :children children))

(defun vbox* (&rest children)
  (make-instance 'vbox :children children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hbox 

(defclass hbox (clim3-zone:compound-sequence-zone
		clim3-zone:dependent-gives-mixin
		clim3-zone:any-number-of-children-mixin)
  ())

(defmethod clim3-zone:combine-child-gives ((zone hbox))
  (setf (clim3-zone:vgive zone)
	(rigidity:combine-in-parallel
	 (mapcar #'clim3-zone:vgive (clim3-zone:children zone))))
  (setf (clim3-zone:hgive zone)
	(rigidity:combine-in-series
	 (mapcar #'clim3-zone:hgive (clim3-zone:children zone)))))
  
(defmethod clim3-zone:impose-layout ((zone hbox) hpos vpos width height)
  (declare (ignore hpos vpos))
  (let* ((children (clim3-zone:children zone))
	 (horizontal-gives (mapcar #'clim3-zone:hgive children))
	 (widths (rigidity:sizes-in-series horizontal-gives width)))
    (loop for hpos = 0 then (+ hpos width)
	  for width in widths
	  for child in children
	  do (clim3-zone:impose-layout child hpos 0 width height))))

(defun hbox (children)
  (make-instance 'hbox :children children))

(defun hbox* (&rest children)
  (make-instance 'hbox :children children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pile

(defclass pile (clim3-zone:compound-sequence-zone
		clim3-zone:dependent-gives-mixin
		clim3-zone:any-number-of-children-mixin)
  ())

(defmethod clim3-zone:combine-child-gives ((zone pile))
  (setf (clim3-zone:vgive zone)
	(rigidity:combine-in-parallel
	 (mapcar #'clim3-zone:vgive (clim3-zone:children zone))))
  (setf (clim3-zone:hgive zone)
	(rigidity:combine-in-parallel
	 (mapcar #'clim3-zone:hgive (clim3-zone:children zone)))))
  
(defmethod clim3-zone:impose-layout ((zone pile) hpos vpos width height)
  (declare (ignore hpos vpos))
  (let ((children (clim3-zone:children zone)))
    (loop for child in children
	  do (clim3-zone:impose-layout child 0 0 width height))))

(defun pile (children)
  (make-instance 'pile :children children))

(defun pile* (&rest children)
  (make-instance 'pile :children children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Grid

(defclass grid (clim3-zone:compound-zone
		clim3-zone:dependent-gives-mixin
		clim3-zone:any-number-of-children-mixin)
  ((%combined-rows :initform nil :accessor combined-rows)
   (%combined-cols :initform nil :accessor combined-cols)))

(defmethod clim3-zone:combine-child-gives ((zone grid))
  (let* ((children (clim3-zone:children zone))
	 (rows (array-dimension children 0))
	 (cols (array-dimension children 1)))
    (cond ((= rows 1)
	   (setf (combined-cols zone)
		 (loop for col from 0 below cols
		       collect (clim3-zone:hgive (aref children 0 col))))
	   (setf (clim3-zone:hgive zone)
		 (rigidity:combine-in-series (combined-cols zone)))
	   (setf (clim3-zone:vgive zone)
		 (rigidity:combine-in-parallel
		  (loop for col from 0 below cols
			collect (clim3-zone:vgive (aref children 0 col))))))
	  ((= cols 1)
	   (setf (combined-rows zone)
		 (loop for row from 0 below rows
		       collect (clim3-zone:vgive (aref children 0 row))))
	   (setf (clim3-zone:hgive zone)
		 (rigidity:combine-in-parallel
		  (loop for row from 0 below rows
			collect (clim3-zone:hgive (aref children 0 row)))))
	   (setf (clim3-zone:vgive zone)
		 (rigidity:combine-in-series (combined-rows zone))))
	  (t
	   (setf (combined-rows zone)
		 (loop for row from 0 below rows
		       collect (rigidity:combine-in-parallel
				(loop for col from 0 below cols
				      collect (clim3-zone:vgive (aref children row col))))))
	   (setf (combined-cols zone)
		 (loop for col from 0 below cols
		       collect (rigidity:combine-in-parallel
				(loop for row from 0 below rows
				      collect (clim3-zone:hgive (aref children row col))))))
	   (setf (clim3-zone:hgive zone)
		 (rigidity:combine-in-series (combined-cols children)))
	   (setf (clim3-zone:vgive zone)
		 (rigidity:combine-in-series (combined-rows children)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bboard
;;;
;;; This a zone that lets its children be positioned wherever they want.

(defclass bboard (clim3-zone:compound-zone
		  clim3-zone:independent-gives-mixin
		  clim3-zone:any-number-of-children-mixin)
  ()
  (:default-initargs :vgive (rigidity:little-rigid)
		     :hgive (rigidity:little-rigid)))

(defmethod clim3-zone:combine-child-gives ((zone bboard))
  nil)

(defmethod clim3-zone:impose-layout ((zone bboard) hpos vpos width height)
  (declare (ignore hpos vpos width height))
  (loop for child in (clim3-zone:children zone)
	do (multiple-value-bind (width height)
	       (clim3-zone:natural-size child)
	     (clim3-zone:impose-layout
	      child
	      (clim3-zone:hpos child) (clim3-zone:vpos child)
	      width height))))

(defun bboard (children)
  (make-instance 'bboard :children children))

(defun bboard* (&rest children)
  (make-instance 'bboard :children children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sponge.  A very elastic zone.

(defclass sponge (clim3-zone:atomic-zone)
  ()
  (:default-initargs :hgive (rigidity:little-rigid)
		     :vgive (rigidity:little-rigid)))

(defun sponge ()
  (make-instance 'sponge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vbrick and hbrick.  Very rigid zones. 

(defclass vbrick (clim3-zone:atomic-zone)
  ()
  (:default-initargs :hgive (rigidity:little-rigid)))

(defmethod initialize-instance :after ((zone vbrick) &key (height nil height-p))
  (unless height-p
    (error "No natural height given."))
  (check-type height (real (0)))
  (setf (clim3-zone:vgive zone) (rigidity:very-rigid height)))
  
(defun vbrick (height)
  (make-instance 'vbrick :height height))

(defclass hbrick (clim3-zone:atomic-zone)
  ()
  (:default-initargs :vgive (rigidity:little-rigid)))

(defmethod initialize-instance :after ((zone hbrick) &key (width nil width-p))
  (unless width-p
    (error "No natural width given."))
  (check-type width (real (0)))
  (setf (clim3-zone:hgive zone) (rigidity:very-rigid width)))
  
(defun hbrick (width)
  (make-instance 'hbrick :width width))


