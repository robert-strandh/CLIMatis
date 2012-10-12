(in-package #:clim3-layout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities

(defun coerce-to-list-of-at-most-one-zone (thing)
  (let ((result
	  (cond ((or (null thing) (and (consp thing) (null (cdr thing))))
		 thing)
		((consp thing)
		 (error "The list can have at most one element ~s"
			thing))
		((and (vectorp thing) (< (length thing) 2))
		 (coerce thing 'list))
		((vectorp thing)
		 (error "The vector can have at most one element ~s"
			thing))
		(t
		 (error "A proper sequence of length at most 1 required ~s"
			thing)))))
    (unless (null result)
      (unless (clim3-zone:zone-p (car result))
	(error "A zone was expected ~s" (car result))))
    result))

;;; For now, this is a duplication of the one in Zone/zone.lisp
(defun proper-list-p (object)
  (let ((cells '())
	(rest object))
    (loop until (atom rest)
	  do (if (member rest cells :test #'eq)
		 (return-from proper-list-p nil)
		 (progn (push rest cells)
			(pop rest))))
    (null rest)))

(defun coerce-to-list-of-zones (thing)
  (let ((result
	  (cond ((proper-list-p thing)
		 thing)
		((vectorp thing)
		 (coerce thing 'list))
		(t
		 (error "A proper sequence required ~s" thing)))))
    (loop for element in result
	  do (unless (clim3-zone:zone-p element)
	       (error "a zone was expected ~s" element)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VBOX. 

(defclass vbox (clim3-zone:compound-sequence-zone
		clim3-zone:dependent-gives-mixin
		clim3-zone:any-number-of-children-mixin)
  ())

(defmethod clim3-zone:combine-child-gives ((zone vbox))
  (setf (clim3-zone:hgive zone)
	(rigidity:combine-in-parallel
	 (mapcar #'clim3-zone:hgive (clim3-zone:children zone))))
  (setf (clim3-zone:vgive zone)
	(rigidity:combine-in-series
	 (mapcar #'clim3-zone:vgive (clim3-zone:children zone)))))
  
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
  (make-instance 'vbox :children (coerce-to-list-of-zones children)))

(defun vbox* (&rest children)
  (make-instance 'vbox :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HBOX.

(defclass hbox (clim3-zone:compound-sequence-zone
		clim3-zone:dependent-gives-mixin
		clim3-zone:any-number-of-children-mixin)
  ())

(defmethod clim3-zone:combine-child-gives ((zone hbox))
  (setf (clim3-zone:hgive zone)
	(rigidity:combine-in-series
	 (mapcar #'clim3-zone:hgive (clim3-zone:children zone))))
  (setf (clim3-zone:vgive zone)
	(rigidity:combine-in-parallel
	 (mapcar #'clim3-zone:vgive (clim3-zone:children zone)))))
  
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
  (make-instance 'hbox :children (coerce-to-list-of-zones children)))

(defun hbox* (&rest children)
  (make-instance 'hbox :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PILE.

(defclass pile (clim3-zone:compound-sequence-zone
		clim3-zone:dependent-gives-mixin
		clim3-zone:any-number-of-children-mixin)
  ())

(defmethod clim3-zone:combine-child-gives ((zone pile))
  (setf (clim3-zone:hgive zone)
	(rigidity:combine-in-parallel
	 (mapcar #'clim3-zone:hgive (clim3-zone:children zone))))
  (setf (clim3-zone:vgive zone)
	(rigidity:combine-in-parallel
	 (mapcar #'clim3-zone:vgive (clim3-zone:children zone)))))
  
(defmethod clim3-zone:impose-layout ((zone pile) hpos vpos width height)
  (declare (ignore hpos vpos))
  (let ((children (clim3-zone:children zone)))
    (loop for child in children
	  do (clim3-zone:impose-layout child 0 0 width height))))

(defun pile (children)
  (make-instance 'pile :children (coerce-to-list-of-zones children)))

(defun pile* (&rest children)
  (make-instance 'pile :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GRID.

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
;;; Class BBOARD.
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
  (make-instance 'bboard :children (coerce-to-list-of-zones children)))

(defun bboard* (&rest children)
  (make-instance 'bboard :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPONGE.
;;;
;;; A sponge is a zone that can have at most one child.  It ignores
;;; the gives of its child, and imposes its own, which makes it very
;;; elastic, both horizontally and vertically.

(defclass sponge (clim3-zone:compound-simple-zone
		  clim3-zone:independent-gives-mixin
		  clim3-zone:at-most-one-child-mixin)
  ()
  (:default-initargs :hgive (rigidity:little-rigid)
		     :vgive (rigidity:little-rigid)))

;;; No method on combine-child-gives is required, because such a
;;; method already exists for clim3-zone:independent-gives-mixin, and
;;; it does nothing. 

;;; The :before method sets the corresponding slots.  Impose the
;;; layout on the child if any.
(defmethod clim3-zone:impose-layout ((zone sponge) hpos vpos width height)
  (let ((children (clim3-zone:children zone)))
    (unless (null children)
      (clim3-zone:impose-layout (car children) hpos vpos width height))))

(defun sponge (children)
  (make-instance
   'sponge
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun sponge* (&rest children)
  (make-instance
   'sponge
   :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HSPONGE.
;;;
;;; A hsponge is a zone that can have at most one child.  It ignores
;;; the horizontal give of its child, and imposes its own, which makes
;;; it very elastic horizontally.  It copies the vertical give of its
;;; child, or if it has no child, makes it very elastic vertically.

(defclass hsponge (clim3-zone:compound-simple-zone
		   clim3-zone:vdependent-gives-mixin
		   clim3-zone:at-most-one-child-mixin)
  ())

(defmethod clim3-zone:combine-child-gives ((zone hsponge))
  (setf (clim3-zone:hgive zone)
	(rigidity:little-rigid))
  (setf (clim3-zone:vgive zone)
	(if (null (clim3-zone:children zone))
	    (rigidity:little-rigid)
	    (clim3-zone:vgive (car (clim3-zone:children zone))))))

;;; The :before method sets the corresponding slots.  Impose the
;;; layout on the child if any.
(defmethod clim3-zone:impose-layout ((zone hsponge) hpos vpos width height)
  (let ((children (clim3-zone:children zone)))
    (unless (null children)
      (clim3-zone:impose-layout (car children) hpos vpos width height))))

(defun hsponge (children)
  (make-instance
   'hsponge
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun hsponge* (&rest children)
  (make-instance
   'hsponge
   :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VSPONGE.
;;;
;;; A vsponge is a zone that can have at most one child.  It ignores
;;; the vertical give of its child, and imposes its own, which makes
;;; it very elastic vertically.  It copies the horizontal give of its
;;; child, or if it has no child, makes it very elastic horizontally.

(defclass vsponge (clim3-zone:compound-simple-zone
		   clim3-zone:hdependent-gives-mixin
		   clim3-zone:at-most-one-child-mixin)
  ())

(defmethod clim3-zone:combine-child-gives ((zone vsponge))
  (setf (clim3-zone:hgive zone)
	(if (null (clim3-zone:children zone))
	    (rigidity:little-rigid)
	    (clim3-zone:hgive (car (clim3-zone:children zone)))))
  (setf (clim3-zone:vgive zone)
	(rigidity:little-rigid)))

;;; The :before method sets the corresponding slots.  Impose the
;;; layout on the child if any.
(defmethod clim3-zone:impose-layout ((zone vsponge) hpos vpos width height)
  (let ((children (clim3-zone:children zone)))
    (unless (null children)
      (clim3-zone:impose-layout (car children) hpos vpos width height))))

(defun vsponge (children)
  (make-instance
   'vsponge
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun vsponge* (&rest children)
  (make-instance
   'vsponge
   :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BRICK.
;;;
;;; A brick is a zone that can have at most one child.  It ignores
;;; the gives of its child, and imposes its own, which makes it very
;;; rigid, both horizontally and vertically.

(defclass brick (clim3-zone:compound-simple-zone
		 clim3-zone:independent-gives-mixin
		 clim3-zone:at-most-one-child-mixin)
  ())

;;; No method on combine-child-gives is required, because such a
;;; method already exists for clim3-zone:independent-gives-mixin, and
;;; it does nothing. 

;;; The :before method sets the corresponding slots.  Impose the
;;; layout on the child if any.
(defmethod clim3-zone:impose-layout ((zone brick) hpos vpos width height)
  (let ((children (clim3-zone:children zone)))
    (unless (null children)
      (clim3-zone:impose-layout (car children) hpos vpos width height))))

(defun brick (width height children)
  (make-instance
   'brick
   :hgive (rigidity:very-rigid width)
   :vgive (rigidity:very-rigid height)
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun brick* (width height &rest children)
  (make-instance
   'brick
   :hgive (rigidity:very-rigid width)
   :vgive (rigidity:very-rigid height)
   :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HBRICK.
;;;
;;; A hbrick is a zone that can have at most one child.  It ignores
;;; the horizontal give of its child, and imposes its own, which makes
;;; it very rigid horizontally.  It copies the vertical give of its
;;; child, or if it has no child, makes it very elastic vertically.

(defclass hbrick (clim3-zone:compound-simple-zone
		  clim3-zone:vdependent-gives-mixin
		  clim3-zone:at-most-one-child-mixin)
  ())

(defmethod clim3-zone:combine-child-gives ((zone hbrick))
  (setf (clim3-zone:vgive zone)
	(if (null (clim3-zone:children zone))
	    (rigidity:little-rigid)
	    (clim3-zone:vgive (car (clim3-zone:children zone))))))

;;; The :before method sets the corresponding slots.  Impose the
;;; layout on the child if any.
(defmethod clim3-zone:impose-layout ((zone hbrick) hpos vpos width height)
  (let ((children (clim3-zone:children zone)))
    (unless (null children)
      (clim3-zone:impose-layout (car children) hpos vpos width height))))

(defun hbrick (width children)
  (make-instance
   'hbrick
   :hgive (rigidity:very-rigid width)
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun hbrick* (width &rest children)
  (make-instance
   'hbrick
   :hgive (rigidity:very-rigid width)
   :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VBRICK.
;;;
;;; A vbrick is a zone that can have at most one child.  It ignores
;;; the vertical give of its child, and imposes its own, which makes
;;; it very elastic vertically.  It copies the horizontal give of its
;;; child, or if it has no child, makes it very elastic horizontally.

(defclass vbrick (clim3-zone:compound-simple-zone
		  clim3-zone:hdependent-gives-mixin
		  clim3-zone:at-most-one-child-mixin)
  ())

(defmethod clim3-zone:combine-child-gives ((zone vbrick))
  (setf (clim3-zone:hgive zone)
	(if (null (clim3-zone:children zone))
	    (rigidity:little-rigid)
	    (clim3-zone:hgive (car (clim3-zone:children zone))))))

;;; The :before method sets the corresponding slots.  Impose the
;;; layout on the child if any.
(defmethod clim3-zone:impose-layout ((zone vbrick) hpos vpos width height)
  (let ((children (clim3-zone:children zone)))
    (unless (null children)
      (clim3-zone:impose-layout (car children) hpos vpos width height))))

(defun vbrick (height children)
  (make-instance
   'vbrick
   :vgive (rigidity:very-rigid height)
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun vbrick* (height &rest children)
  (make-instance
   'vbrick
   :vgive (rigidity:very-rigid height)
   :children (coerce-to-list-of-at-most-one-zone children)))
