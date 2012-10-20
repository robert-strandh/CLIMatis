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
		clim3-zone:dependent-sprawls-mixin
		clim3-zone:any-number-of-children-mixin)
  ())

(defmethod clim3-zone:combine-child-sprawls ((zone vbox))
  (clim3-zone:set-hsprawl
   (clim3-sprawl:combine-in-parallel
    (mapcar #'clim3-zone:hsprawl (clim3-zone:children zone)))
   zone)
  (clim3-zone:set-vsprawl
   (clim3-sprawl:combine-in-series
    (mapcar #'clim3-zone:vsprawl (clim3-zone:children zone)))
   zone))
  
;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone vbox) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone vbox))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone))
	 (vertical-sprawls (mapcar #'clim3-zone:vsprawl children))
	 (heights (clim3-sprawl:sizes-in-series vertical-sprawls height)))
    (loop for vpos = 0 then (+ vpos height)
	  for height in heights
	  for child in children
	  do (clim3-zone:set-hpos 0 child)
	     (clim3-zone:set-vpos vpos child)
	     (clim3-zone:impose-size child width height))))
  
(defun vbox (children)
  (make-instance 'vbox :children (coerce-to-list-of-zones children)))

(defun vbox* (&rest children)
  (make-instance 'vbox :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HBOX.

(defclass hbox (clim3-zone:compound-sequence-zone
		clim3-zone:dependent-sprawls-mixin
		clim3-zone:any-number-of-children-mixin)
  ())

(defmethod clim3-zone:combine-child-sprawls ((zone hbox))
  (clim3-zone:set-hsprawl
   (clim3-sprawl:combine-in-series
    (mapcar #'clim3-zone:hsprawl (clim3-zone:children zone)))
   zone)
  (clim3-zone:set-vsprawl
   (clim3-sprawl:combine-in-parallel
    (mapcar #'clim3-zone:vsprawl (clim3-zone:children zone)))
   zone))
  
;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone hbox) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone hbox))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone))
	 (horizontal-sprawls (mapcar #'clim3-zone:hsprawl children))
	 (widths (clim3-sprawl:sizes-in-series horizontal-sprawls width)))
    (loop for hpos = 0 then (+ hpos width)
	  for width in widths
	  for child in children
	  do (clim3-zone:set-hpos hpos child)
	     (clim3-zone:set-vpos 0 child)
	     (clim3-zone:impose-size child width height))))

(defun hbox (children)
  (make-instance 'hbox :children (coerce-to-list-of-zones children)))

(defun hbox* (&rest children)
  (make-instance 'hbox :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PILE.

(defclass pile (clim3-zone:compound-sequence-zone
		clim3-zone:dependent-sprawls-mixin
		clim3-zone:any-number-of-children-mixin)
  ())

(defmethod clim3-zone:combine-child-sprawls ((zone pile))
  (clim3-zone:set-hsprawl
   (clim3-sprawl:combine-in-parallel
    (mapcar #'clim3-zone:hsprawl (clim3-zone:children zone)))
   zone)
  (clim3-zone:set-vsprawl
   (clim3-sprawl:combine-in-parallel
    (mapcar #'clim3-zone:vsprawl (clim3-zone:children zone)))
   zone))
  
;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone pile) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone pile))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone)))
    (loop for child in children
	  do (clim3-zone:set-hpos 0 child)
	     (clim3-zone:set-vpos 0 child)
	     (clim3-zone:impose-size child width height))))

(defun pile (children)
  (make-instance 'pile :children (coerce-to-list-of-zones children)))

(defun pile* (&rest children)
  (make-instance 'pile :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GRID.

(defclass grid (clim3-zone:compound-zone
		clim3-zone:dependent-sprawls-mixin
		clim3-zone:any-number-of-children-mixin)
  ((%combined-rows :initform nil :accessor combined-rows)
   (%combined-cols :initform nil :accessor combined-cols)))

(defmethod clim3-zone:combine-child-sprawls ((zone grid))
  (let* ((children (clim3-zone:children zone))
	 (rows (array-dimension children 0))
	 (cols (array-dimension children 1)))
    (cond ((= rows 1)
	   (setf (combined-cols zone)
		 (loop for col from 0 below cols
		       collect (clim3-zone:hsprawl (aref children 0 col))))
	   (clim3-zone:set-hsprawl
	    (clim3-sprawl:combine-in-series (combined-cols zone))
	    zone)
	   (clim3-zone:set-vsprawl
	    (clim3-sprawl:combine-in-parallel
	     (loop for col from 0 below cols
		   collect (clim3-zone:vsprawl (aref children 0 col))))
	    zone))
	  ((= cols 1)
	   (setf (combined-rows zone)
		 (loop for row from 0 below rows
		       collect (clim3-zone:vsprawl (aref children 0 row))))
	   (clim3-zone:set-hsprawl
	    (clim3-sprawl:combine-in-parallel
	     (loop for row from 0 below rows
		   collect (clim3-zone:hsprawl (aref children 0 row))))
	    zone)
	   (clim3-zone:set-vsprawl
	    (clim3-sprawl:combine-in-series (combined-rows zone))
	    zone))
	  (t
	   (setf (combined-rows zone)
		 (loop for row from 0 below rows
		       collect (clim3-sprawl:combine-in-parallel
				(loop for col from 0 below cols
				      collect (clim3-zone:vsprawl (aref children row col))))))
	   (setf (combined-cols zone)
		 (loop for col from 0 below cols
		       collect (clim3-sprawl:combine-in-parallel
				(loop for row from 0 below rows
				      collect (clim3-zone:hsprawl (aref children row col))))))
	   (clim3-zone:set-hsprawl
	    (clim3-sprawl:combine-in-series (combined-cols children))
	    zone)
	   (clim3-zone:set-vsprawl
	    (clim3-sprawl:combine-in-series (combined-rows children))
	    zone)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BBOARD.
;;;
;;; This a zone that lets its children be positioned wherever they want.

(defclass bboard (clim3-zone:compound-simple-zone
		  clim3-zone:independent-sprawls-mixin
		  clim3-zone:any-number-of-children-mixin)
  ()
  (:default-initargs :vsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :hsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod clim3-zone:impose-size ((zone bboard) width height)
  nil)

(defmethod clim3-zone:combine-child-sprawls ((zone bboard))
  nil)

(defmethod clim3-zone:impose-child-layouts ((zone bboard))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (loop for child in (clim3-zone:children zone)
	do (clim3-zone:ensure-sprawls-valid child)
	   (multiple-value-bind (width height)
	       (clim3-zone:natural-size child)
	     (clim3-zone:impose-size child width height))))

(defun bboard (children)
  (make-instance 'bboard :children (coerce-to-list-of-zones children)))

(defun bboard* (&rest children)
  (make-instance 'bboard :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPONGE.
;;;
;;; A sponge is a zone that can have at most one child.  It ignores
;;; the sprawls of its child, and imposes its own, which makes it very
;;; elastic, both horizontally and vertically.

(defclass sponge (clim3-zone:compound-simple-zone
		  clim3-zone:independent-sprawls-mixin
		  clim3-zone:at-most-one-child-mixin)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

;;; No method on combine-child-sprawls is required, because such a
;;; method already exists for clim3-zone:independent-sprawls-mixin, and
;;; it does nothing. 

;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone sponge) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone sponge))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone)))
    (unless (null children)
      (let ((child (car children)))
	(clim3-zone:set-hpos 0 child)
	(clim3-zone:set-vpos 0 child)
	(clim3-zone:impose-size child width height)))))

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
;;; the horizontal sprawl of its child, and imposes its own, which makes
;;; it very elastic horizontally.  It copies the vertical sprawl of its
;;; child, or if it has no child, makes it very elastic vertically.

(defclass hsponge (clim3-zone:compound-simple-zone
		   clim3-zone:vdependent-sprawls-mixin
		   clim3-zone:at-most-one-child-mixin)
  ())

(defmethod clim3-zone:combine-child-sprawls ((zone hsponge))
  (clim3-zone:set-hsprawl
   (clim3-sprawl:sprawl 0 0 nil)
   zone)
  (clim3-zone:set-vsprawl
   (if (null (clim3-zone:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-zone:vsprawl (car (clim3-zone:children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone hsponge) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone hsponge))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone)))
    (unless (null children)
      (let ((child (car children)))
	(clim3-zone:set-hpos 0 child)
	(clim3-zone:set-vpos 0 child)
	(clim3-zone:impose-size child width height)))))

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
;;; the vertical sprawl of its child, and imposes its own, which makes
;;; it very elastic vertically.  It copies the horizontal sprawl of its
;;; child, or if it has no child, makes it very elastic horizontally.

(defclass vsponge (clim3-zone:compound-simple-zone
		   clim3-zone:hdependent-sprawls-mixin
		   clim3-zone:at-most-one-child-mixin)
  ())

(defmethod clim3-zone:combine-child-sprawls ((zone vsponge))
  (clim3-zone:set-hsprawl
   (if (null (clim3-zone:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-zone:hsprawl (car (clim3-zone:children zone))))
   zone)
  (clim3-zone:set-vsprawl
   (clim3-sprawl:sprawl 0 0 nil)
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone vsponge) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone vsponge))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone)))
    (unless (null children)
      (let ((child (car children)))
	(clim3-zone:set-hpos 0 child)
	(clim3-zone:set-vpos 0 child)
	(clim3-zone:impose-size child width height)))))

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
;;; the sprawls of its child, and imposes its own, which makes it very
;;; rigid, both horizontally and vertically.

(defclass brick (clim3-zone:compound-simple-zone
		 clim3-zone:independent-sprawls-mixin
		 clim3-zone:at-most-one-child-mixin)
  ())

;;; No method on combine-child-sprawls is required, because such a
;;; method already exists for clim3-zone:independent-sprawls-mixin, and
;;; it does nothing. 

;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone brick) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone brick))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone)))
    (unless (null children)
      (let ((child (car children)))
	(clim3-zone:set-hpos 0 child)
	(clim3-zone:set-vpos 0 child)
	(clim3-zone:impose-size child width height)))))

(defun brick (width height children)
  (make-instance
   'brick
   :hsprawl (clim3-sprawl:sprawl width width width)
   :vsprawl (clim3-sprawl:sprawl height height height)
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun brick* (width height &rest children)
  (make-instance
   'brick
   :hsprawl (clim3-sprawl:sprawl width width width)
   :vsprawl (clim3-sprawl:sprawl height height height)
   :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HBRICK.
;;;
;;; A hbrick is a zone that can have at most one child.  It ignores
;;; the horizontal sprawl of its child, and imposes its own, which makes
;;; it very rigid horizontally.  It copies the vertical sprawl of its
;;; child, or if it has no child, makes it very elastic vertically.

(defclass hbrick (clim3-zone:compound-simple-zone
		  clim3-zone:vdependent-sprawls-mixin
		  clim3-zone:at-most-one-child-mixin)
  ())

(defmethod clim3-zone:combine-child-sprawls ((zone hbrick))
  (clim3-zone:set-vsprawl
   (if (null (clim3-zone:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-zone:vsprawl (car (clim3-zone:children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone hbrick) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone hbrick))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone)))
    (unless (null children)
      (let ((child (car children)))
	(clim3-zone:set-hpos 0 child)
	(clim3-zone:set-vpos 0 child)
	(clim3-zone:impose-size child width height)))))

(defun hbrick (width children)
  (make-instance
   'hbrick
   :hsprawl (clim3-sprawl:sprawl width width width)
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun hbrick* (width &rest children)
  (make-instance
   'hbrick
   :hsprawl (clim3-sprawl:sprawl width width width)
   :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VBRICK.
;;;
;;; A vbrick is a zone that can have at most one child.  It ignores
;;; the vertical sprawl of its child, and imposes its own, which makes
;;; it very elastic vertically.  It copies the horizontal sprawl of its
;;; child, or if it has no child, makes it very elastic horizontally.

(defclass vbrick (clim3-zone:compound-simple-zone
		  clim3-zone:hdependent-sprawls-mixin
		  clim3-zone:at-most-one-child-mixin)
  ())

(defmethod clim3-zone:combine-child-sprawls ((zone vbrick))
  (clim3-zone:set-hsprawl
   (if (null (clim3-zone:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-zone:hsprawl (car (clim3-zone:children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone vbrick) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone vbrick))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone)))
    (unless (null children)
      (let ((child (car children)))
	(clim3-zone:set-hpos 0 child)
	(clim3-zone:set-vpos 0 child)
	(clim3-zone:impose-size child width height)))))

(defun vbrick (height children)
  (make-instance
   'vbrick
   :vsprawl (clim3-sprawl:sprawl height height height)
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun vbrick* (height &rest children)
  (make-instance
   'vbrick
   :vsprawl (clim3-sprawl:sprawl height height height)
   :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HFRAME.
;;;
;;; A hframe is a zone that can have at most one child.  It ignores
;;; the horizontal sprawl of its child, and imposes its own.  It
;;; copies the vertical sprawl of its child, or if it has no child,
;;; makes it very elastic vertically.

(defclass hframe (clim3-zone:compound-simple-zone
		  clim3-zone:vdependent-sprawls-mixin
		  clim3-zone:at-most-one-child-mixin)
  ())

(defmethod clim3-zone:combine-child-sprawls ((zone hframe))
  (clim3-zone:set-vsprawl
   (if (null (clim3-zone:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-zone:vsprawl (car (clim3-zone:children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone hframe) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone hframe))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone)))
    (unless (null children)
      (let ((child (car children)))
	(clim3-zone:set-hpos 0 child)
	(clim3-zone:set-vpos 0 child)
	(clim3-zone:impose-size child width height)))))

(defun hframe (min-width width max-width children)
  (make-instance
   'hframe
   :hsprawl (clim3-sprawl:sprawl min-width width max-width)
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun hframe* (min-width width max-width &rest children)
  (make-instance
   'hframe
   :hsprawl (clim3-sprawl:sprawl min-width width max-width)
   :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VFRAME.
;;;
;;; A vframe is a zone that can have at most one child.  It ignores
;;; the vertical sprawl of its child, and imposes its own.  It copies
;;; the horizontal sprawl of its child, or if it has no child, makes
;;; it very elastic horizontally.

(defclass vframe (clim3-zone:compound-simple-zone
		  clim3-zone:hdependent-sprawls-mixin
		  clim3-zone:at-most-one-child-mixin)
  ())

(defmethod clim3-zone:combine-child-sprawls ((zone vframe))
  (clim3-zone:set-hsprawl
   (if (null (clim3-zone:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-zone:hsprawl (car (clim3-zone:children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod clim3-zone:impose-size ((zone vframe) width height)
  (unless (and (= width (clim3-zone:width zone))
	       (= height (clim3-zone:height zone)))
    (setf (clim3-zone:child-layouts-valid-p zone) nil)))

(defmethod clim3-zone:impose-child-layouts ((zone vframe))
  (clim3-zone:map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (clim3-zone:width zone))
	 (height (clim3-zone:height zone))
	 (children (clim3-zone:children zone)))
    (unless (null children)
      (let ((child (car children)))
	(clim3-zone:set-hpos 0 child)
	(clim3-zone:set-vpos 0 child)
	(clim3-zone:impose-size child width height)))))

(defun vframe (min-height height max-height children)
  (make-instance
   'vframe
   :vsprawl (clim3-sprawl:sprawl min-height height max-height)
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun vframe* (min-height height max-height &rest children)
  (make-instance
   'vframe
   :vsprawl (clim3-sprawl:sprawl min-height height max-height)
   :children (coerce-to-list-of-at-most-one-zone children)))
