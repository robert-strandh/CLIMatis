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
      (unless (zone-p (car result))
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
	  do (unless (zone-p element)
	       (error "a zone was expected ~s" element)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VBOX. 

(defclass vbox (compound-sequence-zone
		dependent-sprawls-mixin
		any-number-of-children-mixin)
  ())

(defmethod combine-child-sprawls ((zone vbox))
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'hsprawl (children zone))))
   zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-series
	(mapcar #'vsprawl (children zone))))
   zone))
  
;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone vbox) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone vbox))
  (map-over-children #'clim3-zone:ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone))
	 (vertical-sprawls (mapcar #'vsprawl children))
	 (heights (clim3-sprawl:sizes-in-series vertical-sprawls height)))
    (loop for vpos = 0 then (+ vpos height)
	  for height in heights
	  for child in children
	  do (set-hpos 0 child)
	     (set-vpos vpos child)
	     (impose-size child width height))))
  
(defun vbox (children)
  (make-instance 'vbox :children (coerce-to-list-of-zones children)))

(defun vbox* (&rest children)
  (make-instance 'vbox :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HBOX.

(defclass hbox (compound-sequence-zone
		dependent-sprawls-mixin
		any-number-of-children-mixin)
  ())

(defmethod combine-child-sprawls ((zone hbox))
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-series
	(mapcar #'hsprawl (children zone))))
   zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'vsprawl (children zone))))
   zone))
  
;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone hbox) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone hbox))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone))
	 (horizontal-sprawls (mapcar #'hsprawl children))
	 (widths (clim3-sprawl:sizes-in-series horizontal-sprawls width)))
    (loop for hpos = 0 then (+ hpos width)
	  for width in widths
	  for child in children
	  do (set-hpos hpos child)
	     (set-vpos 0 child)
	     (impose-size child width height))))

(defun hbox (children)
  (make-instance 'hbox :children (coerce-to-list-of-zones children)))

(defun hbox* (&rest children)
  (make-instance 'hbox :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PILE.

(defclass pile (compound-sequence-zone
		dependent-sprawls-mixin
		any-number-of-children-mixin)
  ())

(defmethod combine-child-sprawls ((zone pile))
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'hsprawl (children zone))))
   zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'vsprawl (children zone))))
   zone))
  
;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone pile) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone pile))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone)))
    (loop for child in children
	  do (set-hpos 0 child)
	     (set-vpos 0 child)
	     (impose-size child width height))))

(defun pile (children)
  (make-instance 'pile :children (coerce-to-list-of-zones children)))

(defun pile* (&rest children)
  (make-instance 'pile :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GRID.

(defclass grid (compound-zone
		dependent-sprawls-mixin
		any-number-of-children-mixin)
  ((%combined-rows :initform nil :accessor combined-rows)
   (%combined-cols :initform nil :accessor combined-cols)))

(defmethod combine-child-sprawls ((zone grid))
  (let* ((children (children zone))
	 (rows (array-dimension children 0))
	 (cols (array-dimension children 1)))
    (cond ((= rows 1)
	   (setf (combined-cols zone)
		 (loop for col from 0 below cols
		       collect (hsprawl (aref children 0 col))))
	   (set-hsprawl
	    (clim3-sprawl:combine-in-series (combined-cols zone))
	    zone)
	   (set-vsprawl
	    (clim3-sprawl:combine-in-parallel
	     (loop for col from 0 below cols
		   collect (vsprawl (aref children 0 col))))
	    zone))
	  ((= cols 1)
	   (setf (combined-rows zone)
		 (loop for row from 0 below rows
		       collect (vsprawl (aref children 0 row))))
	   (set-hsprawl
	    (clim3-sprawl:combine-in-parallel
	     (loop for row from 0 below rows
		   collect (hsprawl (aref children 0 row))))
	    zone)
	   (set-vsprawl
	    (clim3-sprawl:combine-in-series (combined-rows zone))
	    zone))
	  (t
	   (setf (combined-rows zone)
		 (loop for row from 0 below rows
		       collect (clim3-sprawl:combine-in-parallel
				(loop for col from 0 below cols
				      collect (vsprawl (aref children row col))))))
	   (setf (combined-cols zone)
		 (loop for col from 0 below cols
		       collect (clim3-sprawl:combine-in-parallel
				(loop for row from 0 below rows
				      collect (hsprawl (aref children row col))))))
	   (set-hsprawl
	    (clim3-sprawl:combine-in-series (combined-cols children))
	    zone)
	   (set-vsprawl
	    (clim3-sprawl:combine-in-series (combined-rows children))
	    zone)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BBOARD.
;;;
;;; This a zone that lets its children be positioned wherever they want.

(defclass bboard (compound-simple-zone
		  independent-sprawls-mixin
		  any-number-of-children-mixin)
  ()
  (:default-initargs :vsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :hsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod impose-size ((zone bboard) width height)
  nil)

(defmethod combine-child-sprawls ((zone bboard))
  nil)

(defmethod impose-child-layouts ((zone bboard))
  (map-over-children #'ensure-sprawls-valid zone)
  (loop for child in (children zone)
	do (ensure-sprawls-valid child)
	   (multiple-value-bind (width height)
	       (natural-size child)
	     (impose-size child width height))))

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

(defclass sponge (compound-simple-zone
		  independent-sprawls-mixin
		  at-most-one-child-mixin)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

;;; No method on combine-child-sprawls is required, because such a
;;; method already exists for independent-sprawls-mixin, and
;;; it does nothing. 

;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone sponge) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone sponge))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone)))
    (unless (null children)
      (let ((child (car children)))
	(set-hpos 0 child)
	(set-vpos 0 child)
	(impose-size child width height)))))

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

(defclass hsponge (compound-simple-zone
		   vdependent-sprawls-mixin
		   at-most-one-child-mixin)
  ())

(defmethod combine-child-sprawls ((zone hsponge))
  (set-hsprawl
   (clim3-sprawl:sprawl 0 0 nil)
   zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (vsprawl (car (children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone hsponge) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone hsponge))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone)))
    (unless (null children)
      (let ((child (car children)))
	(set-hpos 0 child)
	(set-vpos 0 child)
	(impose-size child width height)))))

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

(defclass vsponge (compound-simple-zone
		   hdependent-sprawls-mixin
		   at-most-one-child-mixin)
  ())

(defmethod combine-child-sprawls ((zone vsponge))
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (hsprawl (car (children zone))))
   zone)
  (set-vsprawl
   (clim3-sprawl:sprawl 0 0 nil)
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone vsponge) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone vsponge))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone)))
    (unless (null children)
      (let ((child (car children)))
	(set-hpos 0 child)
	(set-vpos 0 child)
	(impose-size child width height)))))

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

(defclass brick (compound-simple-zone
		 independent-sprawls-mixin
		 at-most-one-child-mixin)
  ())

;;; No method on combine-child-sprawls is required, because such a
;;; method already exists for independent-sprawls-mixin, and
;;; it does nothing. 

;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone brick) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone brick))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone)))
    (unless (null children)
      (let ((child (car children)))
	(set-hpos 0 child)
	(set-vpos 0 child)
	(impose-size child width height)))))

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

(defclass hbrick (compound-simple-zone
		  vdependent-sprawls-mixin
		  at-most-one-child-mixin)
  ())

(defmethod combine-child-sprawls ((zone hbrick))
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (vsprawl (car (children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone hbrick) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone hbrick))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone)))
    (unless (null children)
      (let ((child (car children)))
	(set-hpos 0 child)
	(set-vpos 0 child)
	(impose-size child width height)))))

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

(defclass vbrick (compound-simple-zone
		  hdependent-sprawls-mixin
		  at-most-one-child-mixin)
  ())

(defmethod combine-child-sprawls ((zone vbrick))
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (hsprawl (car (children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone vbrick) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone vbrick))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone)))
    (unless (null children)
      (let ((child (car children)))
	(set-hpos 0 child)
	(set-vpos 0 child)
	(impose-size child width height)))))

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

(defclass hframe (compound-simple-zone
		  vdependent-sprawls-mixin
		  at-most-one-child-mixin)
  ())

(defmethod combine-child-sprawls ((zone hframe))
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (vsprawl (car (children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone hframe) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone hframe))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone)))
    (unless (null children)
      (let ((child (car children)))
	(set-hpos 0 child)
	(set-vpos 0 child)
	(impose-size child width height)))))

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

(defclass vframe (compound-simple-zone
		  hdependent-sprawls-mixin
		  at-most-one-child-mixin)
  ())

(defmethod combine-child-sprawls ((zone vframe))
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (hsprawl (car (children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone vframe) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone vframe))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone)))
    (unless (null children)
      (let ((child (car children)))
	(set-hpos 0 child)
	(set-vpos 0 child)
	(impose-size child width height)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WRAP
;;; 
;;; A wrap zone is a zone that has at most one child.  If it has a
;;; child, then it acquires the same sprawls as the child.  If not, it
;;; becomes very elastic.
;;;
;;; This class can be subclassed for convenience

(defclass wrap (compound-simple-zone
		hdependent-sprawls-mixin
		at-most-one-child-mixin)
  ()
  (:default-initargs :children '()))

(defmethod combine-child-sprawls ((zone wrap))
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (hsprawl (car (children zone))))
   zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (vsprawl (car (children zone))))
   zone))

;;; We should probably factor this one out to a mixin class
(defmethod impose-size ((zone wrap) width height)
  (unless (and (= width (width zone))
	       (= height (height zone)))
    (setf (child-layouts-valid-p zone) nil)))

(defmethod impose-child-layouts ((zone wrap))
  (map-over-children #'ensure-sprawls-valid zone)
  (let* ((width (width zone))
	 (height (height zone))
	 (children (children zone)))
    (unless (null children)
      (let ((child (car children)))
	(set-hpos 0 child)
	(set-vpos 0 child)
	(impose-size child width height)))))

;;; These constructors would typically not be used.  Instead, client
;;; code would use MAKE-INSTANCE on the subclass of the wrap.

(defun wrap (children)
  (make-instance
   'wrap
   :children (coerce-to-list-of-at-most-one-zone children)))

(defun wrap* (&rest children)
  (make-instance
   'wrap
   :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VBRICK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HBRICK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BRICK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VSPONGE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HSPONGE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPONGE

