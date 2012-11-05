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
		any-number-of-children-mixin
		changing-child-hsprawl-changes-hsprawl-mixin
		changing-child-vsprawl-changes-vsprawl-mixin
		changing-children-changes-both-sprawls-mixin
		changing-child-position-not-allowed-mixin
		child-depth-insignificant-mixin)
  ())

(defmethod compute-hsprawl ((zone vbox))
  (map-over-children #'clim3-zone:ensure-hsprawl-valid zone)
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'hsprawl (children zone))))
   zone))
  
(defmethod compute-vsprawl ((zone vbox))
  (map-over-children #'clim3-zone:ensure-vsprawl-valid zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-series
	(mapcar #'vsprawl (children zone))))
   zone))
  
(defmethod impose-child-layouts ((zone vbox))
  (map-over-children #'clim3-zone:ensure-hsprawl-valid zone)
  (map-over-children #'clim3-zone:ensure-vsprawl-valid zone)
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
		any-number-of-children-mixin
		changing-child-hsprawl-changes-hsprawl-mixin
		changing-child-vsprawl-changes-vsprawl-mixin
		changing-children-changes-both-sprawls-mixin
		changing-child-position-not-allowed-mixin
		child-depth-insignificant-mixin)
  ())

(defmethod compute-hsprawl ((zone hbox))
  (map-over-children #'clim3-zone:ensure-hsprawl-valid zone)
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-series
	(mapcar #'hsprawl (children zone))))
   zone))
  
(defmethod compute-vsprawl ((zone hbox))
  (map-over-children #'clim3-zone:ensure-vsprawl-valid zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'vsprawl (children zone))))
   zone))
  
(defmethod impose-child-layouts ((zone hbox))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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
		any-number-of-children-mixin
		changing-child-hsprawl-changes-hsprawl-mixin
		changing-child-vsprawl-changes-vsprawl-mixin
		changing-children-changes-both-sprawls-mixin
		changing-child-position-not-allowed-mixin
		child-depth-significant-mixin)
  ())

(defmethod compute-hsprawl ((zone pile))
  (map-over-children #'clim3-zone:ensure-hsprawl-valid zone)
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'hsprawl (children zone))))
   zone))
  
(defmethod compute-vsprawl ((zone pile))
  (map-over-children #'clim3-zone:ensure-vsprawl-valid zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'vsprawl (children zone))))
   zone))
  
(defmethod impose-child-layouts ((zone pile))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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
		any-number-of-children-mixin
		changing-child-hsprawl-changes-hsprawl-mixin
		changing-child-vsprawl-changes-vsprawl-mixin
		changing-children-changes-both-sprawls-mixin
		changing-child-position-not-allowed-mixin
		child-depth-insignificant-mixin)
  ((%combined-rows :initform nil :accessor combined-rows)
   (%combined-cols :initform nil :accessor combined-cols)))

(defmethod compute-hsprawl ((zone grid))
  (map-over-children #'clim3-zone:ensure-hsprawl-valid zone)
  (let* ((children (children zone))
	 (rows (array-dimension children 0))
	 (cols (array-dimension children 1)))
    (cond ((= rows 1)
	   (setf (combined-cols zone)
		 (loop for col from 0 below cols
		       collect (hsprawl (aref children 0 col))))
	   (set-hsprawl
	    (clim3-sprawl:combine-in-series (combined-cols zone))
	    zone))
	  ((= cols 1)
	   (set-hsprawl
	    (clim3-sprawl:combine-in-parallel
	     (loop for row from 0 below rows
		   collect (hsprawl (aref children 0 row))))
	    zone))
	  (t
	   (setf (combined-cols zone)
		 (loop for col from 0 below cols
		       collect (clim3-sprawl:combine-in-parallel
				(loop for row from 0 below rows
				      collect (hsprawl (aref children row col))))))
	   (set-hsprawl
	    (clim3-sprawl:combine-in-series (combined-cols children))
	    zone)))))

(defmethod compute-vsprawl ((zone grid))
  (map-over-children #'clim3-zone:ensure-vsprawl-valid zone)
  (let* ((children (children zone))
	 (rows (array-dimension children 0))
	 (cols (array-dimension children 1)))
    (cond ((= rows 1)
	   (set-vsprawl
	    (clim3-sprawl:combine-in-parallel
	     (loop for col from 0 below cols
		   collect (vsprawl (aref children 0 col))))
	    zone))
	  ((= cols 1)
	   (setf (combined-rows zone)
		 (loop for row from 0 below rows
		       collect (vsprawl (aref children 0 row))))
	   (set-vsprawl
	    (clim3-sprawl:combine-in-series (combined-rows zone))
	    zone))
	  (t
	   (setf (combined-rows zone)
		 (loop for row from 0 below rows
		       collect (clim3-sprawl:combine-in-parallel
				(loop for col from 0 below cols
				      collect (vsprawl (aref children row col))))))
	   (set-vsprawl
	    (clim3-sprawl:combine-in-series (combined-rows children))
	    zone)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SCROLLER.

(defclass scroller (compound-simple-zone
		    at-most-one-child-mixin
		    changing-child-hsprawl-changes-child-layouts-mixin
		    changing-child-vsprawl-changes-child-layouts-mixin
		    changing-children-changes-child-layouts-mixin
		    changing-child-position-not-allowed-mixin
		    child-depth-insignificant-mixin)
  ())

(defmethod compute-hsprawl ((zone scroller))
  (error "attempt to combine hsprawls of children of a scroller zone"))

(defmethod compute-vsprawl ((zone scroller))
  (error "attempt to combine vsprawls of children of a scroller zone"))

(defmethod impose-child-layouts ((zone scroller))
  ;; There is either no child or one child, but this is convenient.
  (map-over-children
   (lambda (child)
     (ensure-hsprawl-valid child)
     (ensure-vsprawl-valid child)
     (multiple-value-bind (width height)
	 (natural-size child)
       (impose-size child width height)))
   zone))

(defun scroller (children)
  (make-instance 'scroller
		 :children (coerce-to-list-of-at-most-one-zone children)))

(defun scroller* (&rest children)
  (make-instance 'scroller
		 :children (coerce-to-list-of-at-most-one-zone children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BBOARD.

(defclass bboard (compound-simple-zone
		  any-number-of-children-mixin
		  changing-child-hsprawl-changes-hsprawl-mixin
		  changing-child-vsprawl-changes-vsprawl-mixin
		  changing-children-changes-both-sprawls-mixin
		  changing-child-position-changes-both-sprawls-mixin
		  child-depth-significant-mixin)
  ()
  (:default-initargs :vsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :hsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod impose-size ((zone bboard) width height)
  nil)

(defmethod compute-hsprawl ((zone bboard))
  (map-over-children #'clim3-zone:ensure-hsprawl-valid zone)
  (impose-child-layouts zone)
  (let ((max-hpos 0))
    (map-over-children
     (lambda (child)
       (setf max-hpos (max max-hpos (+ (hpos child) (width child)))))
     zone)
    (set-hsprawl (clim3-sprawl:sprawl max-hpos max-hpos nil) zone)))

(defmethod compute-vsprawl ((zone bboard))
  (map-over-children #'clim3-zone:ensure-vsprawl-valid zone)
  (impose-child-layouts zone)
  (let ((max-vpos 0))
    (map-over-children
     (lambda (child)
       (setf max-vpos (max max-vpos (+ (vpos child) (height child)))))
     zone)
    (set-vsprawl (clim3-sprawl:sprawl max-vpos max-vpos nil) zone)))

(defmethod impose-child-layouts ((zone bboard))
  (loop for child in (children zone)
	do (ensure-hsprawl-valid child)
	   (ensure-vsprawl-valid child)
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
		  at-most-one-child-mixin
		  changing-child-hsprawl-changes-nothing-mixin
		  changing-child-vsprawl-changes-nothing-mixin
		  changing-children-changes-child-layouts-mixin
		  changing-child-position-not-allowed-mixin
		  child-depth-insignificant-mixin)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod impose-child-layouts ((zone sponge))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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
		   at-most-one-child-mixin
		   changing-child-hsprawl-changes-nothing-mixin
		   changing-child-vsprawl-changes-vsprawl-mixin
		   changing-children-changes-vsprawl-mixin
		   changing-child-position-not-allowed-mixin
		   child-depth-insignificant-mixin)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod compute-vsprawl ((zone hsponge))
  (map-over-children #'clim3-zone:ensure-vsprawl-valid zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (vsprawl (car (children zone))))
   zone))

(defmethod impose-child-layouts ((zone hsponge))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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
		   at-most-one-child-mixin
		   changing-child-hsprawl-changes-hsprawl-mixin
		   changing-child-vsprawl-changes-nothing-mixin
		   changing-children-changes-hsprawl-mixin
		   changing-child-position-not-allowed-mixin
		   child-depth-insignificant-mixin)
  ()
  (:default-initargs :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod compute-hsprawl ((zone vsponge))
  (map-over-children #'clim3-zone:ensure-hsprawl-valid zone)
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (hsprawl (car (children zone))))
   zone))

(defmethod impose-child-layouts ((zone vsponge))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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
		 at-most-one-child-mixin
		 changing-child-hsprawl-changes-nothing-mixin
		 changing-child-vsprawl-changes-nothing-mixin
		 changing-children-changes-nothing-mixin
		 changing-child-position-not-allowed-mixin
		 child-depth-insignificant-mixin)
  ())

(defmethod impose-child-layouts ((zone brick))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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
		  at-most-one-child-mixin
		  changing-child-hsprawl-changes-nothing-mixin
		  changing-child-vsprawl-changes-vsprawl-mixin
		  changing-children-changes-vsprawl-mixin
		  changing-child-position-not-allowed-mixin
		  child-depth-insignificant-mixin)
  ())

(defmethod compute-vsprawl ((zone hbrick))
  (map-over-children #'clim3-zone:ensure-vsprawl-valid zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (vsprawl (car (children zone))))
   zone))

(defmethod impose-child-layouts ((zone hbrick))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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
		  at-most-one-child-mixin
		  changing-child-hsprawl-changes-hsprawl-mixin
		  changing-child-vsprawl-changes-nothing-mixin
		  changing-children-changes-hsprawl-mixin
		  changing-child-position-not-allowed-mixin
		  child-depth-insignificant-mixin)
  ())

(defmethod compute-hsprawl ((zone vbrick))
  (map-over-children #'clim3-zone:ensure-hsprawl-valid zone)
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (hsprawl (car (children zone))))
   zone))

(defmethod impose-child-layouts ((zone vbrick))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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
		  at-most-one-child-mixin
		  changing-child-hsprawl-changes-nothing-mixin
		  changing-child-vsprawl-changes-vsprawl-mixin
		  changing-children-changes-vsprawl-mixin
		  changing-child-position-not-allowed-mixin
		  child-depth-insignificant-mixin)
  ())

(defmethod compute-vsprawl ((zone hframe))
  (map-over-children #'clim3-zone:ensure-vsprawl-valid zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (vsprawl (car (children zone))))
   zone))

(defmethod impose-child-layouts ((zone hframe))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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
		  at-most-one-child-mixin
		  changing-child-hsprawl-changes-hsprawl-mixin
		  changing-child-vsprawl-changes-nothing-mixin
		  changing-children-changes-hsprawl-mixin
		  changing-child-position-not-allowed-mixin
		  child-depth-insignificant-mixin)
  ())

(defmethod compute-hsprawl ((zone vframe))
  (map-over-children #'clim3-zone:ensure-hsprawl-valid zone)
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (hsprawl (car (children zone))))
   zone))

(defmethod impose-child-layouts ((zone vframe))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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
		at-most-one-child-mixin
		changing-child-hsprawl-changes-hsprawl-mixin
		changing-child-vsprawl-changes-vsprawl-mixin
		changing-children-changes-both-sprawls-mixin
		changing-child-position-not-allowed-mixin
		child-depth-insignificant-mixin)
  ()
  (:default-initargs :children '()))

(defmethod compute-hsprawl ((zone wrap))
  (map-over-children #'clim3-zone:ensure-hsprawl-valid zone)
  (set-hsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (hsprawl (car (children zone))))
   zone))

(defmethod compute-vsprawl ((zone wrap))
  (map-over-children #'clim3-zone:ensure-vsprawl-valid zone)
  (set-vsprawl
   (if (null (children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (vsprawl (car (children zone))))
   zone))

(defmethod impose-child-layouts ((zone wrap))
  (map-over-children #'ensure-hsprawl-valid zone)
  (map-over-children #'ensure-vsprawl-valid zone)
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

