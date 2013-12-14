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
      (unless (clim3:zone-p (car result))
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
	  do (unless (clim3:zone-p element)
	       (error "a zone was expected ~s" element)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VBOX. 

(defclass clim3:vbox
    (clim3:standard-zone
     clim3-ext:list-children-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-both-sprawls-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ())

(defmethod clim3-ext:compute-hsprawl ((zone clim3:vbox))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:set-hsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'clim3:hsprawl (clim3:children zone))))
   zone))
  
(defmethod clim3-ext:compute-vsprawl ((zone clim3:vbox))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (clim3-ext:set-vsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-series
	(mapcar #'clim3:vsprawl (clim3:children zone))))
   zone))
  
(defmethod clim3-ext:impose-child-layouts ((zone clim3:vbox))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let* ((width (clim3:width zone))
	 (height (clim3:height zone))
	 (children (clim3:children zone))
	 (vertical-sprawls (mapcar #'clim3:vsprawl children))
	 (heights (clim3-sprawl:sizes-in-series vertical-sprawls height)))
    (loop for vpos = 0 then (+ vpos height)
	  for height in heights
	  for child in children
	  do (setf (clim3-ext:hpos child) hpos)
	     (setf (clim3-ext:vpos child) vpos)
	     (clim3-ext:impose-size child width height))))
  
(defun clim3:vbox (children)
  (make-instance 'clim3:vbox :children (coerce-to-list-of-zones children)))

(defun clim3:vbox* (&rest children)
  (make-instance 'clim3:vbox :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HBOX.

(defclass clim3:hbox
    (clim3:standard-zone
     clim3-ext:list-children-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-both-sprawls-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ())

(defmethod clim3-ext:compute-hsprawl ((zone clim3:hbox))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:set-hsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-series
	(mapcar #'clim3:hsprawl (clim3:children zone))))
   zone))
  
(defmethod clim3-ext:compute-vsprawl ((zone clim3:hbox))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (clim3-ext:set-vsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'clim3:vsprawl (clim3:children zone))))
   zone))
  
(defmethod clim3-ext:impose-child-layouts ((zone clim3:hbox))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let* ((width (clim3:width zone))
	 (height (clim3:height zone))
	 (children (clim3:children zone))
	 (horizontal-sprawls (mapcar #'clim3:hsprawl children))
	 (widths (clim3-sprawl:sizes-in-series horizontal-sprawls width)))
    (loop for hpos = 0 then (+ hpos width)
	  for width in widths
	  for child in children
	  do (setf (clim3-ext:hpos child) hpos)
	     (setf (clim3-ext:vpos child) 0)
	     (clim3-ext:impose-size child width height))))

(defun clim3:hbox (children)
  (make-instance 'clim3:hbox :children (coerce-to-list-of-zones children)))

(defun clim3:hbox* (&rest children)
  (make-instance 'clim3:hbox :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PILE.

(defclass clim3:pile
    (clim3:standard-zone
     clim3-ext:list-children-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-both-sprawls-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-significant-mixin)
  ())

(defmethod clim3-ext:compute-hsprawl ((zone clim3:pile))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:set-hsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'clim3:hsprawl (clim3:children zone))))
   zone))
  
(defmethod clim3-ext:compute-vsprawl ((zone clim3:pile))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (clim3-ext:set-vsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3-sprawl:combine-in-parallel
	(mapcar #'clim3:vsprawl (clim3:children zone))))
   zone))
  
(defmethod clim3-ext:impose-child-layouts ((zone clim3:pile))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let* ((width (clim3:width zone))
	 (height (clim3:height zone))
	 (children (clim3:children zone)))
    (loop for child in children
	  do (setf (clim3-ext:hpos child) 0)
	     (setf (clim3-ext:vpos child) 0)
	     (clim3-ext:impose-size child width height))))

(defun clim3:pile (children)
  (make-instance 'clim3:pile :children (coerce-to-list-of-zones children)))

(defun clim3:pile* (&rest children)
  (make-instance 'clim3:pile :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GRID.

(defclass clim3:grid
    (clim3:standard-zone
     clim3-ext:matrix-children-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-both-sprawls-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ((%combined-rows :initform nil :accessor combined-rows)
   (%combined-cols :initform nil :accessor combined-cols)))

(defmethod clim3-ext:compute-hsprawl ((zone clim3:grid))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (let* ((children (clim3:children zone))
	 (rows (array-dimension children 0))
	 (cols (array-dimension children 1)))
    (cond ((= rows 1)
	   (setf (combined-cols zone)
		 (loop for col from 0 below cols
		       collect (clim3:hsprawl (aref children 0 col))))
	   (clim3-ext:set-hsprawl
	    (clim3-sprawl:combine-in-series (combined-cols zone))
	    zone))
	  ((= cols 1)
	   (clim3-ext:set-hsprawl
	    (clim3-sprawl:combine-in-parallel
	     (loop for row from 0 below rows
		   collect (clim3:hsprawl (aref children 0 row))))
	    zone))
	  (t
	   (setf (combined-cols zone)
		 (loop for col from 0 below cols
		       collect (clim3-sprawl:combine-in-parallel
				(loop for row from 0 below rows
				      collect (clim3:hsprawl (aref children row col))))))
	   (clim3-ext:set-hsprawl
	    (clim3-sprawl:combine-in-series (combined-cols children))
	    zone)))))

(defmethod clim3-ext:compute-vsprawl ((zone clim3:grid))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let* ((children (clim3:children zone))
	 (rows (array-dimension children 0))
	 (cols (array-dimension children 1)))
    (cond ((= rows 1)
	   (clim3-ext:set-vsprawl
	    (clim3-sprawl:combine-in-parallel
	     (loop for col from 0 below cols
		   collect (clim3:vsprawl (aref children 0 col))))
	    zone))
	  ((= cols 1)
	   (setf (combined-rows zone)
		 (loop for row from 0 below rows
		       collect (clim3:vsprawl (aref children 0 row))))
	   (clim3-ext:set-vsprawl
	    (clim3-sprawl:combine-in-series (combined-rows zone))
	    zone))
	  (t
	   (setf (combined-rows zone)
		 (loop for row from 0 below rows
		       collect (clim3-sprawl:combine-in-parallel
				(loop for col from 0 below cols
				      collect (clim3:vsprawl (aref children row col))))))
	   (clim3-ext:set-vsprawl
	    (clim3-sprawl:combine-in-series (combined-rows children))
	    zone)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SCROLL.

(defclass clim3:scroll
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:changing-child-hsprawl-changes-nothing-mixin
     clim3-ext:changing-child-vsprawl-changes-nothing-mixin
     clim3-ext:changing-child-position-changes-nothing-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod clim3-ext:compute-hsprawl ((zone clim3:scroll))
  (error "attempt to combine hsprawls of children of a scroll zone"))

(defmethod clim3-ext:compute-vsprawl ((zone clim3:scroll))
  (error "attempt to combine vsprawls of children of a scroll zone"))

(defmethod clim3-ext:impose-child-layouts ((zone clim3:scroll))
  ;; There is either no child or one child, but this is convenient.
  (clim3-ext:map-over-children
   (lambda (child)
     (clim3-ext:ensure-hsprawl-valid child)
     (clim3-ext:ensure-vsprawl-valid child)
     (multiple-value-bind (width height)
	 (clim3:natural-size child)
       (clim3-ext:impose-size child width height)))
   zone))

(defun clim3:scroll (&optional child)
  (make-instance 'clim3:scroll :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HSCROLL.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VSCROLL.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BBOARD.

(defclass clim3:bboard
    (clim3:standard-zone
     clim3-ext:list-children-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-both-sprawls-mixin
     clim3-ext:changing-child-position-changes-both-sprawls-mixin
     clim3-ext:child-depth-significant-mixin)
  ()
  (:default-initargs :vsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :hsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod clim3-ext:compute-hsprawl ((zone clim3:bboard))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:impose-child-layouts zone)
  (let ((max-hpos 0))
    (clim3-ext:map-over-children
     (lambda (child)
       (setf max-hpos (max max-hpos (+ (clim3:hpos child) (clim3:width child)))))
     zone)
    (clim3-ext:set-hsprawl (clim3-sprawl:sprawl max-hpos max-hpos nil) zone)))

(defmethod clim3-ext:compute-vsprawl ((zone clim3:bboard))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (clim3-ext:impose-child-layouts zone)
  (let ((max-vpos 0))
    (clim3-ext:map-over-children
     (lambda (child)
       (setf max-vpos (max max-vpos (+ (clim3:vpos child) (clim3:height child)))))
     zone)
    (clim3-ext:set-vsprawl (clim3-sprawl:sprawl max-vpos max-vpos nil) zone)))

(defmethod clim3-ext:impose-child-layouts ((zone clim3:bboard))
  (loop for child in (clim3:children zone)
	do (clim3-ext:ensure-hsprawl-valid child)
	   (clim3-ext:ensure-vsprawl-valid child)
	   (multiple-value-bind (width height)
	       (clim3:natural-size child)
	     (clim3-ext:impose-size child width height))))

(defun clim3:bboard (children)
  (make-instance 'clim3:bboard :children (coerce-to-list-of-zones children)))

(defun clim3:bboard* (&rest children)
  (make-instance 'clim3:bboard :children (coerce-to-list-of-zones children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPONGE.
;;;
;;; A sponge is a zone that can have at most one child.  It ignores
;;; the sprawls of its child, and imposes its own so that it becomes
;;; very elastic, both horizontally and vertically.

(defclass clim3:sponge
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:horizontally-very-elastic-mixin
     clim3-ext:vertically-very-elastic-mixin
     clim3-ext:changing-child-hsprawl-changes-nothing-mixin
     clim3-ext:changing-child-vsprawl-changes-nothing-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:sponge))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (setf (clim3-ext:hpos child) 0)
      (setf (clim3-ext:vpos child) 0)
      (clim3-ext:impose-size child width height))))

(defun clim3:sponge (&optional child)
  (make-instance 'clim3:sponge :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HSPONGE.
;;;
;;; A hsponge is a zone that can have at most one child.  It ignores
;;; the horizontal sprawl of its child, and imposes its own so that it
;;; becomes very elastic horizontally.  It copies the vertical sprawl
;;; of its child, or if it has no child, makes it very elastic
;;; vertically.

(defclass clim3:hsponge
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:horizontally-very-elastic-mixin
     clim3-ext:changing-child-hsprawl-changes-nothing-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-vsprawl-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ())

(defmethod clim3-ext:compute-vsprawl ((zone clim3:hsponge))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (clim3-ext:set-vsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3:vsprawl (car (clim3:children zone))))
   zone))

(defmethod clim3-ext:impose-child-layouts ((zone clim3:hsponge))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (setf (clim3-ext:hpos child) 0)
      (setf (clim3-ext:vpos child) 0)
      (clim3-ext:impose-size child width height))))

(defun clim3:hsponge (&optional child)
  (make-instance 'clim3:hsponge :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VSPONGE.
;;;
;;; A vsponge is a zone that can have at most one child.  It ignores
;;; the vertical sprawl of its child, and imposes its own so that it
;;; becomes very elastic vertically.  It copies the horizontal sprawl
;;; of its child, or if it has no child, makes it very elastic
;;; horizontally.

(defclass clim3:vsponge
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:vertically-very-elastic-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-nothing-mixin
     clim3-ext:changing-children-changes-hsprawl-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ())

(defmethod clim3-ext:compute-hsprawl ((zone clim3:vsponge))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:set-hsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3:hsprawl (car (clim3:children zone))))
   zone))

(defmethod clim3-ext:impose-child-layouts ((zone clim3:vsponge))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (setf (clim3-ext:hpos child) 0)
      (setf (clim3-ext:vpos child) 0)
      (clim3-ext:impose-size child width height))))

(defun clim3:vsponge (&optional child)
  (make-instance 'clim3:vsponge :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BRICK.
;;;
;;; A brick is a zone that can have at most one child.  It ignores
;;; the sprawls of its child, and imposes its own, which makes it very
;;; rigid, both horizontally and vertically.

(defclass clim3:brick
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:changing-child-hsprawl-changes-nothing-mixin
     clim3-ext:changing-child-vsprawl-changes-nothing-mixin
     clim3-ext:changing-children-changes-nothing-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:brick))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (setf (clim3-ext:hpos child) 0)
      (setf (clim3-ext:vpos child) 0)
      (clim3-ext:impose-size child width height))))

(defun clim3:brick (width height &optional child)
  (make-instance
   'clim3:brick
   :hsprawl (clim3-sprawl:sprawl width width width)
   :vsprawl (clim3-sprawl:sprawl height height height)
   :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HBRICK.
;;;
;;; A hbrick is a zone that can have at most one child.  It ignores
;;; the horizontal sprawl of its child, and imposes its own, which makes
;;; it very rigid horizontally.  It copies the vertical sprawl of its
;;; child, or if it has no child, makes it very elastic vertically.

(defclass clim3:hbrick
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:changing-child-hsprawl-changes-nothing-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-vsprawl-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ())

(defmethod clim3-ext:compute-vsprawl ((zone clim3:hbrick))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (clim3-ext:set-vsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3:vsprawl (clim3:children zone)))
   zone))

(defmethod clim3-ext:impose-child-layouts ((zone clim3:hbrick))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (setf (clim3-ext:hpos child) 0)
      (setf (clim3-ext:vpos child) 0)
      (clim3-ext:impose-size child width height))))

(defun clim3:hbrick (width &optional child)
  (make-instance
   'clim3:hbrick
   :hsprawl (clim3-sprawl:sprawl width width width)
   :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VBRICK.
;;;
;;; A vbrick is a zone that can have at most one child.  It ignores
;;; the vertical sprawl of its child, and imposes its own, which makes
;;; it very rigid vertically.  It copies the horizontal sprawl of its
;;; child, or if it has no child, makes it very elastic horizontally.

(defclass clim3:vbrick
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-nothing-mixin
     clim3-ext:changing-children-changes-hsprawl-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ())

(defmethod clim3-ext:compute-hsprawl ((zone clim3:vbrick))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:set-hsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3:hsprawl (clim3:children zone)))
   zone))

(defmethod clim3-ext:impose-child-layouts ((zone clim3:vbrick))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (setf (clim3-ext:hpos child) 0)
      (setf (clim3-ext:vpos child) 0)
      (clim3-ext:impose-size child width height))))

(defun clim3:vbrick (height &optional child)
  (make-instance
   'clim3:vbrick
   :vsprawl (clim3-sprawl:sprawl height height height)
   :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HFRAME.
;;;
;;; A hframe is a zone that can have at most one child.  It ignores
;;; the horizontal sprawl of its child, and imposes its own.  It
;;; copies the vertical sprawl of its child, or if it has no child,
;;; makes it very elastic vertically.

(defclass clim3:hframe
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:changing-child-hsprawl-changes-nothing-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-vsprawl-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ())

(defmethod clim3-ext:compute-vsprawl ((zone clim3:hframe))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (clim3-ext:set-vsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3:vsprawl (clim3:children zone)))
   zone))

(defmethod clim3-ext:impose-child-layouts ((zone clim3:hframe))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (setf (clim3-ext:hpos child) 0)
      (setf (clim3-ext:vpos child) 0)
      (clim3-ext:impose-size child width height))))

(defun clim3:hframe (min-width width max-width &optional child)
  (make-instance
   'clim3:hframe
   :hsprawl (clim3-sprawl:sprawl min-width width max-width)
   :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VFRAME.
;;;
;;; A vframe is a zone that can have at most one child.  It ignores
;;; the vertical sprawl of its child, and imposes its own.  It copies
;;; the horizontal sprawl of its child, or if it has no child, makes
;;; it very elastic horizontally.

(defclass clim3:vframe
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-nothing-mixin
     clim3-ext:changing-children-changes-hsprawl-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ())

(defmethod clim3-ext:compute-hsprawl ((zone clim3:vframe))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:set-hsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3:hsprawl (clim3:children zone)))
   zone))

(defmethod clim3-ext:impose-child-layouts ((zone clim3:vframe))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (setf (clim3-ext:hpos child) 0)
      (setf (clim3-ext:vpos child) 0)
      (clim3-ext:impose-size child width height))))

(defun clim3:vframe (min-height height max-height &optional child)
  (make-instance
   'clim3:vframe
   :vsprawl (clim3-sprawl:sprawl min-height height max-height)
   :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WRAP
;;; 
;;; A wrap zone is a zone that has at most one child.  If it has a
;;; child, then it acquires the same sprawls as the child.  If not, it
;;; becomes very elastic.
;;;
;;; This class can be subclassed for convenience

(defclass clim3:wrap
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-both-sprawls-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ()
  (:default-initargs :children '()))

(defmethod clim3-ext:compute-hsprawl ((zone clim3:wrap))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:set-hsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3:hsprawl (clim3:children zone)))
   zone))

(defmethod clim3-ext:compute-vsprawl ((zone clim3:wrap))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (clim3-ext:set-vsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (clim3:vsprawl (clim3:children zone)))
   zone))

(defmethod clim3-ext:impose-child-layouts ((zone clim3:wrap))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (setf (clim3-ext:hpos child) 0)
      (setf (clim3-ext:vpos child) 0)
      (clim3-ext:impose-size child width height))))

;;; These constructors would typically not be used.  Instead, client
;;; code would use MAKE-INSTANCE on the subclass of the wrap.

(defun clim3:wrap (&optional child)
  (make-instance 'clim3:wrap :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BORDER

(defclass clim3:border
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-both-sprawls-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ((%thickness :initarg :thickness :reader thickness))
  (:default-initargs :children '()))

(defmethod clim3-ext:compute-hsprawl ((zone clim3:border))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:set-hsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (let* ((sprawl (clim3:hsprawl (clim3:children zone)))
	      (min-size (clim3-sprawl:min-size sprawl))
	      (size (clim3-sprawl:size sprawl))
	      (max-size (clim3-sprawl:max-size sprawl))
	      (thickness (thickness zone)))
	 (clim3-sprawl:sprawl (+ (* 2 thickness) min-size)
			      (+ (* 2 thickness) size)
			      (if (null max-size)
				  nil
				  (+ (* 2 thickness) max-size)))))
   zone))

(defmethod clim3-ext:compute-vsprawl ((zone clim3:border))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (clim3-ext:set-vsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (let* ((sprawl (clim3:vsprawl (clim3:children zone)))
	      (min-size (clim3-sprawl:min-size sprawl))
	      (size (clim3-sprawl:size sprawl))
	      (max-size (clim3-sprawl:max-size sprawl))
	      (thickness (thickness zone)))
	 (clim3-sprawl:sprawl (+ (* 2 thickness) min-size)
			      (+ (* 2 thickness) size)
			      (if (null max-size)
				  nil
				  (+ (* 2 thickness) max-size)))))
   zone))

(defmethod clim3-ext:impose-child-layouts ((zone clim3:border))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone))
	(thickness (thickness zone)))
    (unless (null child)
      (setf (clim3-ext:hpos child) thickness)
      (setf (clim3-ext:vpos child) thickness)
      (clim3-ext:impose-size child
			     (max 0 (- width (* 2 thickness)))
			     (max 0 (- height (* 2 thickness)))))))

(defun clim3:border (thickness child)
  (make-instance 'clim3:border
    :thickness thickness
    :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXED-POSITION
;;;
;;; This class is not meant to be instantiated directly.  Instead it
;;; is the superclass of 9 zone classes, each of which positions a
;;; single child in a fixed relative position (top, bottom, left,
;;; right, top-left, top-right, bottom-left, bottom-right, center) and
;;; gives the child its natural size (or the size of the layout zone
;;; if the natural size of the child is too great.

(defclass fixed-position
    (clim3:standard-zone
     clim3-ext:at-most-one-child-mixin
     clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin
     clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin
     clim3-ext:changing-children-changes-both-sprawls-mixin
     clim3-ext:changing-child-position-not-allowed-mixin
     clim3-ext:child-depth-insignificant-mixin)
  ()
  (:default-initargs :children '()))

(defmethod clim3-ext:compute-hsprawl ((zone fixed-position))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:set-hsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (let* ((sprawl (clim3:hsprawl (clim3:children zone)))
	      (min-size (clim3-sprawl:min-size sprawl))
	      (size (clim3-sprawl:size sprawl)))
	 (clim3-sprawl:sprawl min-size size nil)))
   zone))

(defmethod clim3-ext:compute-vsprawl ((zone fixed-position))
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (clim3-ext:set-vsprawl
   (if (null (clim3:children zone))
       (clim3-sprawl:sprawl 0 0 nil)
       (let* ((sprawl (clim3:vsprawl (clim3:children zone)))
	      (min-size (clim3-sprawl:min-size sprawl))
	      (size (clim3-sprawl:size sprawl)))
	 (clim3-sprawl:sprawl min-size size nil)))
   zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CENTER

(defclass clim3:center (fixed-position) ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:center))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (multiple-value-bind (child-width child-height)
	  (clim3:natural-size child)
	(let ((hgap (max 0 (floor (- width child-width) 2)))
	      (vgap (max 0 (floor (- height child-height) 2)))
	      (w (min width child-width))
	      (h (min height child-height)))
	  (setf (clim3-ext:hpos child) hgap)
	  (setf (clim3-ext:vpos child) vgap)
	  (clim3-ext:impose-size child w h))))))

(defun clim3:center (&optional child)
  (make-instance 'clim3:center
    :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TOP

(defclass clim3:top (fixed-position) ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:top))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (multiple-value-bind (child-width child-height)
	  (clim3:natural-size child)
	(let ((hgap (max 0 (floor (- width child-width) 2)))
	      (vgap 0)
	      (w (min width child-width))
	      (h (min height child-height)))
	  (setf (clim3-ext:hpos child) hgap)
	  (setf (clim3-ext:vpos child) vgap)
	  (clim3-ext:impose-size child w h))))))

(defun clim3:top (&optional child)
  (make-instance 'clim3:top
    :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BOTTOM

(defclass clim3:bottom (fixed-position) ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:bottom))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (multiple-value-bind (child-width child-height)
	  (clim3:natural-size child)
	(let ((hgap (max 0 (floor (- width child-width) 2)))
	      (vgap (max 0 (- height child-height)))
	      (w (min width child-width))
	      (h (min height child-height)))
	  (setf (clim3-ext:hpos child) hgap)
	  (setf (clim3-ext:vpos child) vgap)
	  (clim3-ext:impose-size child w h))))))

(defun clim3:bottom (&optional child)
  (make-instance 'clim3:bottom
    :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEFT

(defclass clim3:left (fixed-position) ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:left))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (multiple-value-bind (child-width child-height)
	  (clim3:natural-size child)
	(let ((hgap 0)
	      (vgap (max 0 (floor (- height child-height) 2)))
	      (w (min width child-width))
	      (h (min height child-height)))
	  (setf (clim3-ext:hpos child) hgap)
	  (setf (clim3-ext:vpos child) vgap)
	  (clim3-ext:impose-size child w h))))))

(defun clim3:left (&optional child)
  (make-instance 'clim3:left
    :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RIGHT

(defclass clim3:right (fixed-position) ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:right))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (multiple-value-bind (child-width child-height)
	  (clim3:natural-size child)
	(let ((hgap (max 0 (- width child-width)))
	      (vgap (max 0 (floor (- height child-height) 2)))
	      (w (min width child-width))
	      (h (min height child-height)))
	  (setf (clim3-ext:hpos child) hgap)
	  (setf (clim3-ext:vpos child) vgap)
	  (clim3-ext:impose-size child w h))))))

(defun clim3:right (&optional child)
  (make-instance 'clim3:right
    :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TOP-LEFT

(defclass clim3:top-left (fixed-position) ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:top-left))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (multiple-value-bind (child-width child-height)
	  (clim3:natural-size child)
	(let ((hgap 0)
	      (vgap 0)
	      (w (min width child-width))
	      (h (min height child-height)))
	  (setf (clim3-ext:hpos child) hgap)
	  (setf (clim3-ext:vpos child) vgap)
	  (clim3-ext:impose-size child w h))))))

(defun clim3:top-left (&optional child)
  (make-instance 'clim3:top-left
    :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TOP-RIGHT

(defclass clim3:top-right (fixed-position) ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:top-right))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (multiple-value-bind (child-width child-height)
	  (clim3:natural-size child)
	(let ((hgap (max 0 (- width child-width)))
	      (vgap 0)
	      (w (min width child-width))
	      (h (min height child-height)))
	  (setf (clim3-ext:hpos child) hgap)
	  (setf (clim3-ext:vpos child) vgap)
	  (clim3-ext:impose-size child w h))))))

(defun clim3:top-right (&optional child)
  (make-instance 'clim3:top-right
    :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BOTTOM-LEFT

(defclass clim3:bottom-left (fixed-position) ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:bottom-left))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (multiple-value-bind (child-width child-height)
	  (clim3:natural-size child)
	(let ((hgap 0)
	      (vgap (max 0 (- height child-height)))
	      (w (min width child-width))
	      (h (min height child-height)))
	  (setf (clim3-ext:hpos child) hgap)
	  (setf (clim3-ext:vpos child) vgap)
	  (clim3-ext:impose-size child w h))))))

(defun clim3:bottom-left (&optional child)
  (make-instance 'clim3:bottom-left
    :children child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BOTTOM-RIGHT

(defclass clim3:bottom-right (fixed-position) ())

(defmethod clim3-ext:impose-child-layouts ((zone clim3:bottom-right))
  (clim3-ext:map-over-children #'clim3-ext:ensure-hsprawl-valid zone)
  (clim3-ext:map-over-children #'clim3-ext:ensure-vsprawl-valid zone)
  (let ((width (clim3:width zone))
	(height (clim3:height zone))
	(child (clim3:children zone)))
    (unless (null child)
      (multiple-value-bind (child-width child-height)
	  (clim3:natural-size child)
	(let ((hgap (max 0 (- width child-width)))
	      (vgap (max 0 (- height child-height)))
	      (w (min width child-width))
	      (h (min height child-height)))
	  (setf (clim3-ext:hpos child) hgap)
	  (setf (clim3-ext:vpos child) vgap)
	  (clim3-ext:impose-size child w h))))))

(defun clim3:bottom-right (&optional child)
  (make-instance 'clim3:bottom-right
    :children child))

