(defpackage #:clim-3
    (:use #:common-lisp))

(in-package #:clim-3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The design protocol

;;; Return a region that should be redrawn below DESIGN as a result of
;;; changes to it.  The primary methods can count on the
;;; previous-region on being non NIL.
(defgeneric compute-redraw-region-below (design))

;;; Return a region that should be redrawn above DESIGN as a result of
;;; changes to it.
(defgeneric compute-redraw-region-above (design))

;;; Return a region where DESIGN was opaque before redraw
(defgeneric opaque-region-before (design))

;;; Return a region where DESIGN is opaque after redraw
(defgeneric opaque-region-after (design))

;;; Redraw the part of DESIGN falls inside the intersection of REGION
;;; and CLIPPING-REGION at POSITION.  All coordinates are device
;;; coordinates.  Return the region actually drawn. 
(defgeneric redraw-design (design position region clipping-region))

(defclass design ()
  (;; A value of nil indicates that region was created
   ;; since the last redraw.
   (%previous-region :initform nil :accessor previous-region)
   ;; A value of nil indicates that the region was deleted
   ;; since the last redraw.
   (%current-region :initarg :region :accessor current-region)))

(defmethod previous-region :around ((design design))
  (or (call-next-method) clim:+nowhere+))

(defmethod current-region :around ((design design))
  (or (call-next-method) clim:+nowhere+))
      
(defun moved-p (design)
  (and (not (eq (previous-region design) clim:+nowhere+))
       (not (eq (previous-region design) (current-region design)))))

(defmethod opaque-region-before ((design design))
  clim:+nowhere+)

(defmethod opaque-region-after ((design design))
  clim:+nowhere+)

(defclass primitive-design (design) ())

(defmethod compute-redraw-region-below ((design primitive-design))
  (previous-region design))

(defmethod compute-redraw-region-above ((design primitive-design))
  (current-region design))

(defmethod redraw-design ((design primitive-design)
			  position region clipping-region)
  (draw-design design
	       position
	       (clim:region-intersection region clipping-region)))

(defclass complex-design (design)
  ((%modified-p :initform nil :accessor modified-p)
   (%children :initarg :children :accessor children)))

(defclass compute-redraw-region-below ((design complex-design))
  (loop for child in (children design)
	for region = clim:+nowhere+ then (clim:region-union
					  region
					  (clim:region-intersection
					   (previous-region child)
					   (previous-region design)))
	finally (return region)))

(defclass compute-redraw-region-above ((design complex-design))
  (loop for child in (children design)
	for region = clim:+nowhere+ then (clim:region-union
					  region
					  (clim:region-intersection
					   (current-region child)
					   (current-region design)))
	finally (return region)))
  

(defun redraw-children (children position region clipping-region)
  (if (null children)
      nil
      (let ((region-drawn
	     (redraw-children (cdr children)
			      position
			      (clim:region-union
			       region
			       (compute-redraw-region-below
				(car children)))
			      (clim:region-intersection
			       clipping-region
			       (compute-clipping-region-below
				(car children))))))
	(redraw-design (car children)
		       position
		       

(defmethod redraw-design ((design complex-design)
			  position region clipping-region)
  (redraw-children (children design)
		   position
		   region
		   (clim:region-intersection
		    region clipping-region)))


(defclass opaque-design-mixin () ())

