(defpackage #:clim-3
    (:use #:common-lisp))

(in-package #:clim-3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Boxes

(defclass box ()
  (;; when nil, this box was created since last redraw.
   (%previous-region :initform nil :accessor previous-region)
   ;; when nil, this box was removed since last redraw.
   (%current-region :initarg :region :accessor current-region)))

(defclass opaque-mixin () ())

(defgeneric opaquep (box)
  (:method (box) (declare (ignore box)) nil)
  (:method ((box opaque-mixin)) (declare (ignore box)) t))

(defclass primitive-box (box) ())

(defclass complex-box (box)
  ((%first-child :initarg :first-child :accessor first-child)
   (%last-child :initarg :last-child :accessor last-child)
   ;; true if and only if any of the children has changed since last redraw.
   (%modified-p :initarg :modified-p :initform nil :accessor modified-p)
   ;; true if and only if it is best to redraw the entire box.
   (%damaged-p :initarg :damaged-p :initform nil :accessor damaged-p)))

(defmethod initialize-instance :after ((box complex-box)
				       &key children
				       &allow-other-keys)
  (multiple-value-bind (first last)
      (make-double-list-from-list children)
    (setf (first-child box) first
	  (last-child box) last)))

(defclass primitive-opaque-box (primitive-box opaque-mixin) ())

(defclass complex-opaque-box (complex-box opaque-mixin) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Doubly linked list cell

(defclass double-cell ()
  ((%data :initarg :data :accessor data)
   (%prev :initarg :prev :initform nil :accessor prev)
   (%next :initarg :next :initform nil :accessor next)))

;;; take a list and return a doubly-linked list
;;; with the same elements as two values, the first
;;; and the last cells, or nil if the list is empty.
(defun make-double-list-from-list (list)
  (if (null list)
      (values nil nil)
      (let* ((first (make-instance 'double-cell :data (car list)))
	     (last first))
	(loop for element in (cdr list)
	      do (let ((new (make-instance 'double-cell
			      :data element
			      :prev last)))
		   (setf (next last) new
			 last new)))
	(values first last))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Redrawing a box

;;; the current region of the box, and the region argument are 
;;; both in the coordinate system of the parent box. 
;;; x and y are the absolute coordinates of the upper-left corner
;;; of the parent box.
(defgeneric redraw-box (box region x y))

(defmethod redraw-box ((box primitive-box) region x y)
  (let* ((transformation (clim:make-translation-transformation x y))
	 (new-region (clim:transform-region transformation region)))
    (format t "redrawing the box: ~s inside the region ~s~%" box new-region)))

(defmethod redraw-box :around (box region x y)
  (let ((restricted-region (clim:region-intersection region (current-region box))))
    (unless (eq restricted-region clim:+nowhere+)
      (call-next-method box restricted-region x y))))

(defmethod redraw-box ((box complex-box) region x y)
  (let* ((xx (clim:rectangle-min-x (current-region box)))
	 (yy (clim:rectangle-min-y (current-region box)))
	 (new-region
	  (clim:untransform-region
	   (clim:make-translation-transformation xx yy)
	   region)))
    (loop for children = (last-child box) then (prev children)
	  until (null children)
	  do (redraw-box (data children) new-region (+ x xx) (+ y yy)))))

(defmethod redraw-box :before ((box opaque-mixin) region x y)
  (let* ((transformation (clim:make-translation-transformation x y))
	 (new-region (clim:transform-region transformation region)))
    (format t "redrawing background of ~s inside the region ~s~%" box new-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing a region to redraw

;;; Compute the region to be redrawn by boxes underneath this one.
;;; arguments:
;;;   box 
;;;      a box that might influence the region to redraw
;;;   region-to-redraw 
;;;      a region above this box that needs to be redrawn 
;;;      taking into account only boxes above this one
;;;   visible-region-after
;;;      a region that is visible after the redraw
;;;   visible-translucent-covering-region-after
;;;      a region that, after the redraw, is visible, contains some 
;;;      boxes above this one, and that might be translucent. 
;;;      if a new box needs to be drawn, the boxes underneath it
;;;      do not have to be redrawn if it is outside this region.
;;;      It can just be drawn on top of the ones underneath.  If, however,
;;;      it is inside this region, some box above it might be 
;;;      influenced by drawing this one, so the area needs to be "cleared"
;;;      and redrawn by boxes underneath. 
;;;
;;; this function returns xxx values:
;;;   - the new region to redraw
;;;   - the new visible-translucent-covering-region-after

(defgeneric compute-new-regions (box
				 region-to-redraw
				 visible-region-after
				 visible-translucent-covering-region-after))

(defmethod compute-new-regions ((box primitive-box)
				region-to-redraw
				visible-region-after
				visible-translucent-covering-region-after)
  (if (or (null (previous-region box))
	  (null (current-region box))
	  (not (clim:region-equal (previous-region box)
				  (current-region box))))
      (let ((previous-region (or (previous-region box) clim:+nowhere+))
	    (current-region (or (current-region box) clim:+nowhere+)))
	(values (clim:region-union
		 region-to-redraw
		 (clim:region-intersection
		  visible-region-after
		  (clim:region-union
		   previous-region
		   (clim:region-intersection
		    current-region
		    visible-translucent-covering-region-after))))
		visible-region-after
		(clim:region-union
		 visible-translucent-covering-region-after
		 (clim:region-intersection
		  current-region
		  visible-region-after)))
      (values region-to-redraw
	      visible-region-after
	      (clim:region-union
	       visible-translucent-covering-region-after
	       (clim:region-intersection
		current-region
		visible-region-after))))))

(defmethod compute-new-regions ((box opaque-mixin)
				region-to-redraw
				visible-region-after
				visible-translucent-covering-region-after)
  (let ((previous-region (or (previous-region box) clim:+nowhere+))
	(current-region (or (current-region box) clim:+nowhere+)))
    (values (clim:region-union
	     region-to-redraw
	     (clim:region-intersection
	      visible-region-after
	      (clim:region-difference
	       previous-region
	       current-region)))
	    (clim:region-difference
	     visible-region-after
	     current-region)
	    (clim:region-difference
	     visible-translucent-covering-region-after
	     current-region))))
	    
(defmethod compute-new-regions ((box complex-box)
				region-to-redraw
				visible-region-after
				visible-translucent-covering-region-after)
  
  (cond ((or (null (previous-region box))
	     (null (current-region box))
	     (not (clim:region-equal (previous-region box)
				     (current-region box)))
	     (damaged-p box))
	 ;; A lot of changes have occured.  Treat this box
	 ;; as if it were a moved primitive box. 
	 (let ((previous-region (or (previous-region box) clim:+nowhere+))
	       (current-region (or (current-region box) clim:+nowhere+)))
	   (values (clim:region-union
		    region-to-redraw
		    (clim:region-intersection
		     visible-region-after
		     (clim:region-union
		      previous-region
		      (clim:region-intersection
		       current-region
		       visible-translucent-covering-region-after))))
		   visible-region-after
		   (clim:region-union
		    visible-translucent-covering-region-after
		    (clim:region-intersection
		     current-region)))))
	((not (modified-p box))
	 ;; No changes have occured.
	 (values region-to-redraw
		 visible-region-after
		 (clim:region-union
		  visible-translucent-covering-region-after
		  (clim:region-intersection
		   current-region))))
	(t
	 ;; Changes have occured in some children.
	 (let ((transformation (clim:make-translation-transformation
				(clim:rectangle-min-x (current-region box))
				(clim:rectangle-min-y (current-region box)))))
	   (let ((sub-region-to-redraw
		  (clim:untransform-region
		   transformation
		   (clim:region-intersection
		    region-to-redraw
		    (current-region box))))
		 (sub-visible-region-after
		  (clim:untransform-region
		   transformation
		   (clim:region-intersection
		    visible-region-after
		    (current-region box))))
		 (sub-visible-translucent-covering-region-after
		  (clim:untransform-region
		   transformation
		   (clim:region-intersection
		    visible-translucent-covering-region-after
		    (current-region box)))))
	     (loop for children = (first-child box) then (next children)
		   until (null children)
		   do (multiple-value-bind (new-region-to-redraw
					    new-visible-region-after
					    new-visible-translucent-covering-region-after)
			  (compute-new-regions (data children)
					       sub-region-to-redraw
					       sub-visible-region-after
					       sub-visible-translucent-covering-region-after)
			(setf sub-region-to-redraw new-region-to-redraw
			      sub-visible-region-after new-visible-region-after
			      sub-visible-translucent-covering-region-after new-visible-translucent-covering-region-after)))
	     (values (clim:transform-region
		      transformation
		      sub-region-to-redraw)
		     (clim:transform-region
		      transformation
		      sub-visible-region-after)
		     (clim:transform-region
		      transformation
		      sub-visible-translucent-covering-region-after)))))))

(defun redraw (box)
  (let* ((region-to-redraw (compute-new-regions
			    box
			    clim:+nowhere+
			    clim:+everywhere+
			    clim:+everywhere+)))
    (redraw-box box region-to-redraw 0 0)))
