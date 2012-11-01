(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AT-MOST-ONE-CHILD-MIXIN.

(defclass at-most-one-child-mixin () ())

;;; Around method for (SETF CHILDREN)
;;; This around method is charged with a few things:
;;;
;;;  * Check that the children is a sequence of at most one element.
;;;
;;;  * Check that no new child already has a parent.
;;;
;;;  * Set the parent of every removed child to NIL.
;;;
;;;  * Notify the client of changes in parent-child relations.

(defmethod (setf children) :around
  (new-children (zone at-most-one-child-mixin))
  (let ((children-before (children zone)))
    (unless (null children-before)
      (setf (parent (car children-before)) nil)))
  (let ((children-after
	  (cond ((or (null new-children)
		     (and (consp new-children) (null (cdr new-children))))
		 new-children)
		((consp new-children)
		 (error "List must be a proper list of length at most 1: ~s"
			new-children))
		((vectorp new-children)
		 (case (length new-children)
		   (0 '())
		   (1 (list (aref new-children 0)))
		   (t (error "Vector must have at most one element"))))
		(t
		 (error "Children must be a proper sequence of length at most 1 ~s"
			new-children)))))
    (unless (null children-after)
      (unless (null (parent (car children-after)))
	(error "Attempt to connect a zone that is already connected ~s"
	       (car children-after)))
      (unless (zone-p (car children-after))
	(error "Child must be a zone: ~s" (car children-after)))
      (setf (parent (car children-after)) zone))
    (call-next-method children-after zone)))

(defmethod map-over-children-top-to-bottom
    (function (zone at-most-one-child-mixin))
  (unless (null (children zone))
    (funcall function (car (children zone)))))

(defmethod map-over-children-bottom-to-top
    (function (zone at-most-one-child-mixin))
  (unless (null (children zone))
    (funcall function (car (children zone)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ANY-NUMBER-OF-CHILDREN-MIXIN.

(defclass any-number-of-children-mixin ()
  ((%depth-ordered-children :initform nil :accessor depth-ordered-children)))

;;; Around method for (SETF CHILDREN)
;;; This around method is charged with a few things:
;;;
;;;  * Check that no new child already has a parent.
;;;
;;;  * Set the parent of every removed child to NIL.
;;;
;;;  * Destroy the depth-ordered vector of children.
;;;
;;;  * Notify the client of changes in parent-child relations.

(defmethod (setf children) :around
  (new-children (zone any-number-of-children-mixin))
  (let ((children-before (children zone))
	(table-children-before (make-hash-table :test #'eq))
	(table-children-after (make-hash-table :test #'eq)))
    ;; Fill the table with the current children of the zone.
    (map-over-children (lambda (child) 
			 (setf (gethash child table-children-before) t))
		       zone)
    ;; Set the new children.
    (call-next-method)
    ;; Fill the table with the new children of the zone.
    (map-over-children (lambda (child) 
			 (setf (gethash child table-children-after) t))
		       zone)
    ;; Check that no new child already has a parent.
    (maphash (lambda (new-child value)
	       (declare (ignore value))
	       (unless (gethash new-child table-children-before)
		 (unless (null (parent new-child))
		   ;; Restore the old children.
		   (call-next-method children-before zone)
		   (error "Attemps to add a child that already has a parent ~s."
			  new-child))))
	     table-children-after)
    ;; Destroy the depth-ordered vector of children.
    (setf (depth-ordered-children zone) nil)
    ;; Set parent of new children.
    (maphash (lambda (new-child value)
	       (declare (ignore value))
	       (unless (gethash new-child table-children-before)
		 (setf (parent new-child) zone)))
	     table-children-after)
    ;; Set the parent of removed children to nil.
    (maphash (lambda (old-child value)
	       (declare (ignore value))
	       (unless (gethash old-child table-children-after)
		 (setf (parent old-child) nil)))
	     table-children-before)))


(defmethod map-over-children-top-to-bottom
    (function (zone any-number-of-children-mixin))
  (ensure-depth-ordered-children zone)
  (let ((depth-ordered-children (depth-ordered-children zone)))
    (loop for i from 0 below (length depth-ordered-children)
	  do (funcall function (aref depth-ordered-children i)))))

(defmethod map-over-children-bottom-to-top
    (function (zone any-number-of-children-mixin))
  (ensure-depth-ordered-children zone)
  (let ((depth-ordered-children (depth-ordered-children zone)))
    (loop for i downfrom (1- (length depth-ordered-children)) to 0
	  do (funcall function (aref depth-ordered-children i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-HSPRAWL-CHANGES-HSPRAWL-MIXIN

(defclass changing-child-hsprawl-changes-hsprawl-mixin () ())

(defmethod notify-child-hsprawl-changed
    ((child zone) (parent changing-child-hsprawl-changes-hsprawl-mixin))
  (setf (hsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-VSPRAWL-CHANGES-VSPRAWL-MIXIN

(defclass changing-child-vsprawl-changes-vsprawl-mixin () ())

(defmethod notify-child-vsprawl-changed
    ((child zone) (parent changing-child-vsprawl-changes-vsprawl-mixin))
  (setf (vsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-HSPRAWL-CHANGES-CHILD-LAYOUTS-MIXIN

(defclass changing-child-hsprawl-changes-child-layouts-mixin () ())

(defmethod notify-child-hsprawl-changed
    ((child zone) (parent changing-child-hsprawl-changes-child-layouts-mixin))
  (setf (child-layouts-valid-p parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-VSPRAWL-CHANGES-CHILD-LAYOUTS-MIXIN

(defclass changing-child-vsprawl-changes-child-layouts-mixin () ())

(defmethod notify-child-vsprawl-changed
    ((child zone) (parent changing-child-vsprawl-changes-child-layouts-mixin))
  (setf (child-layouts-valid-p parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-HSPRAWL-CHANGES-NOTHING-MIXIN

(defclass changing-child-hsprawl-changes-nothing-mixin () ())

(defmethod notify-child-hsprawl-changed
    ((child zone) (parent changing-child-hsprawl-changes-nothing-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-VSPRAWL-CHANGES-NOTHING-MIXIN

(defclass changing-child-vsprawl-changes-nothing-mixin () ())

(defmethod notify-child-vsprawl-changed
    ((child zone) (parent changing-child-vsprawl-changes-nothing-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-HSPRAWL-MIXIN

(defclass changing-children-changes-hsprawl-mixin () ())

(defmethod notify-children-changed
    ((parent changing-children-changes-hsprawl-mixin))
  (setf (hsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-VSPRAWL-MIXIN

(defclass changing-children-changes-vsprawl-mixin () ())

(defmethod notify-children-changed
    ((parent changing-children-changes-vsprawl-mixin))
  (setf (vsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-BOTH-SPRAWLS-MIXIN

(defclass changing-children-changes-both-sprawls-mixin () ())

(defmethod notify-children-changed
    ((parent changing-children-changes-both-sprawls-mixin))
  (setf (hsprawl parent) nil)
  (setf (vsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-CHILD-LAYOUTS-MIXIN

(defclass changing-children-changes-child-layouts-mixin () ())

(defmethod notify-children-changed
    ((parent changing-children-changes-child-layouts-mixin))
  (setf (child-layouts-valid-p parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-NOTHING-MIXIN

(defclass changing-children-changes-nothing-mixin () ())

(defmethod notify-children-changed
    ((parent changing-children-changes-nothing-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-NOT-ALLOWED-MIXIN

(defclass changing-child-position-not-allowed-mixin () ())

(defmethod notify-changing-child-position
    ((child zone) (parent changing-child-position-not-allowed-mixin))
  (error "attempt to change the position of a child"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-HSPRAWL-MIXIN

(defclass changing-child-position-changes-hsprawl-mixin () ())

(defmethod notify-changing-child-position
    ((child zone) (parent changing-child-position-changes-hsprawl-mixin))
  (setf (hsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-VSPRAWL-MIXIN

(defclass changing-child-position-changes-vsprawl-mixin () ())

(defmethod notify-changing-child-position
    ((child zone) (parent changing-child-position-changes-vsprawl-mixin))
  (setf (vsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-BOTH-SPRAWLS-MIXIN

(defclass changing-child-position-changes-both-sprawls-mixin () ())

(defmethod notify-changing-child-position
    ((child zone) (parent changing-child-position-changes-both-sprawls-mixin))
  (setf (hsprawl parent) nil)
  (setf (vsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-CHILD-LAYOUTS-MIXIN

(defclass changing-child-position-changes-child-layouts-mixin () ())

(defmethod notify-changing-child-position
    ((child zone) (parent changing-child-position-changes-child-layouts-mixin))
  (setf (child-layouts-valid-p parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-DEPTH-NOT-ALLOWED-MIXIN

(defclass changing-child-depth-not-allowed-mixin () ())

(defmethod notify-changing-child-depth
    ((child zone) (parent changing-child-depth-not-allowed-mixin))
  (error "attempt to change the depth of a child"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-DEPTH-CHANGES-NOTHING-MIXIN

(defclass changing-child-depth-changes-nothing-mixin () ())

(defmethod notify-changing-child-depth
    ((child zone) (parent changing-child-depth-changes-nothing-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-DEPTH-CHANGES-CHILD-LAYOUTS-MIXIN

(defclass changing-child-depth-changes-child-layouts-mixin () ())

(defmethod notify-changing-child-depth
    ((child zone) (parent changing-child-depth-changes-child-layouts-mixin))
  (setf (child-layouts-valid-p parent) nil))


