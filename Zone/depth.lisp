(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Every zone has a depth.  Although stored as part of a zone, the
;;; depth information really belongs to the parent of the zone,
;;; because it determines the order of traversal of the children in
;;; some circumstances..

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DEPTH.
;;;
;;; Return the depth of a zone.

(defgeneric clim3:depth (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF DEPTH).
;;;
;;; Set the depth of a zone.  This is the function that is normally
;;; used by client code that wishes to modify the depth of a zone
;;; relative to its sibling zones.
;;;
;;; Modifying the depth by using this generic function automatically
;;; triggers the depth-notification protocol which informs the parent
;;; that the depth of one of its children has changed.  This
;;; notification is done by an :AFTER method on this generic function,
;;; specialized for DEPTH-MIXIN. 

(defgeneric (setf clim3:depth) (new-depth zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-DEPTH.
;;;
;;; Set the dept of a zone without triggering the depth notification
;;; protocol.  This function is not used by normal client code.  It is
;;; used by parents who wish to alter the depth of its children
;;; without triggering the depth-notification protocol.

(defgeneric clim3-ext:set-depth (new-depth zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAP-OVER-CHILDREN-TOP-TO-BOTTOM.
;;;
;;; The first argument is a function of a single argument.  The second
;;; argument is a zone.  This function calls the function given as the
;;; first argument on each child of the zone given as a second argument.
;;;
;;; The depths of the children are respected only if they are
;;; important to the rendering of the zone.  Commpound zones for which
;;; the depths of the children are important should include
;;; CHILD-DEPTH-SIGNIFICANT-MIXIN in its superclasses.  Compound zones
;;; for which the depth of the children is are not imporant should
;;; include CHILD-DEPTH-INSIGNIFICANT-MIXIN in its superclasses. 
;;;
;;; This function is used mainly for traversing zones to determine
;;; whether the pointer is inside.  For that reason, it maps over ALL
;;; children, i.e. including hidden children (also called cuckoos).

(defgeneric clim3-ext:map-over-children-top-to-bottom (function zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAP-OVER-CHILDREN-BOTTOM-TO-TOP.
;;;
;;; The first argument is a function of a single argument.  The second
;;; argument is a zone.  This function calls the function given as the
;;; first argument on each child of the zone given as a second argument.  
;;;
;;; The depths of the children are respected only if they are
;;; important to the rendering of the zone.  Commpound zones for which
;;; the depths of the children are important should include
;;; CHILD-DEPTH-SIGNIFICANT-MIXIN in its superclasses.  Compound zones
;;; for which the depth of the children is are not imporant should
;;; include CHILD-DEPTH-INSIGNIFICANT-MIXIN in its superclasses. 
;;;
;;; This function is used mainly for painting zones.  For that reason,
;;; it maps over ALL children, i.e. including hidden children (also
;;; called cuckoos).  

(defgeneric clim3-ext:map-over-children-bottom-to-top (function zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-DEPTH-CHANGED.
;;;
;;; Whenever the depth of a zone Z is modified by client code, This
;;; function is called with Z and the parent of Z as arguments, even
;;; when the parent of Z is NIL.  

(defgeneric clim3-ext:notify-child-depth-changed (child parent))

(defmethod clim3-ext:notify-child-depth-changed ((child clim3:zone) (parent null))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DEPTH-MIXIN. 
;;;
;;; This class is mixed into the class STANDARD-ZONE.  It supplies a
;;; slot that holds the depth of the zone.  It also supplies an :AFTER
;;; method on (SETF DEPTH) that triggers the depth-notification
;;; protocol.

(defclass clim3-ext:depth-mixin ()
  ((%depth :initform 0
	   :initarg :depth
	   :accessor clim3:depth
	   :writer clim3-ext:set-depth)))

(defmethod (setf clim3:depth) :after
    (new-depth (zone clim3-ext:depth-mixin))
  (clim3-ext:notify-child-depth-changed zone (clim3-ext:parent zone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class CHILD-DEPTH-INSIGNIFICANT-MIXIN.
;;;
;;; This class should be mixed into any compound zone for which the
;;; order of the children is of no importance.  This is the case for
;;; layout zones whose children do not overlap, such as the HBOX and
;;; VBOX zones, and layout zones with at most one child. 
;;;
;;; When an instance of this zone receives a notification that the
;;; depth of some child has changed, it ignores the notification. 
;;;
;;; Furthermore, when MAP-OVER-CHILDREN-BOTTOM-TO-TOP or
;;; MAP-OVER-CHILDREN-TOP-TO-BOTTOM is called with an instance of this
;;; zone, the call trampolines to MAP-OVER-CHILDREN.

(defclass clim3-ext:child-depth-insignificant-mixin () ())

(defmethod clim3-ext:notify-child-depth-changed
    ((child clim3:zone) (parent clim3-ext:child-depth-insignificant-mixin))
  nil)

(defmethod clim3-ext:map-over-children-top-to-bottom
    (function (zone clim3-ext:child-depth-insignificant-mixin))
  (clim3-ext:map-over-children function zone))

(defmethod clim3-ext:map-over-children-bottom-to-top
    (function (zone clim3-ext:child-depth-insignificant-mixin))
  (clim3-ext:map-over-children function zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class CHILD-DEPTH-SIGNIFICANT-MIXIN.
;;;
;;; This class should be mixed into any compound zone for which the
;;; order of the children is important.  This is the case for zones
;;; such as the BBOARD zone and the PILE zone. 
;;;
;;; It supplies a slot containing a vector of the children, ordered by
;;; depth.
;;;
;;; When an instance of this zone receives notification that the depth
;;; of some child has changed, then it sets the slot to NIL,
;;; indicating that it no longer has a valid vector of depth-ordered
;;; children.
;;;
;;; By factoring out this mixin class, we can safely add an :AFTER
;;; method on (SETF CHILDREN) specialized to it that invalidates the
;;; depth-ordered children whenever there is any change in the
;;; children of the zone.  Next time one of
;;; MAP-OVER-CHILDREN-BOTTOM-TO-TOP or MAP-OVER-CHILDREN-TOP-TO-BOTTOM
;;; is called, the vector is recomputed. 

(defclass clim3-ext:child-depth-significant-mixin ()
  ((%depth-ordered-children :initform nil :accessor depth-ordered-children)))

(defmethod clim3-ext:notify-child-depth-changed
    ((child clim3:zone) (parent clim3-ext:child-depth-significant-mixin))
  (setf (depth-ordered-children parent) nil))

(defmethod (setf clim3:children) :after
  (new-children (parent clim3-ext:child-depth-significant-mixin))
  (declare (ignore new-children))
  (setf (depth-ordered-children parent) nil))

;;; This function is used by MAP-OVER-CHILDREN-TOP-TO-BOTTOM and by
;;; MAP-OVER-CHILDREN-BOTTOM-TO-TOP to make sure that the the slot
;;; containing the children order by depth is up to date.  Because
;;; these two client functions traverse ALL children, including hidden
;;; children (aslo called cuckoos), this function calls
;;; MAP-OVER-ALL-CHILDREN rather than MAP-OVER-CHILDREN.
(defun ensure-depth-ordered-children (zone)
  (when (null (depth-ordered-children zone))
    (let ((children '()))
      (clim3-ext:map-over-children (lambda (child) (push child children))
				   zone)
      (setf (depth-ordered-children zone)
	    (sort (coerce children 'vector) #'< :key #'clim3:depth)))))

(defmethod clim3-ext:map-over-children-top-to-bottom :before
    (function (zone clim3-ext:child-depth-significant-mixin))
  (ensure-depth-ordered-children zone))

(defmethod clim3-ext:map-over-children-bottom-to-top :before
    (function (zone clim3-ext:child-depth-significant-mixin))
  (ensure-depth-ordered-children zone))

(defmethod clim3-ext:map-over-children-top-to-bottom
    (function (zone clim3-ext:child-depth-significant-mixin))
  (let ((depth-ordered-children (depth-ordered-children zone)))
    (loop for i from 0 below (length depth-ordered-children)
	  do (funcall function (aref depth-ordered-children i)))))

(defmethod clim3-ext:map-over-children-bottom-to-top
    (function (zone clim3-ext:child-depth-significant-mixin))
  (let ((depth-ordered-children (depth-ordered-children zone)))
    (loop for i downfrom (1- (length depth-ordered-children)) to 0
	  do (funcall function (aref depth-ordered-children i)))))
