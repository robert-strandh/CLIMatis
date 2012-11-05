(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function HSPRAWL.

(defgeneric hsprawl (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF HSPRAWL).

(defgeneric (setf hsprawl) (new-hsprawl zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-HSPRAWL.

(defgeneric set-hsprawl (new-hsprawl zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VSPRAWL.

(defgeneric vsprawl (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF VSPRAWL).

(defgeneric (setf vsprawl) (new-vsprawl zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-VSPRAWL.

(defgeneric set-vsprawl (new-vsprawl zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPUTE-HSPRAWL.
;;;
;;; This function is repsonsible for computing and setting the
;;; horizontal sprawls of a zone.  It is only called on a zone with
;;; invalid horizontal sprawls.
;;; 
;;; After a call to this function, the horizontal sprawls of the zone
;;; are valid.

(defgeneric compute-hsprawl (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPUTE-VSPRAWL.
;;;
;;; This function is repsonsible for computing and setting the
;;; vertical sprawls of a zone.  It is only called on a zone with
;;; invalid vertical sprawls.
;;;
;;; After a call to this function, the vertical sprawls of the zone
;;; are valid.

(defgeneric compute-vsprawl (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-HSPRAWL-VALID.
;;;
;;; This function checks whether the horizontal sprawl of the zone is
;;; valid, and if so does nothing.  Otherwise, it calls
;;; COMPUTE-HSPRAWL.

(defun ensure-hsprawl-valid (zone)
  (when (null (hsprawl zone))
    (compute-hsprawl zone)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-HSPRAWL-CHANGED.

(defgeneric notify-child-hsprawl-changed (child parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-VSPRAWL-CHANGED.

(defgeneric notify-child-vsprawl-changed (child parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-VSPRAWL-VALID.
;;;
;;; This function checks whether the vertical sprawl of the zone is
;;; valid, and if so does nothing.  Otherwise, it calls
;;; COMPUTE-VSPRAWL.

(defun ensure-vsprawl-valid (zone)
  (when (null (vsprawl zone))
    (compute-vsprawl zone)))

(defclass sprawls-mixin ()
  ((%hsprawl :initform nil :initarg :hsprawl :accessor hsprawl :writer set-hsprawl)
   (%vsprawl :initform nil :initarg :vsprawl :accessor vsprawl :writer set-vsprawl)))

;;; After the hsprawl of a zone has been explicitly modified, we
;;; notify the parent.
(defmethod (setf hsprawl) :after (new-hsprawl (zone zone))
  (notify-child-hsprawl-changed zone (parent zone)))

;;; After the vsprawl of a zone has been explicitly modified, we
;;; notify the parent.
(defmethod (setf vsprawl) :after (new-vsprawl (zone zone))
  (notify-child-vsprawl-changed zone (parent zone)))

;;; Return as two values the natural width and the natural height of
;;; the zone.  We use this function to determine the size of a zone
;;; where the dimensions do not depend on the parent, such as a bboard
;;; zone or a scroller zone.
(defun natural-size (zone)
  (values (clim3-sprawl:size (hsprawl zone))
	  (clim3-sprawl:size (vsprawl zone))))

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

(defmethod (setf children) :after
  (new-children (zone changing-children-changes-hsprawl-mixin))
  (declare (ignore new-children))
  (setf (hsprawl zone) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-VSPRAWL-MIXIN

(defclass changing-children-changes-vsprawl-mixin () ())

(defmethod (setf children) :after
  (new-children (zone changing-children-changes-vsprawl-mixin))
  (declare (ignore new-children))
  (setf (vsprawl zone) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-BOTH-SPRAWLS-MIXIN

(defclass changing-children-changes-both-sprawls-mixin () ())

(defmethod (setf children) :after
  (new-children (zone changing-children-changes-vsprawl-mixin))
  (declare (ignore new-children))
  (setf (hsprawl zone) nil)
  (setf (vsprawl zone) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-NOTHING-MIXIN

(defclass changing-children-changes-nothing-mixin () ())

;;; Default method on NOTIFY-CHILD-HSPRAWL-CHANGED for ZONE and NULL.
;;; This method does nothing, thus allowing this generic function to
;;; be called with any zone and its parent.
(defmethod notify-child-hsprawl-changed ((child zone) (parent null))
  nil)

;;; Default method on NOTIFY-CHILD-VSPRAWL-CHANGED for ZONE and NULL.
;;; This method does nothing, thus allowing this generic function to
;;; be called with any zone and its parent.
(defmethod notify-child-vsprawl-changed ((child zone) (parent null))
  nil)

