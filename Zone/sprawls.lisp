(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function HSPRAWL.

(defgeneric clim3:hsprawl (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF HSPRAWL).

(defgeneric (setf clim3:hsprawl) (new-hsprawl zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-HSPRAWL.

(defgeneric clim3-ext:set-hsprawl (new-hsprawl zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VSPRAWL.

(defgeneric clim3:vsprawl (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF VSPRAWL).

(defgeneric (setf clim3:vsprawl) (new-vsprawl zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-VSPRAWL.

(defgeneric clim3-ext:set-vsprawl (new-vsprawl zone))

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

(defgeneric clim3-ext:compute-hsprawl (zone))

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

(defgeneric clim3-ext:compute-vsprawl (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-HSPRAWL-VALID.
;;;
;;; This function checks whether the horizontal sprawl of the zone is
;;; valid, and if so does nothing.  Otherwise, it calls
;;; COMPUTE-HSPRAWL.

(defun clim3-ext:ensure-hsprawl-valid (zone)
  (when (null (clim3:hsprawl zone))
    (clim3-ext:compute-hsprawl zone)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-HSPRAWL-CHANGED.

(defgeneric clim3-ext:notify-child-hsprawl-changed (child parent))

;;; Default method on NOTIFY-CHILD-HSPRAWL-CHANGED for ZONE and NULL.
;;; This method does nothing, thus allowing this generic function to
;;; be called with any zone and its parent.
(defmethod clim3-ext:notify-child-hsprawl-changed
    ((child clim3:zone) (parent null))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-VSPRAWL-CHANGED.

(defgeneric clim3-ext:notify-child-vsprawl-changed (child parent))

;;; Default method on NOTIFY-CHILD-VSPRAWL-CHANGED for ZONE and NULL.
;;; This method does nothing, thus allowing this generic function to
;;; be called with any zone and its parent.
(defmethod clim3-ext:notify-child-vsprawl-changed
    ((child clim3:zone) (parent null))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-VSPRAWL-VALID.
;;;
;;; This function checks whether the vertical sprawl of the zone is
;;; valid, and if so does nothing.  Otherwise, it calls
;;; COMPUTE-VSPRAWL.

(defun clim3-ext:ensure-vsprawl-valid (zone)
  (when (null (clim3:vsprawl zone))
    (clim3-ext:compute-vsprawl zone)))

(defclass sprawls-mixin ()
  ((%hsprawl :initform nil
	     :initarg :hsprawl
	     :accessor clim3:hsprawl
	     :writer clim3-ext:set-hsprawl)
   (%vsprawl :initform nil
	     :initarg :vsprawl
	     :accessor clim3:vsprawl
	     :writer clim3-ext:set-vsprawl)))

;;; After the hsprawl of a zone has been explicitly modified, we
;;; notify the parent.
(defmethod (setf clim3:hsprawl) :after (new-hsprawl (zone clim3:zone))
  (clim3-ext:notify-child-hsprawl-changed zone (clim3-ext:parent zone)))

;;; After the vsprawl of a zone has been explicitly modified, we
;;; notify the parent.
(defmethod (setf clim3:vsprawl) :after (new-vsprawl (zone clim3:zone))
  (clim3-ext:notify-child-vsprawl-changed zone (clim3-ext:parent zone)))

;;; Return as two values the natural width and the natural height of
;;; the zone.  We use this function to determine the size of a zone
;;; where the dimensions do not depend on the parent, such as a bboard
;;; zone or a scroller zone.
(defun clim3:natural-size (zone)
  (values (clim3-sprawl:size (clim3:hsprawl zone))
	  (clim3-sprawl:size (clim3:vsprawl zone))))

