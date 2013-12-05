(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-ZONE.
;;;
;;; The position of a zone is in the coordinate system of its parent.
;;; 
;;; Client code can set the position and the dimensions of a zone, but
;;; if the zone is the child of a layout zone, then the layout
;;; protocol may modify those parameters again, so that the
;;; modification by the client will have no effect.  Some layout zones
;;; like the bboard zone do not impose any position or dimensions on
;;; the children.  Setting the position and the dimensions of a child
;;; of such a layout zone will move and resize the child. 

(defclass clim3:standard-zone (clim3:zone
			       clim3-ext:parent-mixin
			       clim3-ext:position-mixin
			       clim3-ext:size-mixin
			       clim3-ext:depth-mixin
			       clim3-ext:sprawls-mixin
			       clim3-ext:client-mixin)
  ())

(defun clim3:zone-p (object)
  (typep object 'clim3:zone))
