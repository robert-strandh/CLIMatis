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
			 parent-mixin
			 position-mixin
			 size-mixin
			 depth-mixin
			 sprawls-mixin
			 client-mixin)
  ())

(defun clim3:zone-p (object)
  (typep object 'clim3:zone))

(defgeneric clim3-ext:print-components (zone stream)
  (:method-combination progn :most-specific-last))

;; (defmethod print-object ((object zone) stream)
;;   (print-unreadable-object (object stream :type t :identity t)
;;     (print-components object stream)))

(defmethod clim3-ext:print-components progn ((zone clim3:zone) stream)
  nil)

(defmethod clim3-ext:print-components progn ((zone clim3:standard-zone) stream)
  (format stream
	  "hp: ~a vp: ~a w: ~a h: ~a d: ~a "
	  (clim3:hpos zone)
	  (clim3:vpos zone)
	  (clim3:width zone)
	  (clim3:height zone)
	  (clim3:depth zone)))
