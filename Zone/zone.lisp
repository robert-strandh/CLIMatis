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

(defclass standard-zone (zone
			 parent-mixin
			 position-mixin
			 size-mixin
			 depth-mixin
			 sprawls-mixin
			 client-mixin)
  ())

(defun zone-p (object)
  (typep object 'zone))

(defgeneric print-components (zone stream)
  (:method-combination progn :most-specific-last))

;; (defmethod print-object ((object zone) stream)
;;   (print-unreadable-object (object stream :type t :identity t)
;;     (print-components object stream)))

(defmethod print-components progn ((zone zone) stream)
  nil)

(defmethod print-components progn ((zone standard-zone) stream)
  (format stream
	  "hp: ~a vp: ~a w: ~a h: ~a d: ~a "
	  (hpos zone) (vpos zone) (width zone) (height zone) (depth zone)))
