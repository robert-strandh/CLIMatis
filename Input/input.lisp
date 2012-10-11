;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input zone.
;;;
;;; An input zone is very elastic.  Client code will stick the input
;;; zone in a compound zone on top of some more rigid zones. 

(defclass input-zone (zone)
  ((%event-mask :initarg :event-mask
		:initform '()
		:accessor event-mask))
  (:default-initargs :vgive (rigidity:little-rigid)
		     :hgive (rigidity:little-rigid)))

(defmethod map-over-children (function (zone input-zone))
  (declare (ignore function))
  nil)

