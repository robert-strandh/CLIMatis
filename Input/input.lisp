(in-package #:clim3-input)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INPUT.
;;;
;;; An input zone is very elastic.  Client code will stick the input
;;; zone in a compound zone on top of some more rigid zones. 

(defclass clim3:input (clim3:standard-zone
		 clim3-ext:atomic-mixin)
  ()
  (:default-initargs :vsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :hsprawl (clim3-sprawl:sprawl 0 0 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VISIT
;;;
;;; The handlers are called with the following arguments:
;;;
;;;   the zone

(defgeneric clim3:enter (zone)
  (:method-combination progn))

(defmethod clim3:enter progn (zone)
  (declare (ignore zone))
  nil)

(defgeneric clim3:leave (zone)
  (:method-combination progn))

(defmethod clim3:leave progn (zone)
  (declare (ignore zone))
  nil)

(defclass clim3:visit ()
  ((%inside-p :initarg :inside-p :reader clim3:inside-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MOTION.
;;;
;;; The handler is called with the following arguments:
;;;
;;;   the zone
;;;   hpos
;;;   vpos

(defclass clim3:motion (clim3:input)
  ((%handler :initarg :handler :reader clim3:handler)))

(defun clim3:motion (handler)
  (make-instance 'clim3:motion :handler handler))

