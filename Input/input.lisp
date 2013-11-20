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
;;; This class is not a subclass of zone.  Rather, it should be used
;;; as a mixin superclass of a custom zone that should detect when the
;;; pointer enters or leaves the zone.
;;;
;;; When the pointer enters the zone, the generic function ENTER is
;;; called with the zone as an argument.  When the pointer leaves the
;;; zone, the generic function LEAVE is called with the zone as an
;;; argument.

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
;;; Class CLICKABLE.

(defgeneric clim3:button-press (zone button)
  (:method-combination progn))

(defmethod clim3:button-press progn (zone button)
  (declare (ignore zone button))
  nil)

(defgeneric clim3:button-release (zone button)
  (:method-combination progn))

(defmethod clim3:button-release progn (zone button)
  (declare (ignore zone button))
  nil)

(defclass clim3:zone-button-handler (clim3:button-handler)
  ((%zone :initarg :zone :reader zone)))

(defmethod clim3:handle-button-press
    ((button-handler clim3:zone-button-handler) code modifiers)
  (clim3:button-press (zone button-handler)
		      (clim3:standard-button-processor code modifiers)))

(defmethod clim3:handle-button-release
    ((button-handler clim3:zone-button-handler) code modifiers)
  (clim3:button-release (zone button-handler)
			(clim3:standard-button-processor code modifiers)))

(defclass clim3:clickable ()
  ((%previous :initform nil :accessor previous)))

(defgeneric clim3:attention (zone))

(defmethod clim3:attention ((zone clim3:clickable))
  (setf (previous zone) clim3:*button-handler*)
  (setf clim3:*button-handler*
	(make-instance 'clim3:zone-button-handler :zone zone)))

(defgeneric clim3:at-ease (zone))

(defmethod clim3:at-ease ((zone clim3:clickable))
  (setf clim3:*button-handler*
	(previous zone)))

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

