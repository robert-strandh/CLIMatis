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

(defclass clim3:visit (clim3:input)
  ((%enter-handler :initarg :enter-handler :reader clim3:enter-handler)
   (%leave-handler :initarg :leave-handler :reader clim3:leave-handler)
   (%inside-p :initarg :inside-p :reader clim3:inside-p)))

(defun clim3:visit (enter-handler leave-handler &optional (inside-p (constantly t)))
  (make-instance 'clim3:visit
		 :enter-handler enter-handler
		 :leave-handler leave-handler
		 :inside-p inside-p))

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

