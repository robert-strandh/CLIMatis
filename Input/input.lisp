(in-package #:clim3-input)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INPUT.
;;;
;;; An input zone is very elastic.  Client code will stick the input
;;; zone in a compound zone on top of some more rigid zones. 

(defclass input (clim3:standard-zone
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

(defclass visit (input)
  ((%enter-handler :initarg :enter-handler :reader enter-handler)
   (%leave-handler :initarg :leave-handler :reader leave-handler)
   (%inside-p :initarg :inside-p :reader inside-p)))

(defun visit (enter-handler leave-handler &optional (inside-p (constantly t)))
  (make-instance 'visit
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

(defclass motion (input)
  ((%handler :initarg :handler :reader handler)))

(defun motion (handler)
  (make-instance 'motion :handler handler))

