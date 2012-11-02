(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Every zone has a client.  
;;; FIXME: say more
;;;
;;; A zone can belong to at most one client at a time.  The nature of
;;; this object is entirely determined by client code, but it is
;;; typically a port.
;;;
;;; By factoring out this class, we can safely add an :AFTER method to
;;; (SETF PARENT), specialized to this class.  

(defgeneric client (zone))

(defgeneric (setf client) (new-client zone))

(defclass client-mixin ()
  ((%client :initform nil :accessor client)))

(defun set-clients (zone client)
  (labels ((aux (zone)
	     (unless (eq (client zone) client)
	       (setf (client zone) client)
	       (map-over-children #'aux zone))))
    (aux zone)))

(defmethod (setf parent) :after ((new-parent zone) (zone client-mixin))
  (set-clients zone (client new-parent)))



