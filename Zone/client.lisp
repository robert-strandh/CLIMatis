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

(defgeneric clim3-ext:client (zone))

(defgeneric (setf clim3-ext:client) (new-client zone))

(defclass clim3-ext:client-mixin ()
  ((%client :initform nil :accessor clim3-ext:client)))

(defun clim3-ext:set-clients (zone client)
  (labels ((aux (zone)
	     (unless (eq (clim3-ext:client zone) client)
	       (setf (clim3-ext:client zone) client)
	       (clim3-ext:map-over-children #'aux zone))))
    (aux zone)))

(defmethod (setf clim3-ext:parent) :after
    ((new-parent clim3:zone) (zone clim3-ext:client-mixin))
  (clim3-ext:set-clients zone (clim3-ext:client new-parent)))



