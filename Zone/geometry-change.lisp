(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-GEOMETRY-CHANGED.
;;;
;;; This generic function is called whenever the position or size of
;;; the zone is explicitly changed by a call to (SETF HPOS), (SETF
;;; VPOS), (SETF WIDTH), or (SETF HEIGHT).
;;;
;;; Two default methods are supplied.  The first default method is
;;; specialized on a NULL parent and it does nothing.  This way, a
;;; zone and its parent are always valid arguments to this function,
;;; even though the zone is disconnected.  The second default method
;;; is specialized on a parent of type ZONE and it signals an error.
;;; By doing it this way, we compel zones to make an explicit choice
;;; concerning the action of this function.

(defgeneric notify-child-geometry-changed (child parent))

