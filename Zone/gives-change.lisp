(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-SPRAWLS-CHANGED.
;;;
;;; This function is called when a child has its sprawls modified.
;;; Although we do not exploit this possibility, we could have the
;;; parent do something smarter than just invalidating its sprawls.
;;; For the predefined classed, we do the same thing as
;;; NOTIFY-CHILD-SPRAWLS-INVALID.
;;; 
;;; This function is called on a child and a parent when the sprawls of
;;; the child are altered for some reason, typically because of an
;;; explicit assignment to the sprawls of the child i.e, a call either
;;; to (SETF VSPRAWL) or to (SETF HSPRAWL).  Methods specialized on
;;; parents with sprawls that depend on the sprawls of its children, can
;;; choose to recompute the sprawls of the parent (provided the parent
;;; sprawls are still valid) and make a recursive call to the
;;; grandparent, or it can choose to invalidate the sprawls of the
;;; parent.
;;;
;;; Two default methods are supplied.  The first default method is
;;; specialized on a NULL parent and it does nothing.  This way, a
;;; zone and its parent are always valid arguments to this function,
;;; even though the zone is disconnected.  The second default method
;;; is specialized on a parent of type ZONE and it signals an error.
;;; By doing it this way, we compel zones to make an explicit choice
;;; concerning the action of this function.

(defgeneric notify-child-sprawls-changed (child parent))
