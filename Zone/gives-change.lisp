(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SPRAWLS-VALID-P.
;;;
;;; This function returns true if and only if the sprawls of the zone
;;; are valid.  

(defgeneric sprawls-valid-p (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MARK-SPRAWLS-INVALID.
;;;
;;; This function marks the zone as having invalid sprawls. 
;;;
;;; Different subclasses of ZONE do this differently.  Some set both
;;; the hsprawl and the vsprawl to NIL.  Some others set one but not the
;;; other.  Some may use a completely different mechanism.  

(defgeneric mark-sprawls-invalid (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-SPRAWLS-INVALID.
;;;
;;; This function is called on a child and a parent when the sprawls of
;;; the child are invalidated for some reason (see the function
;;; INVALIDATE SPRAWLS).  If the sprawls of the parent zone depend on the
;;; sprawls of its children, then this call must provoke a call to
;;; INVALIDATE-SPRAWLS with the parent as an argument. 
;;;
;;; Two default methods are supplied.  The first default method is
;;; specialized on a NULL parent and it does nothing.  This way, a
;;; zone and its parent are always valid arguments to this function,
;;; even though the zone is disconnected.  The second default method
;;; is specialized on a parent of type ZONE and it signals an error.
;;; By doing it this way, we compel zones to make an explicit choice
;;; concerning the action of this function.

(defgeneric notify-child-sprawls-invalid (child parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-SPRAWLS-CHANGED.
;;;
;;; This function is similar to NOTIFY-CHILD-SPRAWLS-INVALID, except
;;; that it is called when a child has its sprawls altered from one set
;;; of valid sprawls to another set of valid sprawls.  Although we do not
;;; exploit this possibility, we could have the parent do something
;;; smarter than just invalidating its sprawls.  For the predefined
;;; classed, we do the same thing as NOTIFY-CHILD-SPRAWLS-INVALID.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INVALIDATE-SPRAWLS.
;;;
;;; This function is used to indicate that the sprawls of some zone are
;;; no longer valid.  The reason for that could be that the sprawls
;;; depend on the contents and the contents changed, or that the sprawls
;;; of the zone depend on it being connected to some client, and it
;;; got disconnected.
;;;
;;; If the sprawls of the zone are already invalid, i.e., if
;;; SPRAWLS-VALID-P returns false, then this function does nothing.
;;; Otherwise, it marks them as invalid and calls the function
;;; NOTIFY-CHILD-SPRAWLS-INVALID with the zone and its parent.

(defun invalidate-sprawls (zone)
  (when (sprawls-valid-p zone)
    (mark-sprawls-invalid zone)
    (notify-child-sprawls-invalid zone (parent zone))))

