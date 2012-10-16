(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function GIVES-VALID-P.
;;;
;;; This function returns true if and only if the gives of the zone
;;; are valid.  

(defgeneric gives-valid-p (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MARK-GIVES-INVALID.
;;;
;;; This function marks the zone as having invalid gives. 
;;;
;;; Different subclasses of ZONE do this differently.  Some set both
;;; the hgive and the vgive to NIL.  Some others set one but not the
;;; other.  Some may use a completely different mechanism.  

(defgeneric mark-gives-invalid (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-GIVES-INVALID.
;;;
;;; This function is called on a child and a parent when the gives of
;;; the child are invalidated for some reason (see the function
;;; INVALIDATE GIVES).  If the gives of the parent zone depend on the
;;; gives of its children, then this call must provoke a call to
;;; INVALIDATE-GIVES with the parent as an argument. 
;;;
;;; Two default methods are supplied.  The first default method is
;;; specialized on a NULL parent and it does nothing.  This way, a
;;; zone and its parent are always valid arguments to this function,
;;; even though the zone is disconnected.  The second default method
;;; is specialized on a parent of type ZONE and it signals an error.
;;; By doing it this way, we compel zones to make an explicit choice
;;; concerning the action of this function.

(defgeneric notify-child-gives-invalid (child parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-GIVES-CHANGED.
;;;
;;; This function is similar to NOTIFY-CHILD-GIVES-INVALID, except
;;; that it is called when a child has its gives altered from one set
;;; of valid gives to another set of valid gives.  Although we do not
;;; exploit this possibility, we could have the parent do something
;;; smarter than just invalidating its gives.  For the predefined
;;; classed, we do the same thing as NOTIFY-CHILD-GIVES-INVALID.
;;; 
;;; This function is called on a child and a parent when the gives of
;;; the child are altered for some reason, typically because of an
;;; explicit assignment to the gives of the child i.e, a call either
;;; to (SETF VGIVE) or to (SETF HGIVE).  Methods specialized on
;;; parents with gives that depend on the gives of its children, can
;;; choose to recompute the gives of the parent (provided the parent
;;; gives are still valid) and make a recursive call to the
;;; grandparent, or it can choose to invalidate the gives of the
;;; parent.
;;;
;;; Two default methods are supplied.  The first default method is
;;; specialized on a NULL parent and it does nothing.  This way, a
;;; zone and its parent are always valid arguments to this function,
;;; even though the zone is disconnected.  The second default method
;;; is specialized on a parent of type ZONE and it signals an error.
;;; By doing it this way, we compel zones to make an explicit choice
;;; concerning the action of this function.

(defgeneric notify-child-gives-changed (child parent))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INVALIDATE-GIVES.
;;;
;;; This function is used to indicate that the gives of some zone are
;;; no longer valid.  The reason for that could be that the gives
;;; depend on the contents and the contents changed, or that the gives
;;; of the zone depend on it being connected to some client, and it
;;; got disconnected.
;;;
;;; If the gives of the zone are already invalid, i.e., if
;;; GIVES-VALID-P returns false, then this function does nothing.
;;; Otherwise, it marks them as invalid and calls the function
;;; NOTIFY-CHILD-GIVES-INVALID with the zone and its parent.

(defun invalidate-gives (zone)
  (when (gives-valid-p zone)
    (mark-gives-invalid zone)
    (notify-child-gives-invalid zone (parent zone))))

