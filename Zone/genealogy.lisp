(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function PARENT.
;;;
;;; Return the current parent of the zone. 
;;;
;;; The parent may be another zone, in which case the zone is a child
;;; of that zone, or it may be a client (typically a port), in which
;;; case the zone is the root zone of a hierarchy connected to that
;;; client, or it may be nil, in which case, this zone is the root
;;; zone of a hierarchy not currently connected to any client.

(defgeneric parent (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF PARENT).
;;;
;;; Set the parent of a zone.
;;;
;;; This generic function is part of the internal zone protocols.  It
;;; should not be used directly by applications.  It is called
;;; indirectly as a result of connecting the zone to a client, or as a
;;; result of adding or removing the zone as a child of some other
;;; zone by calling (SETF CHILDREN). 

(defgeneric (setf parent) (new-parent zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CHILDREN.
;;;
;;; This generic function returns the children of a compound zone.
;;; The representation of the return value depends on the subclass of
;;; COMPOUND-ZONE.  It could be a list, a vector, a 2-D array, or
;;; something else.  

(defgeneric children (compound-zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF CHILDREN)
;;;
;;; This generic function sets the children of a compound zone.  The
;;; acceptable representation of the children depends on the subclass
;;; of COMPOUND-ZONE.  It could be a list, a vector, a 2-D array, or
;;; something else.  However, the representation is consistent with
;;; what is returned by the CHILDREN generic function, so that if a
;;; list is returned by that generic function, then a list is
;;; acceptable to this one too.  In particular, it is acceptable to
;;; call this function with the exact same value as was return by a
;;; call to CHILDREN.  
;;;
;;; There are :around methods on this function (see below).  One is
;;; specialized for AT-MOST-ONE-CHILD-MIXIN, and the other for
;;; ANY-NUMBER-OF-CHILDREN-MIXIN, both subclasses of COMPOUND-ZONE.
;;; These :around methods always call the primary methods, but they
;;; also do some extra work such as error checking, setting the parent
;;; of every new child, removing the parent of every removed child,
;;; and client notification.  The :around method calls CHILDREN, which
;;; has as a consequence that in order to use (SETF CHILDREN) the
;;; corresponding slot must be bound.
;;;
;;; There is one :after method specialized for COMPOUND-ZONE that calls 

(defgeneric (setf children) (new-children compound-zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAP-OVER-CHILDREN.
;;;
;;; The first argument is a function of a single argument.  The second
;;; argument is a zone.  This function calls the function given as the
;;; first argument on each child of the zone given as a second argument.  
;;;

(defgeneric map-over-children (function zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAP-OVER-CHILDREN-TOP-TO-BOTTOM.
;;;
;;; The first argument is a function of a single argument.  The second
;;; argument is a zone.  This function calls the function given as the
;;; first argument on each child of the zone given as a second argument.  

(defgeneric map-over-children-top-to-bottom (function zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAP-OVER-CHILDREN-BOTTOM-TO-TOP.
;;;
;;; The first argument is a function of a single argument.  The second
;;; argument is a zone.  This function calls the function given as the
;;; first argument on each child of the zone given as a second argument.  

(defgeneric map-over-children-bottom-to-top (function zone))

