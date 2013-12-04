(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities
;;;

(defun hash-table-from-children (zone)
  (let ((table (make-hash-table :test #'eq)))
    (clim3-ext:map-over-children
     (lambda (child)
       (setf (gethash child table) t))
     zone)
    table))

(defun map-over-hash-table-difference (function table1 table2)
  (maphash (lambda (key value)
	     (declare (ignore value))
	     (unless (gethash key table2)
	       (funcall function key)))
	   table1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CHILDREN.
;;;
;;; This generic function returns the children of a compound zone.
;;; The representation of the return value depends on the subclass of
;;; COMPOUND-ZONE.  It could be a list, a vector, a 2-D array, or
;;; something else.  

(defgeneric clim3:children (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF CHILDREN)
;;;
;;; This generic function sets the children of a compound zone.  The
;;; acceptable representation of the children depends on the subclass
;;; of SEVERAL-CHILDREN-MIXIN.  It could be a list, a vector, a 2-D
;;; array, or something else.  However, the representation is
;;; consistent with what is returned by the CHILDREN generic function,
;;; so that if a list is returned by that generic function, then a
;;; list is acceptable to this one too.  In particular, it is
;;; acceptable to call this function with the exact same value as was
;;; return by a call to CHILDREN.
;;;
;;; There are :around methods on this function (see below).  One is
;;; specialized for AT-MOST-ONE-CHILD-MIXIN, and the other for
;;; SEVERAL-CHILDREN-MIXIN, both subclasses of COMPOUND-ZONE.  These
;;; :around methods always call the primary methods, but they also do
;;; some extra work such as error checking, setting the parent of
;;; every new child, removing the parent of every removed child, and
;;; client notification.  The :around method calls CHILDREN, which has
;;; as a consequence that in order to use (SETF CHILDREN) the
;;; corresponding slot must be bound.

(defgeneric (setf clim3:children) (new-children compound-zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CHILD.

(defgeneric clim3:child (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF CHILD).

(defgeneric (setf clim3:child) (new-child zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ALL-CHILDREN.
;;;
;;; Some zones may be constructed by having "hidden children", also
;;; called "cuckoos".  This cuckoos are used to implement the behavior
;;; of the zone by adding specific zones to it that may not be removed
;;; or replaced by application code.  The generic functions CHILDREN
;;; and (SETF CHILDREN) do not "see" these cuckoos, so that the
;;; behavior of these zones conforms to the specification no matter
;;; how application code alters the "real" children.
;;;
;;; When the zone hierarchy is being traversed in order to compute
;;; sprawls or to paint some graphics, then the cuckoos are included,
;;; and this function is used.
;;;
;;; Since not many zones include cuckoos, we define a default method
;;; on this generic function that simply calls CHILDREN.

(defgeneric clim3-ext:all-children (zone))

(defmethod clim3-ext:all-children (zone)
  (clim3:children zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAP-OVER-CHILDREN.
;;;
;;; The first argument is a function of a single argument.  The second
;;; argument is a zone.  This function calls the function given as the
;;; first argument on each child of the zone given as a second argument.  
;;;

(defgeneric clim3-ext:map-over-children (function zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAP-OVER-ALL-CHILDREN.
;;;
;;; The first argument is a function of a single argument.  The second
;;; argument is a zone.  This function calls the function given as the
;;; first argument on each child of the zone given as a second argument.  
;;;
;;; The difference between this function and the function
;;; MAP-OVER-CHILDREN is that this function also takes into account
;;; hidden children (also called cuckoos).  
;;;
;;; The default method just calls MAP-OVER-CHILDREN.  Zones that have
;;; cuckoos should override this method and map over all children
;;; including the cuckoos.

(defgeneric clim3-ext:map-over-all-children (function zone))

(defmethod clim3-ext:map-over-all-children (function zone)
  (clim3-ext:map-over-children function zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ATOMIC-MIXIN
;;;

(defclass clim3-ext:atomic-mixin (clim3-ext:child-depth-insignificant-mixin) ())

(defmethod clim3:children ((zone clim3-ext:atomic-mixin))
  '())

(defmethod clim3:child ((zone clim3-ext:atomic-mixin))
  nil)

(defmethod (setf clim3:children) (new-children (zone clim3-ext:atomic-mixin))
  (declare (ignore new-children))
  (error "can't set the children of an atomic zone"))

(defmethod clim3-ext:map-over-children (function (zone clim3-ext:atomic-mixin))
  (declare (ignore function))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPOUND-MIXIN.

(defclass clim3-ext:compound-mixin ()
  ((%children :initform '() :initarg :children :accessor clim3:children)))

(defmethod initialize-instance :after ((zone clim3-ext:compound-mixin) &key)
  (clim3-ext:map-over-children (lambda (child) (setf (clim3-ext:parent child) zone)) zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AT-MOST-ONE-CHILD-MIXIN.
;;;

(defclass clim3-ext:at-most-one-child-mixin (clim3-ext:compound-mixin) ())

(defmethod (setf clim3:children) :around (new-child (zone clim3-ext:at-most-one-child-mixin))
  (declare (ignore new-child))
  (let ((child-before (clim3:children zone)))
    (call-next-method)
    (let ((child-after (clim3:children zone)))
      (unless (eq child-before child-after)
	(unless (null child-before)
	  (setf (clim3-ext:parent child-before) nil))
	(unless (null child-after)
	  (setf (clim3-ext:parent child-after) zone))))))

(defmethod (setf clim3:children) :before (new-child (zone clim3-ext:at-most-one-child-mixin))
  (unless (or (null new-child) (clim3:zone-p new-child))
    (error "new child must be a zone or NIL")))

(defmethod clim3-ext:map-over-children (function (zone clim3-ext:at-most-one-child-mixin))
  (let ((child (clim3:children zone)))
    (unless (null child)
      (funcall function child))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SEVERAL-CHILDREN-MIXIN.
;;;

(defclass clim3-ext:several-children-mixin (clim3-ext:compound-mixin) ())

(defmethod (setf clim3:children) :around (new-children (zone clim3-ext:several-children-mixin))
  (declare (ignore new-children))
  (let ((children-before (hash-table-from-children zone)))
    (call-next-method)
    (let ((children-after (hash-table-from-children zone)))
      (map-over-hash-table-difference
       (lambda (deleted-child)
	 (setf (clim3-ext:parent deleted-child) nil))
       children-before children-after)
      (map-over-hash-table-difference
       (lambda (inserted-child)
	 (setf (clim3-ext:parent inserted-child) zone))
       children-after children-before))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LIST-CHILDREN-MIXIN.
;;;

(defclass clim3-ext:list-children-mixin (clim3-ext:several-children-mixin) ())

(defmethod clim3-ext:map-over-children (function (zone clim3-ext:list-children-mixin))
  (mapc function (clim3:children zone)))

(defmethod (setf clim3:children) :before (new-children (zone clim3-ext:list-children-mixin))
  ;; FIXME: check that new-children is a proper list.
  (loop for child in new-children
	do (unless (clim3:zone-p child)
	     (error "new child must be a zone"))
	   (unless (null (clim3-ext:parent child))
	     (error "attempt to connect a zone that is already connected"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VECTOR-CHILDREN-MIXIN.
;;;

(defclass clim3-ext:vector-children-mixin (clim3-ext:several-children-mixin) ())

(defmethod clim3-ext:map-over-children (function (zone clim3-ext:vector-children-mixin))
  (map nil function (clim3:children zone)))

(defmethod (setf clim3:children) :before (new-children (zone clim3-ext:vector-children-mixin))
  (unless (vectorp new-children)
    (error "new children must be a vector"))
  (loop for child across new-children
	do (unless (clim3:zone-p child)
	     (error "new child must be a zone"))
	   (unless (null (clim3-ext:parent child))
	     (error "attempt to connect a zone that is already connected"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MATRIX-CHILDREN-MIXIN.
;;;

(defclass clim3-ext:matrix-children-mixin (clim3-ext:several-children-mixin) ())

(defmethod clim3-ext:map-over-children (function (zone clim3-ext:matrix-children-mixin))
  (let ((children (clim3:children zone)))
    (loop for r from 0 below (array-dimension children 0)
	  do (loop for c from 0 below (array-dimension children 1)
		   do (funcall function (aref children r c))))))

(defmethod (setf clim3:children) :before (new-children (zone clim3-ext:matrix-children-mixin))
  (unless (and (arrayp new-children)
	       (= (array-rank new-children) 2))
    (error "new children must be an array of rank 2"))
  (loop for r from 0 below (array-dimension new-children 0)
	do (loop for c from 0 below (array-dimension new-children 1)
		 do (let ((child (aref new-children r c)))
		      (unless (clim3:zone-p child)
			(error "new child must be a zone"))
		      (unless (null (clim3-ext:parent child))
			(error "attempt to connect a zone that is already connected"))))))

