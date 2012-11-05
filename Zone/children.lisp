(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities
;;;

(defun hash-table-from-children (zone)
  (let ((table (make-hash-table :test #'eq)))
    (map-over-children
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

(defgeneric children (zone))

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
;;; ANY-NUMBER-OF-CHILDREN-MIXIN, both subclasses of COMPOUND-ZONE.
;;; These :around methods always call the primary methods, but they
;;; also do some extra work such as error checking, setting the parent
;;; of every new child, removing the parent of every removed child,
;;; and client notification.  The :around method calls CHILDREN, which
;;; has as a consequence that in order to use (SETF CHILDREN) the
;;; corresponding slot must be bound.

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
;;; Class ATOMIC-MIXIN
;;;

(defclass atomic-mixin () ())

(defmethod children ((zone atomic-mixin))
  '())

(defmethod child ((zone atomic-mixin))
  nil)

(defmethod (setf children) (new-children (zone atomic-mixin))
  (declare (ignore new-children))
  (error "can't set the children of an atomic zone"))

(defmethod map-over-children (function (zone atomic-mixin))
  (declare (ignore function))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPOUND-MIXIN.

(defclass compound-mixin ()
  ((%children :initform '() :initarg :children :accessor children)))

(defmethod initialize-instance :after ((zone compound-mixin) &key)
  (map-over-children (lambda (child) (setf (parent child) zone)) zone))

(defmethod print-components progn ((zone compound-mixin) stream)
  (map-over-children
   (lambda (child)
     (format stream "~s " child))
   zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AT-MOST-ONE-CHILD-MIXIN.
;;;

(defclass at-most-one-child-mixin (compound-mixin) ())

(defmethod (setf children) :around (new-child (zone at-most-one-child-mixin))
  (declare (ignore new-child))
  (let ((child-before (children zone)))
    (call-next-method)
    (let ((child-after (children zone)))
      (unless (eq child-before child-after)
	(unless (null child-before)
	  (setf (parent child-before) nil))
	(unless (null child-after)
	  (setf (parent child-after) zone))))))

(defmethod (setf children) :before (new-child (zone at-most-one-child-mixin))
  (unless (or (null new-child) (zone-p new-child))
    (error "new child must be a zone or NIL")))

(defmethod map-over-children (function (zone at-most-one-child-mixin))
  (let ((child (children zone)))
    (unless (null child)
      (funcall function child))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SEVERAL-CHILDREN-MIXIN.
;;;

(defclass several-children-mixin (compound-mixin) ())

(defmethod (setf children) :around (new-children (zone several-children-mixin))
  (declare (ignore new-children))
  (let ((children-before (hash-table-from-children zone)))
    (call-next-method)
    (let ((children-after (hash-table-from-children zone)))
      (map-over-hash-table-difference
       (lambda (deleted-child)
	 (setf (parent deleted-child) nil))
       children-before children-after)
      (map-over-hash-table-difference
       (lambda (inserted-child)
	 (setf (parent inserted-child) zone))
       children-after children-before))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LIST-CHILDREN-MIXIN.
;;;

(defclass list-children-mixin (several-children-mixin) ())

(defmethod map-over-children (function (zone list-children-mixin))
  (mapc function (children zone)))

(defmethod (setf children) :before (new-children (zone list-children-mixin))
  ;; FIXME: check that new-children is a proper list.
  (loop for child in new-children
	do (unless (zone-p child)
	     (error "new child must be a zone"))
	   (unless (null (parent child))
	     (error "attempt to connect a zone that is already connected"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VECTOR-CHILDREN-MIXIN.
;;;

(defclass vector-children-mixin (several-children-mixin) ())

(defmethod map-over-children (function (zone vector-children-mixin))
  (map nil function (children zone)))

(defmethod (setf children) :before (new-children (zone vector-children-mixin))
  (unless (vectorp new-children)
    (error "new children must be a vector"))
  (loop for child across new-children
	do (unless (zone-p child)
	     (error "new child must be a zone"))
	   (unless (null (parent child))
	     (error "attempt to connect a zone that is already connected"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MATRIX-CHILDREN-MIXIN.
;;;

(defclass matrix-children-mixin (several-children-mixin) ())

(defmethod map-over-children (function (zone matrix-children-mixin))
  (let ((children (children zone)))
    (loop for r from 0 below (array-dimension children 0)
	  do (loop for c from 0 below (array-dimension children 1)
		   do (funcall function (aref children r c))))))

(defmethod (setf children) :before (new-children (zone matrix-children-mixin))
  (unless (and (arrayp new-children)
	       (= (array-rank new-children) 2))
    (error "new children must be an array of rank 2"))
  (loop for r from 0 below (array-dimension new-children 0)
	do (loop for c from 0 below (array-dimension new-children 1)
		 do (let ((child (aref new-children r c)))
		      (unless (zone-p child)
			(error "new child must be a zone"))
		      (unless (null (parent child))
			(error "attempt to connect a zone that is already connected"))))))

