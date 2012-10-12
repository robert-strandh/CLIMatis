(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

;;; Check whether an object is a proper list.  We expect the list to
;;; be relatively short, so there is no point in doing anything fancy.
;;; Loop over the CONS cells of the list until either the end of the
;;; list is found, or we see a cell that we have already seen.  If se
;;; see a cell we have already seen, or we reach the end of the list
;;; and it is not NIL, then we do not have a proper list.  If we reach
;;; NIL at the end of the slist without having seen a cell twice, then
;;; we have a proper list.
(defun proper-list-p (object)
  (let ((cells '())
	(rest object))
    (loop until (atom rest)
	  do (if (member rest cells :test #'eq)
		 (return-from proper-list-p nil)
		 (progn (push rest cells)
			(pop rest))))
    (null rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ZONE.
;;;
;;; The position of a zone is in the coordinate system of its parent.
;;; 
;;; Client code can set the position and the dimensions of a zone, but
;;; if the zone is the child of a layout zone, then the layout
;;; protocol may modify those parameters again, so that the
;;; modification by the client will have no effect.  Some layout zones
;;; like the bboard zone do not impose any position or dimensions on
;;; the children.  Setting the position and the dimensions of a child
;;; of such a layout zone will move and resize the child. 

(defgeneric parent (zone))
(defgeneric (setf parent) (new-parent zone))
(defgeneric hpos (zone))
(defgeneric (setf hpos) (new-hpos zone))
(defgeneric vpos (zone))
(defgeneric (setf vpos) (new-vpos zone))
(defgeneric width (zone))
(defgeneric (setf width) (new-width zone))
(defgeneric height (zone))
(defgeneric (setf height) (new-height zone))
(defgeneric hgive (zone))
(defgeneric (setf hgive) (new-hgive zone))
(defgeneric vgive (zone))
(defgeneric (setf vgive) (new-vgive zone))
(defgeneric depth (zone))
(defgeneric (setf depth) (new-depth zone))
(defgeneric client (zone))
(defgeneric (setf client) (new-client zone))

(defclass zone ()
  ((%parent :initarg :parent :initform nil :accessor parent)
   (%hpos :initform 0 :initarg :hpos :accessor hpos)
   (%vpos :initform 0 :initarg :vpos :accessor vpos)
   (%width :accessor width)
   (%height :accessor height)
   (%hgive :initarg :hgive :accessor hgive)
   (%vgive :initarg :vgive :accessor vgive)
   ;; The depth is used to determine an order between the children of
   ;; a compound zone.  This order is used to determine in which order
   ;; children are painted, and in which order input zones are
   ;; searched for event handlers.  The depth can be any real number.
   (%depth :initform 0 :initarg :depth :accessor depth)
   ;; A zone can belong to at most one client at a time.  If this
   ;; value is NIL, then the zone currently does not belong to any
   ;; client.  The nature of this object is entirely determined by
   ;; client code.
   (%client :initform nil :initarg :client :accessor client)))

(defun zone-p (object)
  (typep object 'zone))

;;; Return as two values the natural width and the natural height of
;;; the zone.  We use this function to determine the size of a zone
;;; where the dimensions do not depend on the parent, such as a bboard
;;; zone or a scroller zone. 
(defun natural-size (zone)
  (values (round (rigidity:natural-size (hgive zone)))
	  (round (rigidity:natural-size (vgive zone)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPUTE-GIVES.
;;;
;;; This function is responsible for first recursively calling itself
;;; recursively on all the children of the zone, second for computing
;;; the gives of the zone itself, and third, for setting the gives of
;;; the zone to the result of the computation.  The order in which it
;;; recursively calls itself on the children is not defined.

(defgeneric compute-gives (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function IMPOSE-LAYOUT.
;;;
;;; This function is called in order to set the position and
;;; dimensions of a zone.  The zone must comply.  The :before method
;;; on ZONE sets the hpos, the vpos, the width and the height of the
;;; zone by calling (SETF HPOS), (SETF VPOS), (SETF WIDTH), and (SETF
;;; HEIGHT).  The contract of the primary method is to take the
;;; consequences of the size imposition, for instance to recursively
;;; impose sizes on the children.

(defgeneric impose-layout (zone hpos vpos width height))

(defmethod impose-layout :before ((zone zone) hpos vpos width height)
  (setf (hpos zone) hpos
	(vpos zone) vpos
	(width zone) width
	(height zone) height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CONNECT.
;;;
;;; This function is called when a zone Z becomes the child of some
;;; other zone P.  It is called with the client of P, Z, and P as
;;; arguments.
;;;
;;; The default method (on NULL) does nothing. 
;;;
;;; Client code will typically specialize at least on the type of the
;;; client.


(defgeneric notify-connect (client child parent))

(defmethod notify-connect ((client null) child parent)
  (declare (ignore child parent))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-DISCONNECT.
;;;
;;; This function is called when a zone Z is removed as the child of
;;; some other zone P.  It is called with the client of P, Z, and P as
;;; arguments.
;;;
;;; The default method (on NULL) does nothing. 
;;;
;;; Client code will typically specialize at least on the type of the
;;; client.

(defgeneric notify-disconnect (client child parent))

(defmethod notify-disconnect ((client null) child parent)
  (declare (ignore child parent))
  nil)

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

(defmethod notify-child-gives-invalid ((child zone) (parent null))
  nil)

(defmethod notify-child-gives-invalid ((child zone) (parent zone))
  (error "No action specified for zone ~s" parent))

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
;;; If both the gives of the zone are alread invalid, then this
;;; function does nothing.  Otherwise, it invalidates them and calls
;;; the function NOTIFY-CHILD-GIVES-INVALID with the zone and its
;;; parent.

(defgeneric invalidate-gives (zone))

(defmethod invalidate-gives ((zone zone))
  (unless (and (null (hgive zone)) (null (vgive zone)))
    (setf (hgive zone) nil)
    (setf (vgive zone) nil)
    (notify-child-gives-invalid zone (parent zone))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ATOMIC-ZONE.
;;; 
;;; An atomic zone is a zone with no children.  

;;; Define primary methods for the generic functions
;;; MAP-OVER-CHILDREN, MAP-OVER-CHILDREN-TOP-TO-BOTTOM, and
;;; MAP-OVER-CHILDREN-BOTTOM-TO-TOP that do nothing. 

(defclass atomic-zone (zone) ())

(defmethod map-over-children (function (zone atomic-zone))
  (declare (ignore function))
  nil)

(defmethod map-over-children-top-to-bottom (function (zone atomic-zone))
  (declare (ignore function))
  nil)

(defmethod map-over-children-bottom-to-top (function (zone atomic-zone))
  (declare (ignore function))
  nil)

;;; Define a primary method for COMPUTE-GIVES that does nothing.
;;; Subclasses of ATOMIC-ZONE that need to do some calculation to
;;; determinre the gives must override this method. 
(defmethod compute-gives ((zone atomic-zone))
  nil)

;;; Define a primary method for IMPOSE-LAYOUT that does nothing.  
(defmethod impose-layout ((zone atomic-zone) hpos vpos width height)
  (declare (ignore hpos vpos width height))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPOUND-ZONE.
;;;
;;; A compound zone is any zone that may have some children.

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
;;; There is an :around method on this function (see below),
;;; specialized on COMPOUND-ZONE.  It always calls the primary
;;; methods, but it also does some extra work such as error checking
;;; and client notification.  The :around mehod calls CHILDREN, which
;;; has as a consequence that in order to use (SETF CHILDREN) the
;;; corresponding slot must be bound. 

(defgeneric (setf children) (new-children compound-zone))

(defclass compound-zone (zone)
  ((%children :initarg :children :accessor children)
   (%depth-ordered-children :initform nil :accessor depth-ordered-children)))


(defun ensure-depth-ordered-children (zone)
  (when (null (depth-ordered-children zone))
    (let ((children '()))
      (map-over-children (lambda (child) (push child children)) zone)
      (setf (depth-ordered-children zone)
	    (sort (coerce children 'vector) #'< :key #'depth)))))

(defmethod map-over-children-top-to-bottom (function (zone compound-zone))
  (ensure-depth-ordered-children zone)
  (let ((depth-ordered-children (depth-ordered-children zone)))
    (loop for i from 0 below (length depth-ordered-children)
	  do (funcall function (aref depth-ordered-children i)))))

(defmethod map-over-children-bottom-to-top (function (zone compound-zone))
  (ensure-depth-ordered-children zone)
  (let ((depth-ordered-children (depth-ordered-children zone)))
    (loop for i downfrom (1- (length depth-ordered-children)) to 0
	  do (funcall function (aref depth-ordered-children i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMBINE-CHILD-GIVES.
;;;
;;; This generic function is charged with combining the give of each
;;; child of a compound zone, and calling the functions (SETF HGIVE)
;;; and (SETF VGIVE) to set the result for the zone.

(defgeneric combine-child-gives (compound-zone))

;;; For a compound zone, in order to compute all gives, we
;;; recursively compute the gives of the children and then
;;; combine the result. 
(defmethod compute-gives ((zone compound-zone))
  (map-over-children #'compute-gives zone)
  (combine-child-gives zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPOUND-SIMPLE-ZONE.
;;;
;;; A compound simple zone is a compound zone in which the order of
;;; the children is of no importance.  We store the children in a
;;; list.
;;;
;;; This class is not meant to be instantiated directly.  Instead, it
;;; is a base class for other zone classes.

(defclass compound-simple-zone (compound-zone)
  ()
  (:default-initargs :children '()))

;;; Since the children are stored as a list, we can use mapc.
(defmethod map-over-children (function (zone compound-simple-zone))
  (mapc function (children zone)))

(defun check-and-coerce-sequence (sequence)
  (cond ((proper-list-p sequence)
	 (if (every #'zone-p sequence)
	     sequence
	     (error "Attemps to set the non-zone objet ~s as a child."
		    (find-if-not #'zone-p sequence))))
	((listp sequence)
	 (error "The list of children must be a proper list, but ~s was found."
		sequence))
	((vectorp sequence)
	 (if (every #'zone-p sequence)
	     (coerce sequence 'list)
	     (error "Attemps to set the non-zone objet ~s as a child."
		    (find-if-not #'zone-p sequence))))
	(t
	 (error "The children must be a proper sequence, but ~s was found."
		sequence))))

;;; This purpose of this around method is to make sure that we are
;;; given a proper sequence of zones, and if it is a vector, to
;;; convert it to a list.
(defmethod (setf children) :around (new-children (zone compound-simple-zone))
  (call-next-method (check-and-coerce-sequence new-children) zone))

(defmethod initialize-instance :after ((zone compound-simple-zone) &key)
  (let* ((children (children zone))
	 (checked-children (check-and-coerce-sequence children)))
    (unless (eq children checked-children)
      (setf (children zone) children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPOUND-SEQUENCE-ZONE.
;;;
;;; A compound sequence zone is a compound zone that imposes a linear
;;; order of the children.  The linear order may for instance
;;; determine relative positions of the children.  We store the
;;; children in a list.
;;;
;;; This class is not meant to be instantiated directly.  Instead, it
;;; is a base class for other zone classes.

(defclass compound-sequence-zone (compound-zone)
  ()
  (:default-initargs :children '()))

;;; Since the children are stored as a list, we can use mapc.
(defmethod map-over-children (function (zone compound-sequence-zone))
  (mapc function (children zone)))

;;; This purpose of this around method is to make sure that we are
;;; given a proper sequence of zones, and if it is a vector, to
;;; convert it to a list.
(defmethod (setf children) :around (new-children (zone compound-sequence-zone))
  (call-next-method (check-and-coerce-sequence new-children) zone))

(defmethod initialize-instance :after ((zone compound-sequence-zone) &key)
  (let* ((children (children zone))
	 (checked-children (check-and-coerce-sequence children)))
    (unless (eq children checked-children)
      (setf (children zone) children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPOUND-MATRIX-ZONE.
;;;
;;; A compound matrix zone is a compound zone that imposes a
;;; 2-dimensional arrangement of its children.  We store the children
;;; as a 2-dimensional array. 
;;;
;;; This class is not meant to be instantiated directly.  Instead, it
;;; is a base class for other zone classes.

(defclass compound-matrix-zone (compound-zone)
  ()
  (:default-initargs :children (make-array '(0 0))))

(defmethod map-over-children (function (zone compound-matrix-zone))
  (let ((children (children zone)))
    (loop for row from 0 below (array-dimension children 0)
	  do (loop for col from 0 below (array-dimension children 1)
		   do (funcall function (aref children row col))))))

(defmethod initialize-instance :after ((zone compound-matrix-zone) &key (children #2A()))
  (setf (slot-value zone '%children) #2A())
  (setf (children zone) children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DEPENDENT-GIVES-MIXIN.
;;; Class INDEPENDENT-GIVES-MIXIN.
;;;
;;; Exactly one of these classes should be mixed into any compound
;;; zone.  They each supply one method on NOTIFY-CHILD-GIVES-INVALID,
;;; specializing the PARENT parameter.  The method specialized for
;;; DEPENDENT-GIVES-MIXIN calls INVALIDATE-GIVES, and the method
;;; specialized for INDEPENDENT-GIVES-MIXIN does nothing. 

(defclass dependent-gives-mixin () ())

(defmethod notify-child-gives-invalid ((child zone)
				       (parent dependent-gives-mixin))
  (invalidate-gives parent))

(defclass independent-gives-mixin () ())

(defmethod notify-child-gives-invalid ((child zone)
				       (parent independent-gives-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AT-MOST-ONE-CHILD-MIXIN.
;;; Class ANY-NUMBER-OF-CHILDREN-MIXIN.
;;;
;;; Exactly one of these classes shoudl be mixed into any compound
;;; zone.

(defclass at-most-one-child-mixin () ())

;;; Around method for (SETF CHILDREN)
;;; This around method is charged with a few things:
;;;
;;;  * Check that the children is a sequence of at most one element.
;;;
;;;  * Check that no new child already has a parent.
;;;
;;;  * Set the parent of every removed child to NIL.
;;;
;;;  * Notify the client of changes in parent-child relations.

(defmethod (setf children) :around
  (new-children (zone at-most-one-child-mixin))
  (let ((children-before (children zone)))
    (unless (null children-before)
      (setf (parent (car children-before) nil))
      (notify-disconnect (client zone) (car children-before) zone)))
  (let ((children-after
	  (cond ((or (null new-children)
		     (and (consp new-children) (null (cdr new-children))))
		 new-children)
		((consp new-children)
		 (error "List must be a proper list of length at most 1: ~s"
			new-children))
		((vectorp new-children)
		 (case (length new-children)
		   (0 '())
		   (1 (list (aref new-children 0)))
		   (t (error "Vector must have at most one element"))))
		(t
		 (error "Children must be a proper sequence of length at most 1 ~s"
			new-children)))))
    (unless (null children-after)
      (unless (null (parent (car children-after)))
	(error "Attempt to connect a zone that is already connected ~s"
	       (car children-after)))
      (unless (zone-p (car children-after))
	(error "Child must be a zone: ~s" (car children-after)))
      (notify-connect (client zone) (car children-after) zone))
    (call-next-method children-after)))

(defmethod map-over-children-top-to-bottom
    (function (zone at-most-one-child-mixin))
  (unless (null (children zone))
    (funcall function (car (children zone)))))

(defmethod map-over-children-bottom-to-top
    (function (zone at-most-one-child-mixin))
  (unless (null (children zone))
    (funcall function (car (children zone)))))

(defclass any-number-of-children-mixin ()
  ((%depth-ordered-children :initform nil :accessor depth-ordered-children)))

;;; Around method for (SETF CHILDREN)
;;; This around method is charged with a few things:
;;;
;;;  * Check that no new child already has a parent.
;;;
;;;  * Set the parent of every removed child to NIL.
;;;
;;;  * Destroy the depth-ordered vector of children.
;;;
;;;  * Notify the client of changes in parent-child relations.

(defmethod (setf children) :around
  (new-children (zone any-number-of-children-mixin))
  (let ((children-before (children zone))
	(client (client zone))
	(table-children-before (make-hash-table :test #'eq))
	(table-children-after (make-hash-table :test #'eq)))
    ;; Fill the table with the current children of the zone.
    (map-over-children (lambda (child) 
			 (setf (gethash child table-children-before) t))
		       zone)
    ;; Set the new children.
    (call-next-method)
    ;; Fill the table with the new children of the zone.
    (map-over-children (lambda (child) 
			 (setf (gethash child table-children-after) t))
		       zone)
    ;; Check that no new child already has a parent.
    (maphash (lambda (new-child value)
	       (declare (ignore value))
	       (unless (gethash new-child table-children-before)
		 (unless (null (parent new-child))
		   ;; Restore the old children.
		   (call-next-method children-before zone)
		   (error "Attemps to add a child that already has a parent ~s."
			  new-child))))
	     table-children-after)
    ;; Destroy the depth-ordered vector of children.
    (setf (depth-ordered-children zone) nil)
    ;; Set parent of new children, and call NOTIFY-CONNECT.
    (maphash (lambda (new-child value)
	       (declare (ignore value))
	       (unless (gethash new-child table-children-before)
		 (setf (parent new-child) zone)
		 (notify-connect client new-child zone)))
	     table-children-after)
    ;; Set the parent of removed children to nil, and call
    ;; NOTIFY-DISCONNECT.
    (maphash (lambda (old-child value)
	       (declare (ignore value))
	       (unless (gethash old-child table-children-after)
		 (setf (parent old-child) nil)
		 (notify-disconnect client old-child zone)))
	     table-children-before)))

(defun ensure-depth-ordered-children (zone)
  (when (null (depth-ordered-children zone))
    (let ((children '()))
      (map-over-children (lambda (child) (push child children)) zone)
      (setf (depth-ordered-children zone)
	    (sort (coerce children 'vector) #'< :key #'depth)))))

(defmethod map-over-children-top-to-bottom
    (function (zone any-number-of-children-mixin))
  (ensure-depth-ordered-children zone)
  (let ((depth-ordered-children (depth-ordered-children zone)))
    (loop for i from 0 below (length depth-ordered-children)
	  do (funcall function (aref depth-ordered-children i)))))

(defmethod map-over-children-bottom-to-top
    (function (zone any-number-of-children-mixin))
  (ensure-depth-ordered-children zone)
  (let ((depth-ordered-children (depth-ordered-children zone)))
    (loop for i downfrom (1- (length depth-ordered-children)) to 0
	  do (funcall function (aref depth-ordered-children i)))))
