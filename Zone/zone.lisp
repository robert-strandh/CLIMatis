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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic fucntion HPOS.
;;;
;;; Return the current horizontal position of the zone relative to its
;;; parent.  
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not reflect the position of the corresponding top-level
;;; window on the display. 

(defgeneric hpos (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF HPOS).
;;;
;;; Set the current horizontal position of the zone relative to its
;;; parent.  
;;;
;;; Calling this function is the normal way of setting the position of
;;; a zone that is the child of a layout zone that does not impose the
;;; position of its children, such as a bboard zone or a scroller
;;; zone.
;;;
;;; Setting the position of a zone may not have the effect that an
;;; application expects.  If the zone is the child of a layout zone
;;; that imposes the position of its children, then the change will be
;;; undone before the next time around the event loop, so that no
;;; visible effect can be detected.  
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not affect the position of the corresponding top-level window
;;; on the display.
;;;
;;; Calling this function triggers the geometry-change protcol,
;;; informing the parent that a change has taken place.  

(defgeneric (setf hpos) (new-hpos zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-HPOS.
;;;
;;; Set the current horizontal position of the zone relative to its
;;; parent, without triggering the gemetry-change protocol.
;;;
;;; This function is similar to (SETF HPOS), with the difference that
;;; it does not trigger the geometry-change protocol.  It is used
;;; exclusively by internal protocols to avoid infinite recursions
;;; when a parent needs to set the position of a child as a result of
;;; some previous change.

(defgeneric set-hpos (new-hpos zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic fucntion VPOS.
;;;
;;; Return the current vertical position of the zone relative to its
;;; parent.  
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not reflect the position of the corresponding top-level window
;;; on the display.

(defgeneric vpos (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF VPOS).
;;;
;;; Set the current vertical position of the zone relative to its
;;; parent.
;;;
;;; Calling this function is the normal way of setting the position of
;;; a zone that is the child of a layout zone that does not impose the
;;; position of its children, such as a bboard zone or a scroller
;;; zone.
;;;
;;; Setting the position of a zone may not have the effect that an
;;; application expects.  If the zone is the child of a layout zone
;;; that imposes the position of its children, then the change will be
;;; undone before the next time around the event loop, so that no
;;; visible effect can be detected.  
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not affect the position of the corresponding top-level window
;;; on the display.
;;;
;;; Calling this function triggers the geometry-change protcol,
;;; informing the parent that a change has taken place.  

(defgeneric (setf vpos) (new-vpos zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-VPOS.
;;;
;;; Set the current vertical position of the zone relative to its
;;; parent, without triggering the gemetry-change protocol.
;;;
;;; This function is similar to (SETF VPOS), with the difference that
;;; it does not trigger the geometry-change protocol.  It is used
;;; exclusively by internal protocols to avoid infinite recursions
;;; when a parent needs to set the position of a child as a result of
;;; some previous change.

(defgeneric set-vpos (new-vpos zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic fucntion WIDTH.
;;;
;;; Return the current width of the zone.  
;;; 
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not reflect the width of the corresponding top-level window.

(defgeneric width (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF WIDTH).
;;;
;;; Set the current width of the zone.
;;;
;;; Calling this function is the normal way of setting the width of a
;;; zone that is the child of a layout zone that does not impose the
;;; size of its children, such as a bboard zone or a scroller zone.
;;;
;;; Setting the position of a zone may not have the effect that an
;;; application expects.  If the zone is the child of a layout zone
;;; that imposes the size of its children, then the change will be
;;; undone before the next time around the event loop, so that no
;;; visible effect can be detected.
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not affect the width of the corresponding top-level window
;;; on the display.
;;;
;;; Calling this function triggers the geometry-change protcol,
;;; informing the parent that a change has taken place.  

(defgeneric (setf width) (new-width zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-WIDTH.
;;;
;;; Set the current width of the zone relative to its parent, without
;;; triggering the gemetry-change protocol.
;;;
;;; This function is similar to (SETF WIDTH), with the difference that
;;; it does not trigger the geometry-change protocol.  It is used
;;; exclusively by internal protocols to avoid infinite recursions
;;; when a parent needs to set the width of a child as a result of
;;; some previous change.

(defgeneric set-width (new-width zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic fucntion HEIGHT.
;;;
;;; Return the current height of the zone.
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not reflect the height of the corresponding top-level window.

(defgeneric height (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF HEIGHT).
;;;
;;; Set the current height of the zone.
;;;
;;; Calling this function is the normal way of setting the height of a
;;; zone that is the child of a layout zone that does not impose the
;;; size of its children, such as a bboard zone or a scroller zone.
;;;
;;; Setting the position of a zone may not have the effect that an
;;; application expects.  If the zone is the child of a layout zone
;;; that imposes the size of its children, then the change will be
;;; undone before the next time around the event loop, so that no
;;; visible effect can be detected.
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not affect the height of the corresponding top-level window on
;;; the display.
;;;
;;; Calling this function triggers the geometry-change protcol,
;;; informing the parent that a change has taken place.

(defgeneric (setf height) (new-height zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-HEIGHT.
;;;
;;; Set the current height of the zone relative to its parent, without
;;; triggering the gemetry-change protocol.
;;;
;;; This function is similar to (SETF HEIGHT), with the difference that
;;; it does not trigger the geometry-change protocol.  It is used
;;; exclusively by internal protocols to avoid infinite recursions
;;; when a parent needs to set the height of a child as a result of
;;; some previous change.

(defgeneric set-height (new-height zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function HGIVE.

(defgeneric hgive (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF HGIVE).

(defgeneric (setf hgive) (new-hgive zone))

(defmethod (setf hgive) :after ((new-hgive cons) zone)
  (notify-child-gives-changed zone (parent zone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VGIVE.

(defgeneric vgive (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF HGIVE).

(defgeneric (setf vgive) (new-vgive zone))

(defmethod (setf vgive) :after ((new-vgive cons) zone)
  (notify-child-gives-changed zone (parent zone)))

(defgeneric depth (zone))
(defgeneric (setf depth) (new-depth zone))
(defgeneric client (zone))
(defgeneric (setf client) (new-client zone))

(defclass zone ()
  ((%parent :initarg :parent :initform nil :accessor parent)
   (%hpos :initform 0 :initarg :hpos :accessor hpos :writer set-hpos)
   (%vpos :initform 0 :initarg :vpos :accessor vpos :writer set-vpos)
   (%width :accessor width :writer set-width)
   (%height :accessor height :writer set-height)
   (%hgive :initarg :hgive :accessor hgive :writer set-hgive)
   (%vgive :initarg :vgive :accessor vgive :writer set-vgive)
   ;; The depth is used to determine an order between the children of
   ;; a compound zone.  This order is used to determine in which order
   ;; children are painted, and in which order input zones are
   ;; searched for event handlers.  The depth can be any real number.
   (%depth :initform 0 :initarg :depth :accessor depth :writer set-depth)
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

;;; Default method on NOTIFY-CHILD-GIVES-INVALID for ZONE and NULL.
;;; This method does nothing, thus allowing this generic function to
;;; be called with any zone and its parent.
(defmethod notify-child-gives-invalid ((child zone) (parent null))
  nil)

;;; Default method on NOTIFY-CHILD-GIVES-INVALID for ZONE and ZONE.
;;; This method signals an error, forcing the parent zone type to
;;; choose an action as a result of a call to this function. 
(defmethod notify-child-gives-invalid ((child zone) (parent zone))
  (error "No action specified for zone ~s" parent))

;;; Default method on NOTIFY-CHILD-GIVES-CHANGED for ZONE and NULL.
;;; This method does nothing, thus allowing this generic function to
;;; be called with any zone and its parent.
(defmethod notify-child-gives-changed ((child zone) (parent null))
  nil)

;;; Default method on NOTIFY-CHILD-GIVES-CHANGED for ZONE and ZONE.
;;; This method signals an error, forcing the parent zone type to
;;; choose an action as a result of a call to this function. 
(defmethod notify-child-gives-changed ((child zone) (parent zone))
  (error "No action specified for zone ~s" parent))

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

(defclass compound-zone (zone)
  ((%children :initarg :children :accessor children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILDREN-CHANGED.
;;;
;;; This function is called when the children of some compound zone
;;; have changed, such as when a child has been added or removed.  
;;; 
;;; FIXME: say more.

(defgeneric notify-children-changed (zone))

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

(defclass dependent-gives-mixin () ())


(defmethod gives-valid-p ((zone dependent-gives-mixin))
  (and (not (null (hgive zone)))
       (not (null (vgive zone)))))

(defmethod mark-gives-invalid ((zone dependent-gives-mixin))
  (setf (hgive zone) nil)
  (setf (vgive zone) nil))

(defmethod notify-child-gives-invalid ((child zone)
				       (parent dependent-gives-mixin))
  (invalidate-gives parent))

(defmethod notify-child-gives-changed ((child zone)
				       (parent dependent-gives-mixin))
  (invalidate-gives parent))

(defmethod notify-children-changed ((zone dependent-gives-mixin))
  (invalidate-gives zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HDEPENDENT-GIVES-MIXIN.

(defclass hdependent-gives-mixin () ())

(defmethod gives-valid-p ((zone hdependent-gives-mixin))
  (not (null (hgive zone))))

(defmethod mark-gives-invalid ((zone hdependent-gives-mixin))
  (setf (hgive zone) nil))

(defmethod notify-child-gives-invalid ((child zone)
				       (parent hdependent-gives-mixin))
  (invalidate-gives parent))

(defmethod notify-child-gives-changed ((child zone)
				       (parent hdependent-gives-mixin))
  (invalidate-gives parent))

(defmethod notify-children-changed ((zone hdependent-gives-mixin))
  (invalidate-gives zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VDEPENDENT-GIVES-MIXIN.

(defclass vdependent-gives-mixin () ())

(defmethod gives-valid-p ((zone vdependent-gives-mixin))
  (not (null (vgive zone))))

(defmethod mark-gives-invalid ((zone vdependent-gives-mixin))
  (setf (vgive zone) nil))

(defmethod notify-child-gives-invalid ((child zone)
				       (parent vdependent-gives-mixin))
  (invalidate-gives parent))

(defmethod notify-child-gives-changed ((child zone)
				       (parent vdependent-gives-mixin))
  (invalidate-gives parent))

(defmethod notify-children-changed ((zone vdependent-gives-mixin))
  (invalidate-gives zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INDEPENDENT-GIVES-MIXIN.

(defclass independent-gives-mixin () ())

(defmethod gives-valid-p ((zone independent-gives-mixin))
  t)

(defmethod mark-gives-invalid ((zone independent-gives-mixin))
  (error "Attempt to marks as invalid the gives of an independent zone ~s"
	 zone))

(defmethod notify-child-gives-invalid ((child zone)
				       (parent independent-gives-mixin))
  nil)

(defmethod notify-child-gives-changed ((child zone)
				       (parent independent-gives-mixin))
  nil)

(defmethod clim3-zone:combine-child-gives ((zone independent-gives-mixin))
  nil)

(defmethod notify-children-changed ((zone dependent-gives-mixin))
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
      (setf (parent (car children-before)) nil)
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
