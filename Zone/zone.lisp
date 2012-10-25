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

(defgeneric client (zone))

(defgeneric (setf client) (new-client zone))

(defclass zone ()
  ((%parent :initarg :parent :initform nil :accessor parent)
   (%hpos :initform 0 :initarg :hpos :accessor hpos :writer set-hpos)
   (%vpos :initform 0 :initarg :vpos :accessor vpos :writer set-vpos)
   (%width :initform 0 :accessor width)
   (%height :initform 0 :accessor height)
   (%hsprawl :initform nil :initarg :hsprawl :accessor hsprawl :writer set-hsprawl)
   (%vsprawl :initform nil :initarg :vsprawl :accessor vsprawl :writer set-vsprawl)
   ;; The depth is used to determine an order between the children of
   ;; a compound zone.  This order is used to determine in which order
   ;; children are painted, and in which order input zones are
   ;; searched for event handlers.  The depth can be any real number.
   (%depth :initform 0 :initarg :depth :accessor depth :writer set-depth)
   ;; A zone can belong to at most one client at a time.  The nature
   ;; of this object is entirely determined by client code.
   (%client :initform nil :accessor client)))

(defun zone-p (object)
  (typep object 'zone))

(defgeneric print-components (zone stream)
  (:method-combination progn :most-specific-last))

(defmethod print-object ((object zone) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-components object stream)))

(defun set-clients (zone client)
  (labels ((aux (zone)
	     (unless (eq (client zone) client)
	       (setf (client zone) client)
	       (map-over-children #'aux zone))))
    (aux zone)))

(defmethod (setf parent) :after ((new-parent zone) (zone zone))
  (set-clients zone (client new-parent)))

(defmethod notify-child-position-changed ((child zone) (parent null))
  nil)

(defmethod notify-child-depth-changed ((child zone) (parent null))
  nil)

;;; After the horizontal position of a zone has been explicity
;;; modified, we notify the parent.
(defmethod (setf hpos) :after (new-hpos (zone zone))
  (notify-child-position-changed zone (parent zone)))

;;; After the vertical position of a zone has been explicity
;;; modified, we notify the parent.
(defmethod (setf vpos) :after (new-vpos (zone zone))
  (notify-child-position-changed zone (parent zone)))

;;; After the hsprawl of a zone has been explicitly modified, we
;;; notify the parent.
(defmethod (setf hsprawl) :after (new-hsprawl (zone zone))
  (notify-child-hsprawl-changed zone (parent zone)))

;;; After the vsprawl of a zone has been explicitly modified, we
;;; notify the parent.
(defmethod (setf vsprawl) :after (new-vsprawl (zone zone))
  (notify-child-vsprawl-changed zone (parent zone)))

;;; Return as two values the natural width and the natural height of
;;; the zone.  We use this function to determine the size of a zone
;;; where the dimensions do not depend on the parent, such as a bboard
;;; zone or a scroller zone.
(defun natural-size (zone)
  (values (clim3-sprawl:size (hsprawl zone))
	  (clim3-sprawl:size (vsprawl zone))))

;;; Default method on NOTIFY-CHILD-HSPRAWL-CHANGED for ZONE and NULL.
;;; This method does nothing, thus allowing this generic function to
;;; be called with any zone and its parent.
(defmethod notify-child-hsprawl-changed ((child zone) (parent null))
  nil)

;;; Default method on NOTIFY-CHILD-VSPRAWL-CHANGED for ZONE and NULL.
;;; This method does nothing, thus allowing this generic function to
;;; be called with any zone and its parent.
(defmethod notify-child-vsprawl-changed ((child zone) (parent null))
  nil)

(defmethod impose-size :after ((zone zone) width height)
  (setf (width zone) width)
  (setf (height zone) height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ATOMIC-ZONE.
;;; 
;;; An atomic zone is a zone with no children.  

;;; Define primary methods for the generic functions
;;; MAP-OVER-CHILDREN, MAP-OVER-CHILDREN-TOP-TO-BOTTOM, and
;;; MAP-OVER-CHILDREN-BOTTOM-TO-TOP that do nothing. 

(defclass atomic-zone (zone) ())

(defmethod print-components progn ((zone zone) stream)
  (format stream
	  "hp: ~a vp: ~a w: ~a h: ~a d: ~a "
	  (hpos zone) (vpos zone) (width zone) (height zone) (depth zone)))

(defmethod map-over-children (function (zone atomic-zone))
  (declare (ignore function))
  nil)

(defmethod map-over-children-top-to-bottom (function (zone atomic-zone))
  (declare (ignore function))
  nil)

(defmethod map-over-children-bottom-to-top (function (zone atomic-zone))
  (declare (ignore function))
  nil)

(defmethod impose-size ((zone atomic-zone) width height)
  (declare (ignore width height))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPOUND-ZONE.
;;;
;;; A compound zone is any zone that may have some children.

(defclass compound-zone (zone)
  ((%children :initarg :children :accessor children)
   (%child-layouts-valid-p :initform nil :accessor child-layouts-valid-p)))

(defmethod initialize-instance :after ((zone compound-zone) &key)
  (map-over-children (lambda (child) (setf (parent child) zone)) zone))

(defmethod print-components progn ((zone compound-zone) stream)
  (map-over-children
   (lambda (child)
     (format stream "~s " child))
   zone))

(defmethod (setf children) :after (new-children (zone compound-zone))
  (declare (ignore new-children))
  (notify-children-changed zone)
  (setf (child-layouts-valid-p zone) nil))

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

