(cl:in-package #:clim3-layout)

(defgeneric node-count (tree))

(defclass tree (clim3:standard-zone
		clim3-ext:at-most-one-child-mixin
		clim3-ext:changing-children-changes-nothing-mixin)
  ())

(defmethod clim3-ext:impose-child-layouts ((zone tree))
  nil)

(defclass node ()
  ((%vtree :initarg :vtree :reader vtree)
   (%node-count :initform 1 :accessor node-count)))
   
(defun splay (node)
  (setf (clim3:children (vtree node))
	(splay-tree:splay node)))

(defmethod node-count ((tree tree))
  (if (null (clim3:children tree))
      0
      (node-count (clim3:children tree))))

(defun find-node (tree node-number)
  (assert (<= 0 node-number (1- (node-count tree))))
  (labels ((find-relative-node (node node-number)
	     (let ((left-count (if (null (splay-tree:left node))
				   0
				   (node-count (splay-tree:left node)))))
	       (cond ((< node-number left-count)
		      (find-relative-node (splay-tree:left node) node-number))
		     ((= node-number left-count)
		      node)
		     (t
		      (find-relative-node (splay-tree:right node)
					  (- node-number left-count 1)))))))
    (find-relative-node (clim3:children tree) node-number)))

(defgeneric clim3:insert-node (tree node position))

(defgeneric clim3:delete-node (tree position))

(defmethod clim3:delete-node ((tree tree) position)
  (assert (<= 0 position (1- (node-count tree))))
  (splay (find-node tree position))
  (let ((root (clim3:children tree)))
    (cond ((null (splay-tree:left root))
	   (setf (clim3:children tree) (splay-tree:right root)))
	  ((null (splay-tree:right root))
	   (setf (clim3:children tree) (splay-tree:left root)))
	  (t
	   (splay (find-node tree (1- position)))
	   (let* ((root (clim3:children tree))
		  (right (splay-tree:right root))
		  (right-right (splay-tree:right right)))
	     (setf (splay-tree:right root) nil)
	     (setf (splay-tree:right right) nil)
	     (setf (splay-tree:right root) right-right)))))
  (let ((root (clim3:children tree)))
    (clim3-ext:notify-child-vsprawl-changed root tree)
    (clim3-ext:notify-child-hsprawl-changed root tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes VTREE and VNODE.

;;; We do not include STANDARD-ZONE as a superclass because this zone
;;; needs no parent, no sprawls, and no depth.
(defclass clim3:vnode (node
		       splay-tree:node
		       clim3:zone
		       clim3-ext:position-mixin
		       clim3-ext:size-mixin
		       clim3-ext:client-mixin
		       clim3-ext:changing-child-position-not-allowed-mixin)
  ((%line :initarg :line :reader line)))

(defclass clim3:vtree (tree) ())

(defmethod clim3-ext:compute-vsprawl ((zone clim3:vtree))
  (if (null (clim3:children zone))
      (clim3-sprawl:sprawl 0 0 nil)
      (let ((height (clim3:height (clim3:children zone))))
	(clim3-sprawl:sprawl height height height))))
				      
(defmethod clim3-ext:compute-hsprawl ((zone clim3:vtree))
  (if (null (clim3:children zone))
      (clim3-sprawl:sprawl 0 0 nil)
      (let ((width (clim3:width (clim3:children zone))))
	(clim3-sprawl:sprawl width width width))))

(defmethod (setf splay-tree:left) :before
    ((new-left null) (node clim3:vnode))
  (let ((existing-left (splay-tree:left node))
	(right (splay-tree:right node)))
    (unless (null existing-left)
      (decf (node-count node) (node-count existing-left))
      (decf (clim3:height node) (clim3:height existing-left))
      (setf (clim3:vpos (line node)) 0)
      (setf (clim3:width node)
	    (max (nth-value 0 (clim3:natural-size (line node)))
		 (if (null right) 0 (clim3:width right))))
      (unless (null right)
	(decf (clim3:vpos (splay-tree:right node))
	      (clim3:height existing-left))))))

(defmethod (setf splay-tree:left) :before
    ((new-left clim3:vnode) (node clim3:vnode))
  (let ((right (splay-tree:right node)))
    (incf (node-count node) (node-count new-left))
    (incf (clim3:height node) (clim3:height new-left))
    (setf (clim3:vpos (line node)) (clim3:height new-left))
    (setf (clim3:width node)
	  (max (clim3:width node) (clim3:width new-left)))
    (unless (null right)
      (incf (clim3:vpos (splay-tree:right node))
	    (clim3:height new-left)))))

(defmethod (setf splay-tree:right) :before
    ((new-right null) (node clim3:vnode))
  (let ((existing-right (splay-tree:right node))
	(left (splay-tree:left node)))
    (unless (null existing-right)
      (decf (node-count node) (node-count existing-right))
      (decf (clim3:height node) (clim3:height existing-right))
      (setf (clim3:width node)
	    (max (nth-value 0 (clim3:natural-size (line node)))
		 (if (null left) 0 (clim3:width left)))))))

(defmethod (setf splay-tree:right) :before
    ((new-right clim3:vnode) (node clim3:vnode))
  (let ((existing-right (splay-tree:right node)))
    (incf (node-count node) (node-count existing-right))
    (incf (clim3:height node) (clim3:height existing-right))
    (setf (clim3:width node)
	  (max (clim3:width node) (clim3:width new-right)))))

(defmethod clim3-ext:map-over-children (function (zone clim3:vnode))
  (unless (null (splay-tree:left zone))
    (funcall function (splay-tree:left zone)))
  (funcall function (line zone))
  (unless (null (splay-tree:right zone))
    (funcall function (splay-tree:right zone))))

(defmethod clim3:insert-node ((tree clim3:vtree) (node clim3:vnode) position)
  (assert (<= 0 position (node-count tree)))
  (cond ((null (clim3:children tree))
	 (setf (clim3:children tree) node))
	((zerop position)
	 (splay (find-node tree 0))
	 (setf (splay-tree:left (clim3:children tree)) node))
	((= position (node-count tree))
	 (splay (find-node tree (1- position)))
	 (setf (splay-tree:right (clim3:children tree)) node))
	(t
	 (splay (find-node tree (1- position)))
	 (let* ((root (clim3:children tree))
		(left (splay-tree:left root)))
	   (setf (splay-tree:left root) nil)
	   (setf (splay-tree:left node) left)
	   (setf (splay-tree:right node) root)
	   (setf (clim3:children tree) node))))
  (let ((root (clim3:children tree)))
    (clim3-ext:notify-child-vsprawl-changed root tree)
    (clim3-ext:notify-child-hsprawl-changed root tree)))

