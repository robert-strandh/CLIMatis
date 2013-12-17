(cl:in-package #:clim3-layout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TREE.
;;;
;;; A tree can have no children or a single child. 

(defclass tree (clim3:standard-zone
		clim3-ext:compound-mixin
		clim3-ext:child-depth-insignificant-mixin
		2-3-tree:tree)
  ())

(defmethod clim3-ext:map-over-children (function (zone tree))
  (unless (null (2-3-tree:root zone))
    (funcall function (2-3-tree:root zone))))

;;; This method invalidates the vsprawl of the tree whenever
;;; the vsprawl of the root of the tree changes.
(defmethod clim3-ext:notify-child-vsprawl-changed (node (parent tree))
  (declare (ignore node))
  (setf (clim3-ext:vsprawl parent) nil))

;;; This method invalidates the hsprawl of the tree whenever
;;; the hsprawl of the root of the tree changes.
(defmethod clim3-ext:notify-child-hsprawl-changed (node (parent tree))
  (declare (ignore node))
  (setf (clim3-ext:hsprawl parent) nil))

(defmethod clim3-ext:compute-vsprawl ((zone tree))
  (let ((root (2-3-tree:root zone)))
    (if (null root)
	(setf (clim3-ext:vsprawl zone)
	      (clim3-sprawl:sprawl 0 0 nil))
	(progn (clim3-ext:ensure-vsprawl-valid root)
	       (setf (clim3-ext:vsprawl zone)
		     (clim3:vsprawl root))))))
				      
(defmethod clim3-ext:compute-hsprawl ((zone tree))
  (let ((root (2-3-tree:root zone)))
    (if (null root)
	(setf (clim3-ext:hsprawl zone)
	      (clim3-sprawl:sprawl 0 0 nil))
	(progn (clim3-ext:ensure-hsprawl-valid root)
	       (setf (clim3-ext:hsprawl zone)
		     (clim3:hsprawl root))))))

(defmethod clim3-ext:impose-child-layouts ((zone tree))
  (let ((root (2-3-tree:root zone))
	(width (clim3:width zone))
	(height (clim3:height zone)))
    (unless (null root)
      (clim3-ext:ensure-hsprawl-valid root)
      (clim3-ext:ensure-vsprawl-valid root)
      (setf (clim3-ext:hpos root) 0)
      (setf (clim3-ext:vpos root) 0)
      (clim3-ext:impose-size root width height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NODE.
;;;
;;; This is the base class of all the node classes: LEAF, 2-NODE, and
;;; 3-NODE.

(defclass node (clim3:standard-zone
		clim3-ext:compound-mixin
		clim3-ext:child-depth-insignificant-mixin)

  ())

;;; This method invalidates the vsprawl of the node whenever the
;;; vsprawl of the child changes.  The child can be another node, or
;;; it can be whatever zone is contained in a leaf.
(defmethod clim3-ext:notify-child-vsprawl-changed (child (parent node))
  (declare (ignore child))
  (setf (clim3-ext:vsprawl parent) nil))

;;; This method invalidates the hsprawl of the node whenever the
;;; hsprawl of the child changes.  The child can be another node, or
;;; it can be whatever zone is contained in a leaf.
(defmethod clim3-ext:notify-child-hsprawl-changed (child (parent node))
  (declare (ignore child))
  (setf (clim3-ext:hsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEAF.

(defclass leaf (node 2-3-tree:leaf)
  ())

(defmethod clim3-ext:map-over-children (function (zone leaf))
  (funcall function (2-3-tree:item zone)))

(defmethod clim3-ext:compute-vsprawl ((zone leaf))
  (let ((item (2-3-tree:item zone)))
    (clim3-ext:ensure-vsprawl-valid item)
    (setf (clim3-ext:vsprawl zone)
	  (clim3:vsprawl item))))
				      
(defmethod clim3-ext:compute-hsprawl ((zone leaf))
  (let ((item (2-3-tree:item zone)))
    (clim3-ext:ensure-hsprawl-valid item)
    (setf (clim3-ext:hsprawl zone)
	  (clim3:hsprawl item))))

(defmethod clim3-ext:impose-child-layouts ((zone leaf))
  (let ((child (2-3-tree:item zone))
	(width (clim3:width zone))
	(height (clim3:height zone)))
    (clim3-ext:ensure-hsprawl-valid child)
    (clim3-ext:ensure-vsprawl-valid child)
    (setf (clim3-ext:hpos child) 0)
    (setf (clim3-ext:vpos child) 0)
    (clim3-ext:impose-size child width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class 2-NODE.

(defclass 2-node (node 2-3-tree:2-node)
  ())

(defmethod clim3-ext:map-over-children (function (zone 2-node))
  (funcall function (2-3-tree:left zone))
  (funcall function (2-3-tree:right zone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class 3-NODE.

(defclass 3-node (node 2-3-tree:3-node)
  ())

(defmethod clim3-ext:map-over-children (function (zone 3-node))
  (funcall function (2-3-tree:left zone))
  (funcall function (2-3-tree:middle zone))
  (funcall function (2-3-tree:right zone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These :BEFORE methods on the 2-3-tree generic functions for
;;; modifying the children of the tree and the nodes make sure that
;;; the parent of the child is assigned correctly.

(defmethod (setf 2-3-tree:root) :before ((new-root null) (tree tree))
  (let ((old-root (2-3-tree:root tree)))
    (unless (null old-root)
      (setf (clim3-ext:parent old-root) nil))))

(defmethod (setf 2-3-tree:root) :before ((new-root node) (tree tree))
  (assert (null (clim3-ext:parent new-root)))
  (setf (clim3-ext:parent new-root) tree))

(defmethod (setf 2-3-tree:item) :before (zone (leaf leaf))
  (assert (null (clim3-ext:parent zone)))
  (setf (clim3-ext:parent zone) leaf))

(defmethod (setf 2-3-tree:left) :before ((new-left null) (node node))
  (let ((old-left (2-3-tree:left node)))
    (unless (null old-left)
      (setf (clim3-ext:parent old-left) nil))))

(defmethod (setf 2-3-tree:left) :before ((new-left node) (node node))
  (assert (null (clim3-ext:parent new-left)))
  (setf (clim3-ext:parent new-left) node))

(defmethod (setf 2-3-tree:middle) :before ((new-middle null) (node node))
  (let ((old-middle (2-3-tree:middle node)))
    (unless (null old-middle)
      (setf (clim3-ext:parent old-middle) nil))))

(defmethod (setf 2-3-tree:middle) :before ((new-middle node) (node node))
  (assert (null (clim3-ext:parent new-middle)))
  (setf (clim3-ext:parent new-middle) node))

(defmethod (setf 2-3-tree:right) :before ((new-right null) (node node))
  (let ((old-right (2-3-tree:right node)))
    (unless (null old-right)
      (setf (clim3-ext:parent old-right) nil))))

(defmethod (setf 2-3-tree:right) :before ((new-right node) (node node))
  (assert (null (clim3-ext:parent new-right)))
  (setf (clim3-ext:parent new-right) node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes VTREE, V2-NODE, V3-NODE.

(defclass clim3:vtree (tree)
  ())

(defmethod 2-3-tree:leaf-class ((tree clim3:vtree))
  'leaf)

(defmethod 2-3-tree:2-node-class ((tree clim3:vtree))
  'v2-node)

(defmethod 2-3-tree:3-node-class ((tree clim3:vtree))
  'v3-node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class V2-NODE.

(defclass v2-node (2-node)
  ())

(defmethod clim3-ext:compute-vsprawl ((zone v2-node))
  (let ((left (2-3-tree:left zone))
	(right (2-3-tree:right zone)))
    (clim3-ext:ensure-vsprawl-valid left)
    (clim3-ext:ensure-vsprawl-valid right)
    (let ((lsprawl (clim3:vsprawl left))
	  (rsprawl (clim3:vsprawl right)))
      (let ((lmin (clim3-sprawl:min-size lsprawl))
	    (lsize (clim3-sprawl:size lsprawl))
	    (lmax (clim3-sprawl:max-size lsprawl))
	    (rmin (clim3-sprawl:min-size rsprawl))
	    (rsize (clim3-sprawl:size rsprawl))
	    (rmax (clim3-sprawl:max-size rsprawl)))
	(setf (clim3-ext:vsprawl zone)
	      (clim3-sprawl:sprawl (+ lmin rmin)
				   (+ lsize rsize)
				   (if (or (null lmax) (null rmax))
				       nil
				       (+ lmax rmax))))))))

(defmethod clim3-ext:compute-hsprawl ((zone v2-node))
  (let ((left (2-3-tree:left zone))
	(right (2-3-tree:right zone)))
    (clim3-ext:ensure-hsprawl-valid left)
    (clim3-ext:ensure-hsprawl-valid right)
    (let ((lsprawl (clim3:hsprawl left))
	  (rsprawl (clim3:hsprawl right)))
      (let ((lmin (clim3-sprawl:min-size lsprawl))
	    (lsize (clim3-sprawl:size lsprawl))
	    (lmax (clim3-sprawl:max-size lsprawl))
	    (rmin (clim3-sprawl:min-size rsprawl))
	    (rsize (clim3-sprawl:size rsprawl))
	    (rmax (clim3-sprawl:max-size rsprawl)))
	(setf (clim3-ext:hsprawl zone)
	      (clim3-sprawl:sprawl (max lmin rmin)
				   (max lsize rsize)
				   (if (or (null lmax) (null rmax))
				       nil
				       (max lmax rmax))))))))

(defmethod clim3-ext:impose-child-layouts ((zone v2-node))
  (let* ((left (2-3-tree:left zone))
	 (right (2-3-tree:right zone)))
    (clim3-ext:ensure-hsprawl-valid left)
    (clim3-ext:ensure-vsprawl-valid left)
    (clim3-ext:ensure-hsprawl-valid right)
    (clim3-ext:ensure-vsprawl-valid right)
    (multiple-value-bind (lwidth lheight) (clim3:natural-size left)
      (multiple-value-bind (rwidth rheight) (clim3:natural-size right)
	(setf (clim3-ext:hpos left) 0)
	(setf (clim3-ext:vpos left) 0)
	(clim3-ext:impose-size left lwidth lheight)
	(setf (clim3-ext:hpos right) 0)
	(setf (clim3-ext:vpos right) lheight)
	(clim3-ext:impose-size right rwidth rheight)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class V3-NODE.

(defclass v3-node (3-node)
  ())

(defmethod clim3-ext:compute-vsprawl ((zone v3-node))
  (let ((left (2-3-tree:left zone))
	(middle (2-3-tree:middle zone))
	(right (2-3-tree:right zone)))
    (clim3-ext:ensure-vsprawl-valid left)
    (clim3-ext:ensure-vsprawl-valid middle)
    (clim3-ext:ensure-vsprawl-valid right)
    (let ((lsprawl (clim3:vsprawl left))
	  (msprawl (clim3:vsprawl middle))
	  (rsprawl (clim3:vsprawl right)))
      (let ((lmin (clim3-sprawl:min-size lsprawl))
	    (lsize (clim3-sprawl:size lsprawl))
	    (lmax (clim3-sprawl:max-size lsprawl))
	    (mmin (clim3-sprawl:min-size msprawl))
	    (msize (clim3-sprawl:size msprawl))
	    (mmax (clim3-sprawl:max-size msprawl))
	    (rmin (clim3-sprawl:min-size rsprawl))
	    (rsize (clim3-sprawl:size rsprawl))
	    (rmax (clim3-sprawl:max-size rsprawl)))
	(setf (clim3-ext:vsprawl zone)
	      (clim3-sprawl:sprawl (+ lmin mmin rmin)
				   (+ lsize msize rsize)
				   (if (or (null lmax) (null mmax) (null rmax))
				       nil
				       (+ lmax mmax rmax))))))))

(defmethod clim3-ext:compute-hsprawl ((zone v3-node))
  (let ((left (2-3-tree:left zone))
	(middle (2-3-tree:middle zone))
	(right (2-3-tree:right zone)))
    (clim3-ext:ensure-hsprawl-valid left)
    (clim3-ext:ensure-hsprawl-valid middle)
    (clim3-ext:ensure-hsprawl-valid right)
    (let ((lsprawl (clim3:hsprawl left))
	  (msprawl (clim3:hsprawl middle))
	  (rsprawl (clim3:hsprawl right)))
      (let ((lmin (clim3-sprawl:min-size lsprawl))
	    (lsize (clim3-sprawl:size lsprawl))
	    (lmax (clim3-sprawl:max-size lsprawl))
	    (mmin (clim3-sprawl:min-size msprawl))
	    (msize (clim3-sprawl:size msprawl))
	    (mmax (clim3-sprawl:max-size msprawl))
	    (rmin (clim3-sprawl:min-size rsprawl))
	    (rsize (clim3-sprawl:size rsprawl))
	    (rmax (clim3-sprawl:max-size rsprawl)))
	(setf (clim3-ext:vsprawl zone)
	      (clim3-sprawl:sprawl (max lmin mmin rmin)
				   (max lsize msize rsize)
				   (if (or (null lmax) (null mmax) (null rmax))
				       nil
				       (max lmax mmax rmax))))))))

(defmethod clim3-ext:impose-child-layouts ((zone v3-node))
  (let* ((left (2-3-tree:left zone))
	 (middle (2-3-tree:middle zone))
	 (right (2-3-tree:right zone)))
    (clim3-ext:ensure-hsprawl-valid left)
    (clim3-ext:ensure-vsprawl-valid left)
    (clim3-ext:ensure-hsprawl-valid middle)
    (clim3-ext:ensure-vsprawl-valid middle)
    (clim3-ext:ensure-hsprawl-valid right)
    (clim3-ext:ensure-vsprawl-valid right)
    (multiple-value-bind (lwidth lheight) (clim3:natural-size left)
      (multiple-value-bind (mwidth mheight) (clim3:natural-size middle)
	(multiple-value-bind (rwidth rheight) (clim3:natural-size right)
	  (setf (clim3-ext:hpos left) 0)
	  (setf (clim3-ext:vpos left) 0)
	  (clim3-ext:impose-size left lwidth lheight)
	  (setf (clim3-ext:hpos middle) 0)
	  (setf (clim3-ext:vpos middle) lheight)
	  (clim3-ext:impose-size middle mwidth mheight)
	  (setf (clim3-ext:hpos right) 0)
	  (setf (clim3-ext:vpos right) (+ lheight mheight))
	  (clim3-ext:impose-size right rwidth rheight))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operations

(defgeneric clim3:insert-zone (tree zone position))

(defgeneric clim3:delete-zone (tree position))

(defgeneric clim3:zone-count (tree))

(defmethod clim3:insert-zone ((tree tree) zone position)
  (2-3-tree:insert tree zone position))

(defmethod clim3:delete-zone ((tree tree) position)
  (2-3-tree:delete tree position))

(defmethod clim3:zone-count ((tree tree))
  (2-3-tree:size tree))
