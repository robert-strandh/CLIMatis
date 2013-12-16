(cl:in-package #:2-3-tree-test)

(defclass parent-mixin ()
  ((%parent :initform nil :accessor parent)))

(defclass tree (2-3-tree:tree)
  ())

(defclass leaf (2-3-tree:leaf parent-mixin)
  ())

(defclass 2-node (2-3-tree:2-node parent-mixin)
  ())

(defclass 3-node (2-3-tree:3-node parent-mixin)
  ())

(defclass item (parent-mixin)
  ((%value :initform (random 1000000))))

(defmethod 2-3-tree:leaf-class ((tree tree))
  'leaf)

(defmethod 2-3-tree:2-node-class ((tree tree))
  '2-node)

(defmethod 2-3-tree:3-node-class ((tree tree))
  '3-node)

(defmethod (setf 2-3-tree:item) :before ((new-item parent-mixin) leaf)
  (assert (null (parent new-item)))
  (setf (parent new-item) leaf))

(defmethod (setf 2-3-tree:left) :before ((new-left null) node)
  (let ((old-left (2-3-tree:left node)))
    (unless (null old-left)
      (setf (parent old-left) nil))))

(defmethod (setf 2-3-tree:left) :before ((new-left parent-mixin) node)
  (assert (null (parent new-left)))
  (setf (parent new-left) node))

(defmethod (setf 2-3-tree:middle) :before ((new-middle null) node)
  (let ((old-middle (2-3-tree:middle node)))
    (unless (null old-middle)
      (setf (parent old-middle) nil))))

(defmethod (setf 2-3-tree:middle) :before ((new-middle parent-mixin) node)
  (assert (null (parent new-middle)))
  (setf (parent new-middle) node))

(defmethod (setf 2-3-tree:right) :before ((new-right null) node)
  (let ((old-right (2-3-tree:right node)))
    (unless (null old-right)
      (setf (parent old-right) nil))))

(defmethod (setf 2-3-tree:right) :before ((new-right parent-mixin) node)
  (assert (null (parent new-right)))
  (setf (parent new-right) node))

(defun tree-to-list (tree)
  (loop for position from 0 below (2-3-tree:size tree)
	collect (2-3-tree:find tree position)))

(defparameter *instructions* '())

(defun test ()
  (let ((tree (make-instance 'tree))
	(state t)  ; t means insert
	(mirror (list nil)))
    (setf *instructions* '())
    (flet ((add-instruction (instruction)
	     (declare (ignorable instruction))
	     #+(or) (push instruction *instructions*)))
      (loop repeat 100000
	    do ;; Flip insert<->delete with low probability.
	       (when (< (random 1.0) 0.01)
		 (setf state (not state)))
	       (cond (state
		      (let ((item (make-instance 'item))
			    (position (random (1+ (2-3-tree:size tree)))))
			(add-instruction
			 `(2-3-tree:insert *t* (make-instance 'item) ,position))
			(2-3-tree:insert tree item position)
			(push item (cdr (nthcdr position mirror)))))
		     ((plusp (2-3-tree:size tree))
		      (let ((position (random (2-3-tree:size tree))))
			(add-instruction `(2-3-tree:delete *t* ,position))
			(2-3-tree:delete tree position)
			(pop (cdr (nthcdr position mirror)))))
		     (t nil))
	       (assert (= (length (cdr mirror)) (2-3-tree:size tree)))
	       (assert (equal (cdr mirror) (tree-to-list tree)))))))

    
