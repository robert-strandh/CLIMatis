(cl:in-package #:2-3-tree)

(defgeneric size (tree-or-node))

(defclass tree ()
  ((%root :initform nil :accessor root)))

(defmethod size ((tree tree))
  (if (null (root tree)) 0 (size (root tree))))

(defclass node ()
  ((%size :initarg :size :accessor size)))

(defclass leaf (node)
  ((%item :initform nil :accessor item)))

(defclass 2-node (node)
  ((%left :initform nil :accessor left)
   (%right :initform nil :accessor right)))

(defclass 3-node (node)
  ((%left :initform nil :accessor left)
   (%middle :initform nil :accessor middle)
   (%right :initform nil :accessor right)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a tree, return the name of the class to use for the nodes.

(defgeneric leaf-class (tree)
  (:method (tree) 'leaf))

(defgeneric 2-node-class (tree)
  (:method (tree) '2-node))

(defgeneric 3-node-class (tree)
  (:method (tree) '3-node))

;;; This variable is set by the methods specialiced for TREE so that
;;; is available when we need to determine what class to allocate when
;;; individual nodes are created.
(defvar *tree*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find an object at a particular position.

(defgeneric find (tree-or-node position))

(defmethod find ((node leaf) position)
  (declare (ignore position))
  (item node))

(defmethod find ((node 2-node) position)
  (with-accessors ((left left) (right right)) node
    (with-accessors ((left-size size)) left
      (if (< position left-size)
	  (find left position)
	  (find right (- position left-size))))))

(defmethod find ((node 3-node) position)
  (with-accessors ((left left) (middle middle) (right right)) node
    (with-accessors ((left-size size)) left
      (with-accessors ((middle-size size)) middle
	(cond ((< position left-size)
	       (find left position))
	      ((< position (+ left-size middle-size))
	       (find middle (- position left-size)))
	      (t
	       (find right (- position left-size middle-size))))))))

(defmethod find ((tree tree) position)
  (unless (<= 0 position (1- (size tree)))
    (error "Invalid position."))
  (find (root tree) position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert an object at a particular position

(defgeneric insert (tree-or-node item position))

(defmethod insert ((node leaf) item position)
  (let ((new-leaf (make-instance (leaf-class *tree*))))
    (setf (item new-leaf) item)
    (if (= position 0)
	(values new-leaf node)
	;; POSITION must be equal to 1.
	(values node new-leaf))))

(defmethod insert ((node 2-node) item position)
  (let* ((left (left node))
	 (right (right node))
	 (left-size (size left)))
    (cond ((< position left-size)
	   (setf (left node) nil)
	   (multiple-value-bind (n1 n2)
	       (insert left item position)
	     (if (null n2)
		 (progn (setf (left node) n1)
			(incf (size node))
			(values node nil))
		 (let ((new-node (make-instance (3-node-class *tree*))))
		   (setf (right node) nil)
		   (setf (left new-node) n1)
		   (setf (middle new-node) n2)
		   (setf (right new-node) right)
		   (setf (size new-node) (1+ (size node)))
		   (values new-node nil)))))
	  (t
	   (setf (right node) nil)
	   (multiple-value-bind (n1 n2)
	       (insert right item (- position left-size))
	     (if (null n2)
		 (progn (setf (right node) n2)
			(incf (size node))
			(values node nil))
		 (let ((new-node (make-instance (3-node-class *tree*))))
		   (setf (left node) nil)
		   (setf (left new-node) left)
		   (setf (middle new-node) n1)
		   (setf (right new-node) n2)
		   (setf (size new-node) (1+ (size node)))
		   (values new-node nil))))))))

(defmethod insert ((node 3-node) item position)
  (let* ((left (left node))
	 (middle (middle node))
	 (right (right node))
	 (left-size (size left))
	 (middle-size (size middle)))
    (cond ((< position left-size)
	   (setf (left node) nil)
	   (multiple-value-bind (n1 n2)
	       (insert left item position)
	     (if (null n2)
		 (progn (setf (left node) n1)
			(incf (size node))
			(values node nil))
		 (let ((new-node-1 (make-instance (2-node-class *tree*)))
		       (new-node-2 (make-instance (2-node-class *tree*))))
		   (setf (middle node) nil)
		   (setf (right node) nil)
		   (setf (left new-node-1) n1)
		   (setf (right new-node-1) n2)
		   (setf (size new-node-1) (+ (size n1) (size n2)))
		   (setf (left new-node-2) middle)
		   (setf (right new-node-2) right)
		   (setf (size new-node-1) (+ (size middle) (size right)))
		   (values new-node-1 new-node-2)))))
	  ((< position (+ left-size middle-size))
	   (setf (middle node) nil)
	   (multiple-value-bind (n1 n2)
	       (insert middle item (- position left-size))
	     (if (null n2)
		 (progn (setf (middle node) n1)
			(incf (size node))
			(values node nil))
		 (let ((new-node-1 (make-instance (2-node-class *tree*)))
		       (new-node-2 (make-instance (2-node-class *tree*))))
		   (setf (middle node) nil)
		   (setf (right node) nil)
		   (setf (left new-node-1) left)
		   (setf (right new-node-1) n1)
		   (setf (size new-node-1) (+ (size left) (size n1)))
		   (setf (left new-node-2) n2)
		   (setf (right new-node-2) right)
		   (setf (size new-node-1) (+ (size n2) (size right)))
		   (values new-node-1 new-node-2)))))
	  (t
	   (setf (right node) nil)
	   (multiple-value-bind (n1 n2)
	       (insert right item (- position left-size middle-size))
	     (if (null n2)
		 (progn (setf right n1)
			(incf (size node))
			(values node nil))
		 (let ((new-node-1 (make-instance (2-node-class *tree*)))
		       (new-node-2 (make-instance (2-node-class *tree*))))
		   (setf (middle node) nil)
		   (setf (right node) nil)
		   (setf (left new-node-1) left)
		   (setf (right new-node-1) middle)
		   (setf (size new-node-1) (+ (size left) (size middle)))
		   (setf (left new-node-2) n1)
		   (setf (right new-node-2) n2)
		   (setf (size new-node-2) (+ (size n1) (size n2)))
		   (values new-node-1 new-node-2))))))))

(defmethod insert ((tree tree) item position)
  (unless (<= 0 position (size tree))
    (error "Invalid position"))
  (let ((*tree* tree)
	(root (root tree)))
    (if (null root)
	(let ((leaf (make-instance (leaf-class tree))))
	  (setf (item leaf) item)
	  (setf (size leaf) 1)
	  (setf (root tree) leaf))
	(progn
	  (setf (root tree) nil)
	  (multiple-value-bind (n1 n2)
	      (insert root item position)
	    (if (null n2)
		(setf (root tree) n1)
		(let ((node (make-instance (2-node-class tree))))
		  (setf (left node) n1)
		  (setf (right node) n2)
		  (setf (size node) (+ (size n1) (size n2)))
		  (setf (root tree) node))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete an object at a particular position

(defgeneric delete (tree-or-node position))

(defmethod delete ((node leaf) position)
  (declare (ignore position))
  (values nil t))

(defmethod delete ((node 2-node) position)
  (let* ((left (left node))
	 (right (right node))
	 (left-size (size left)))
    (cond ((< position left-size)
	   ;; Make sure the left child is not referenced from any
	   ;; other node.
	   (setf (left node) nil)
	   (multiple-value-bind (n lower-p)
	       (delete left position)
	     (cond ((null n)
		    ;; The left child of NODE was a leaf, so it got
		    ;; deleted.  We need to return the right child of
		    ;; NODE, but first we need to make sure it is no
		    ;; longer referenced from NODE.
		    (setf (right node) nil)
		    (values right t))
		   ((not lower-p)
		    ;; This is the simple case where we got back a
		    ;; node with the same height as the original left
		    ;; child.  It suffices to decrement the size of
		    ;; the subtree rooted at NODE, replace the
		    ;; original left child with what we got back, and
		    ;; return the original node.
		    (decf (size node))
		    (setf (left node) n)
		    (values node nil))
		   ((typep right '2-node)
		    ;; The node N that resulted from the recursive
		    ;; call to delete of the left child is the root of
		    ;; a subtree that is lower than the original left
		    ;; child of NODE, and the right child of NODE is a
		    ;; 2-node.  The node N and the two children of the
		    ;; right child of NODE all have the same height,
		    ;; so we stick them in a new 3-node that we
		    ;; return.  The tree rooted at that 3-node is
		    ;; lower than the original tree rooted at NODE.
		    (let ((l (left right))
			  (r (right right))
			  (new-node (make-instance (3-node-class *tree*))))
		      (setf (left right) nil)
		      (setf (right right) nil)
		      (setf (left new-node) n)
		      (setf (middle new-node) l)
		      (setf (right new-node) r)
		      (values new-node t)))
		   (t
		    ;; The right child is a 3-node
		    (let ((l (left right))
			  (m (middle right))
			  (r (right right))
			  (new-node-1 (make-instance (2-node-class *tree*)))
			  (new-node-2 (make-instance (2-node-class *tree*))))
		      (setf (right node) nil)
		      (setf (left right) nil)
		      (setf (middle right) nil)
		      (setf (right right) nil)
		      (setf (left new-node-1) n)
		      (setf (right new-node-1) l)
		      (setf (size new-node-1) (+ (size n) (size l)))
		      (setf (left new-node-2) m)
		      (setf (right new-node-2) r)
		      (setf (size new-node-2) (+ (size m) (size r)))
		      (setf (left node) new-node-1)
		      (setf (right node) new-node-2)
		      (values node nil))))))
	  (t
	   ;; Make sure the right child is not referenced from any
	   ;; other node.
	   (setf (right node) nil)
	   (multiple-value-bind (n lower-p)
	       (delete left (- position left-size))
	     (cond ((null n)
		    ;; The right child of NODE was a leaf, so it got
		    ;; deleted.  We need to return the left child of
		    ;; NODE, but first we need to make sure it is no
		    ;; longer referenced from NODE.
		    (setf (left node) nil)
		    (values left t))
		   ((not lower-p)
		    ;; This is the simple case where we got back a
		    ;; node with the same height as the original right
		    ;; child.  It suffices to decrement the size of
		    ;; the subtree rooted at NODE, replace the
		    ;; original right child with what we got back, and
		    ;; return the original node.
		    (decf (size node))
		    (setf (right node) n)
		    (values node nil))
		   ((typep left '2-node)
		    ;; The node N that resulted from the recursive
		    ;; call to delete of the right child is the root
		    ;; of a subtree that is lower than the original
		    ;; right child of NODE, and the left child of NODE
		    ;; is a 2-node.  The two children of the left
		    ;; child of NODE node N and the all have the same
		    ;; height, so we stick them in a new 3-node that
		    ;; we return.  The tree rooted at that 3-node is
		    ;; lower than the original tree rooted at NODE.
		    (let ((l (left left))
			  (r (right right))
			  (new-node (make-instance (3-node-class *tree*))))
		      (setf (left left) nil)
		      (setf (right left) nil)
		      (setf (left new-node) l)
		      (setf (middle new-node) r)
		      (setf (right new-node) n)
		      (values new-node t)))
		   (t
		    ;; The left child is a 3-node
		    (let ((l (left left))
			  (m (middle left))
			  (r (right left))
			  (new-node-1 (make-instance (2-node-class *tree*)))
			  (new-node-2 (make-instance (2-node-class *tree*))))
		      (setf (left node) nil)
		      (setf (left left) nil)
		      (setf (middle left) nil)
		      (setf (right left) nil)
		      (setf (left new-node-1) l)
		      (setf (right new-node-1) m)
		      (setf (size new-node-1) (+ (size l) (size m)))
		      (setf (left new-node-2) r)
		      (setf (right new-node-2) n)
		      (setf (size new-node-2) (+ (size r) (size n)))
		      (setf (left node) new-node-1)
		      (setf (right node) new-node-2)
		      (values node nil)))))))))
	   
(defmethod delete ((node 3-node) position)
  (let* ((left (left node))
	 (middle (middle node))
	 (right (right node))
	 (left-size (size left))
	 (middle-size (size middle)))
    (cond ((< position left-size)
	   ;; Make sure the left child is not referenced from any
	   ;; other node.
	   (setf (left node) nil)
	   (multiple-value-bind (n lower-p)
	       (delete left position)
	     (cond ((null n)
		    ;; The left child of NODE was a leaf, so it got
		    ;; deleted.  We now have two children left, so we
		    ;; replace the original 3-node by a 2-node,
		    ;; holding the remaining children.
		    (let ((new-node (make-instance (2-node-class *tree*))))
		      (setf (middle node) nil)
		      (setf (right node) nil)
		      (setf (left new-node) middle)
		      (setf (right new-node) right)
		      (values new-node nil)))
		   ((not lower-p)
		    ;; This is the simple case where we got back a
		    ;; node with the same height as the original left
		    ;; child.  It suffices to decrement the size of
		    ;; the subtree rooted at NODE, replace the
		    ;; original left child with what we got back, and
		    ;; return the original node.
		    (decf (size node))
		    (setf (left node) n)
		    (values node nil))
		   ((typep middle '2-node)
		    (let ((l (left middle))
			  (r (right middle))
			  (new-node-1 (make-instance (3-node-class *tree*)))
			  (new-node-2 (make-instance (2-node-class *tree*))))
		      (setf (left middle) nil)
		      (setf (right middle) nil)
		      (setf (middle node) nil)
		      (setf (right node) nil)
		      (setf (left new-node-1) n)
		      (setf (middle new-node-1) l)
		      (setf (right new-node-1) r)
		      (setf (size new-node-1) (+ (size n) (size l) (size r)))
		      (setf (left new-node-2) new-node-1)
		      (setf (right new-node-2) right)
		      (setf (size new-node-2) (+ (size new-node-1) (size right)))
		      (values new-node-2 nil)))
		   (t
		    ;; Middle sibling is a 3-node.  We redistribute
		    ;; the return value and the three children of the
		    ;; middle sibling (4 objects in total) as the
		    ;; children of two 2-nodes.
		    (let ((l (left middle))
			  (m (middle middle))
			  (r (right middle))
			  (new-node-1 (make-instance (2-node-class *tree*)))
			  (new-node-2 (make-instance (2-node-class *tree*))))
		      (setf (middle node) nil)
		      (setf (left middle) nil)
		      (setf (middle middle) nil)
		      (setf (right middle) nil)
		      (setf (left new-node-1) n)
		      (setf (right new-node-1) l)
		      (setf (left new-node-2) m)
		      (setf (right new-node-2) r)
		      (setf (left node) new-node-1)
		      (setf (middle node) new-node-2)
		      (values node nil))))))
	  ((< position (+ left-size middle-size))
	   ;; Make sure the middle child is not referenced from any
	   ;; other node.
	   (setf (middle node) nil)
	   (multiple-value-bind (n lower-p)
	       (delete middle position)
	     (cond ((null n)
		    ;; The middle child of NODE was a leaf, so it got
		    ;; deleted.  We now have two children left, so we
		    ;; replace the original 3-node by a 2-node,
		    ;; holding the remaining children.
		    (let ((new-node (make-instance (2-node-class *tree*))))
		      (setf (left node) nil)
		      (setf (right node) nil)
		      (setf (left new-node) left)
		      (setf (right new-node) right)
		      (values new-node nil)))
		   ((not lower-p)
		    ;; This is the simple case where we got back a
		    ;; node with the same height as the original
		    ;; middle child.  It suffices to decrement the
		    ;; size of the subtree rooted at NODE, replace the
		    ;; original middle child with what we got back,
		    ;; and return the original node.
		    (decf (size node))
		    (setf (middle node) n)
		    (values node nil))
		   ((typep left '2-node)
		    ;; The node n represents a subtree that has the
		    ;; same height as the children of the 2-node in
		    ;; the left child of the 3-node NODE.  We put n
		    ;; and the children of the 2-node in a new 3-node,
		    ;; and we replace NODE by a 2-node with the new
		    ;; 3-node and the old right child of NODE as
		    ;; children.
		    (let ((l (left left))
			  (r (right left))
			  (new-node-1 (make-instance (3-node-class *tree*)))
			  (new-node-2 (make-instance (2-node-class *tree*))))
		      (setf (left left) nil)
		      (setf (right left) nil)
		      (setf (left node) nil)
		      (setf (right node) nil)
		      (setf (left new-node-1) l)
		      (setf (middle new-node-1) r)
		      (setf (right new-node-1) n)
		      (setf (size new-node-1) (+ (size l) (size r) (size n)))
		      (setf (left new-node-2) new-node-1)
		      (setf (right new-node-2) right)
		      (setf (size new-node-2) (+ (size new-node-1) (size right)))
		      (values new-node-2 nil)))
		   (t
		    ;; The node n represents a subtree that has the
		    ;; same height as the children of the 3-node in
		    ;; the left child of the 3-node NODE.  We
		    ;; redistribute the return value and the three
		    ;; children of the left sibling (4 objects in
		    ;; total) as the children of two 2-nodes.
		    (let ((l (left left))
			  (m (middle left))
			  (r (right left))
			  (new-node-1 (make-instance (2-node-class *tree*)))
			  (new-node-2 (make-instance (2-node-class *tree*))))
		      (setf (left node) nil)
		      (setf (left left) nil)
		      (setf (middle left) nil)
		      (setf (right left) nil)
		      (setf (left new-node-1) l)
		      (setf (right new-node-1) m)
		      (setf (left new-node-2) r)
		      (setf (right new-node-2) n)
		      (setf (left node) new-node-1)
		      (setf (middle node) new-node-2)
		      (values node nil))))))
	  (t
	   ;; Make sure the right child is not referenced from any
	   ;; other node.
	   (setf (right node) nil)
	   (multiple-value-bind (n lower-p)
	       (delete right position)
	     (cond ((null n)
		    ;; The right child of NODE was a leaf, so it got
		    ;; deleted.  We now have two children left, so we
		    ;; replace the original 3-node by a 2-node,
		    ;; holding the remaining children.
		    (let ((new-node (make-instance (2-node-class *tree*))))
		      (setf (left node) nil)
		      (setf (middle node) nil)
		      (setf (left new-node) left)
		      (setf (right new-node) middle)
		      (values new-node nil)))
		   ((not lower-p)
		    ;; This is the simple case where we got back a
		    ;; node with the same height as the original right
		    ;; child.  It suffices to decrement the size of
		    ;; the subtree rooted at NODE, replace the
		    ;; original right child with what we got back, and
		    ;; return the original node.
		    (decf (size node))
		    (setf (right node) n)
		    (values node nil))
		   ((typep middle '2-node)
		    ;; The node n represents a subtree that has the
		    ;; same height as the children of the 2-node in
		    ;; the middle child of the 3-node NODE.  We put n
		    ;; and the children of the 2-node in a new 3-node,
		    ;; and we replace NODE by a 2-node with the old
		    ;; left child of NODE and the new 3-node as
		    ;; children.
		    (let ((l (left middle))
			  (r (right middle))
			  (new-node-1 (make-instance (3-node-class *tree*)))
			  (new-node-2 (make-instance (2-node-class *tree*))))
		      (setf (left middle) nil)
		      (setf (right middle) nil)
		      (setf (left node) nil)
		      (setf (middle node) nil)
		      (setf (left new-node-1) l)
		      (setf (middle new-node-1) r)
		      (setf (right new-node-1) n)
		      (setf (size new-node-1) (+ (size l) (size r) (size n)))
		      (setf (right new-node-2) new-node-1)
		      (setf (left new-node-2) left)
		      (setf (size new-node-2) (+ (size new-node-1) (size right)))
		      (values new-node-2 nil)))
		   (t
		    ;; The node n represents a subtree that has the
		    ;; same height as the children of the 3-node in
		    ;; the middle child of the 3-node NODE.  We
		    ;; redistribute the return value and the three
		    ;; children of the middle sibling (4 objects in
		    ;; total) as the children of two 2-nodes.
		    (let ((l (left middle))
			  (m (middle middle))
			  (r (right middle))
			  (new-node-1 (make-instance (2-node-class *tree*)))
			  (new-node-2 (make-instance (2-node-class *tree*))))
		      (setf (middle node) nil)
		      (setf (left middle) nil)
		      (setf (middle middle) nil)
		      (setf (right middle) nil)
		      (setf (left new-node-1) l)
		      (setf (right new-node-1) m)
		      (setf (left new-node-2) r)
		      (setf (right new-node-2) n)
		      (setf (middle node) new-node-1)
		      (setf (right node) new-node-2)
		      (values node nil)))))))))

(defmethod delete ((tree tree) position)
  (unless (<= 0 position (1- (size tree)))
    (error "Invalid position"))
  (let ((root (root tree)))
    (setf (root tree) nil)
    (setf (root tree) (delete root position))))
