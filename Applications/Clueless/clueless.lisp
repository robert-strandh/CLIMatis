(cl:in-package #:clueless)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The stack of objects to inspect.  The stack is popped, so that the
;;; previous object is inspected when the key `l' is hit.

(defparameter *stack* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FIXME: add comment.

(defvar *wrap*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The text style and text color to use in order to show the printed
;;; representation of objects to be inspected.

(defparameter *inspectable-object-style*
  (clim3:text-style :free :sans :roman 12))

(defparameter *inspectable-object-color*
  (clim3:make-color 0.0 0.0 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A zone that contains the object to be inspected when clicked on.

(defclass inspectable-object-zone (clim3:pile clim3:action clim3:presentation)
  ((%background :initarg :background :reader background)))

(defmethod initialize-instance :after
    ((zone inspectable-object-zone) &key &allow-other-keys)
  (setf (clim3:depth (background zone)) 100))

(defun make-inspectable-object-zone (object)
  (let ((background (clim3:wrap)))
    (make-instance 'inspectable-object-zone
      :inside-p (constantly t)
      :gem object
      :command-name 'inspect-item
      :children 
      (list
       (clim3-text:text (let ((*print-circle* t)) (format nil "~s" object))
			*inspectable-object-style*
			*inspectable-object-color*)
       background)
      :background background)))

(defmethod clim3:highlight ((zone inspectable-object-zone))
  (let* ((color (clim3:make-color 0.5 1.0 0.5))
	 (opaque (clim3:opaque color)))
    (setf (clim3:children (background zone)) opaque)))

(defmethod clim3:unhighlight ((zone inspectable-object-zone))
  (setf (clim3:children (background zone)) '()))

(defmethod clim3:action ((zone inspectable-object-zone))
  `(inspect-item ,(clim3:gem zone)))

(clim3:define-command inspect-item (object)
  (push object *stack*)
  (setf (clim3:children *wrap*)
	(inspect-object object)))

(clim3:define-command last-item ()
  (when (not (null (cdr *stack*)))
    (pop *stack*)
    (setf (clim3:children *wrap*)
	  (inspect-object (car *stack*)))))  

(clim3:define-command quit ()
  (throw 'quit nil))

(defparameter *command-names*
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash 'inspect-item table) t)
    (setf (gethash 'last-item table) t)
    (setf (gethash 'quit table) t)
    table))
    
(defparameter *command-table*
  (make-instance 'clim3:hashed-command-table
    :command-names *command-names*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INSPECT-OBJECT.
;;;
;;; Depending on the object to be inspected, this function returns
;;; some zone tree that will be made the child of the WRAP zone that
;;; is the value of the variable *wrap*.

(defgeneric inspect-object (object))

(defmethod inspect-object ((object t))
  (let ((color (clim3:make-color 0.0 0.0 0.0))
	(style (clim3:text-style :free :sans :roman 12)))
    (clim3:hbox*
     (clim3:hbrick
      100 (clim3-text:text "Class:" style color))
     (make-inspectable-object-zone (class-of object)))))

(defmethod inspect-object ((object symbol))
  (let ((color (clim3:make-color 0.0 0.0 0.0))
	(style (clim3:text-style :free :sans :roman 12)))
    (clim3:vbox*
     (clim3:hbox*
      (clim3:hbrick
       100 (clim3-text:text "Class:" style color))
      (make-inspectable-object-zone (class-of object))
      (clim3:sponge))
     (clim3:vbrick 5)
     (clim3:hbox*
      (clim3:hbrick
       100 (clim3-text:text "Name:" style color))
      (make-inspectable-object-zone (symbol-name object))
      (clim3:sponge))
     (clim3:vbrick 5)
     (clim3:hbox*
      (clim3:hbrick
       100 (clim3-text:text "Package:" style color))
      (make-inspectable-object-zone (symbol-package object))
      (clim3:sponge))
     (clim3:sponge))))

(defmethod inspect-object ((object cons))
  (let ((color (clim3:make-color 0.0 0.0 0.0))
	(style (clim3:text-style :free :sans :roman 12)))
    (clim3:vbox*
     (clim3:hbox*
      (clim3:hbrick
       100 (clim3-text:text "Class:" style color))
      (make-inspectable-object-zone (class-of object))
      (clim3:sponge))
     (clim3:vbrick 5)
     (clim3:hbox*
      (clim3-text:text "Elements: " style color)
      (clim3:sponge))
     (clim3:vbox
      (loop for element in object
	    collect
	    (clim3:hbox*
	     (make-inspectable-object-zone element)
	     (clim3:sponge)))))))

(defmethod inspect-object ((object standard-object))
  (let ((color (clim3:make-color 0.0 0.0 0.0))
	(style (clim3:text-style :free :sans :roman 12)))
    (clim3:vbox*
     (clim3:hbox*
      (clim3:hbrick
       100 (clim3-text:text "Class:" style color))
      (make-inspectable-object-zone (class-of object))
      (clim3:sponge))
     (clim3:vbrick 5)
     (clim3:vbox
      (loop for slot in (sb-mop:class-slots (class-of object))
	    collect
	    (clim3:hbox*
	     (make-inspectable-object-zone
	      (slot-value slot 'sb-pcl::name))
	     (clim3:hbrick 20)
	     (make-inspectable-object-zone
	      (slot-value object (slot-value slot 'sb-pcl::name)))
	     (clim3:sponge)))))))

;;; This is a simple application so this class plays the role
;;; of both an application and a view.
(defclass clueless (clim3:application clim3:view)
  ())

(defmethod clim3:command-table ((view clueless))
  *command-table*)

(defmethod clim3:current-view ((application clueless))
  application)

(defun inspect (object)
  (let* ((wrap (clim3:wrap))
	 (scroll (clim3:scroll wrap))
	 (*wrap* wrap)
	 (color (clim3:make-color 1.0 1.0 1.0))
	 (background (clim3:opaque color))
	 (size (clim3:brick 800 500))
	 (root (clim3:pile* scroll size background))
	 (port (clim3:make-port :clx-framebuffer)))
    (setf *stack* (list object))
    (setf (clim3:children *wrap*)
	  (inspect-object object))
    (setf (clim3:depth background) 100)
    (clim3:connect root port)
    (catch 'quit
      (let ((clim3:*port* port)
	    (clim3:*application* (make-instance 'clueless))
	    (clim3:*key-handler*
	      (make-instance 'clim3-port::read-keystroke-key-handler
		:receiver
		(lambda (keystroke)
		  (when (eq clim3-ext:*input-context* 'nil)
		    (cond ((equal keystroke '(#\q))
			   (throw :accept 'quit))
			  ((equal keystroke '(#\l))
			   (throw :accept 'last-item))))))))
	(clim3:command-loop)))
    (clim3:disconnect root port)))
