(defpackage #:clueless
  (:use #:common-lisp)
  (:shadow #:inspect)
  (:export #:inspect))

(in-package #:clueless)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The stack of objects to inspect.  The stack is popped, so that the
;;; previous object is inspected when the key `l' is hit.

(defparameter *stack* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The text style and text color to use in order to show the printed
;;; representation of objects to be inspected.

(defparameter *inspectable-object-style*
  (clim3-text-style:text-style :free :sans :roman 12))

(defparameter *inspectable-object-color*
  (clim3:make-color 0.0 0.0 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This value of this variable is NIL most of the time.  When the
;;; pointer is inside a zone containing an object to inspect, this
;;; variable is set to a list containing that object as its only
;;; element, so that the key-press handler sees it.  When the pointer
;;; leaves the zone, this variable is set to NIL again.

(defvar *object-to-inspect*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A zone that contains the object to be inspected when clicked on.
;;;
;;; We show a method for building such a zone without using
;;; subclassing.  The top-zone is a PILE containing three other zones:
;;; The VISIT input zone that handles highlighting and making the
;;; object clickable, the TEXT containing the printed representation
;;; of the object, and a WRAP zone.  The WRAP zone has no child when
;;; the the pointer is outside the VISIT zone, so that there is no
;;; highlighting then, and when the pointer is inside the VISIT zone,
;;; then an OPAQUE zone is made the child of the WRAP zone.

(defun make-inspectable-object-zone (object)
  (let* ((highlighted-color (clim3:make-color 0.5 1.0 0.5))
	 (highlighted-zone (clim3:opaque highlighted-color))
	 (highlighted-wrap (clim3:wrap)))
    ;; We want to make sure that the background is always in the
    ;; well, background. 
    (setf (clim3:depth highlighted-wrap) 100)
    (flet ((highlight ()
	     (setf (clim3:children highlighted-wrap)
		   highlighted-zone))
	   (unhighlight ()
	     (setf (clim3:children highlighted-wrap)
		   nil)))
      (flet ((enter-handler (zone)
	       (declare (ignore zone))
	       (highlight)
	       (setf *object-to-inspect* (list object)))
	     (leave-handler (zone)
	       (declare (ignore zone))
	       (unhighlight)
	       (setf *object-to-inspect* nil)))
	(clim3:pile*
	 (clim3:visit #'enter-handler #'leave-handler)
	 (clim3-text:text (let ((*print-circle* t)) (format nil "~s" object))
			  *inspectable-object-style*
			  *inspectable-object-color*)
	 highlighted-wrap)))))
      
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
	(style (clim3-text-style:text-style :free :sans :roman 12)))
    (clim3:hbox*
     (clim3:hbrick
      100 (clim3-text:text "Class:" style color))
     (make-inspectable-object-zone (class-of object)))))

(defmethod inspect-object ((object symbol))
  (let ((color (clim3:make-color 0.0 0.0 0.0))
	(style (clim3-text-style:text-style :free :sans :roman 12)))
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
	(style (clim3-text-style:text-style :free :sans :roman 12)))
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
	(style (clim3-text-style:text-style :free :sans :roman 12)))
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

(defvar *wrap*)

(defclass inspect-button-handler (clim3:button-handler) ())

(defmethod clim3:handle-button-press
    ((handler inspect-button-handler) button-code modifiers)
  (declare (ignore button-code modifiers))
  (unless (null *object-to-inspect*)
    (push (car *object-to-inspect*) *stack*)
    (setf (clim3:children *wrap*)
	  (inspect-object (car *object-to-inspect*)))))
  
(defmethod clim3:handle-button-release
    ((handler inspect-button-handler) button-code modifiers)
  (declare (ignore button-code modifiers))
  nil)

(defun inspect (object)
  (let* ((wrap (clim3:wrap))
	 (scroll (clim3:scroll wrap))
	 (*wrap* wrap)
	 (*object-to-inspect* '())
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
    (let ((clim3:*key-handler*
	    (make-instance 'clim3-port::read-keystroke-key-handler
	      :receiver
	      (lambda (keystroke)
		;; Why can keystroke be NIL?
		(cond ((equal keystroke '(#\q))
		       (clim3:disconnect root port)
		       (return-from inspect nil))
		      ((equal keystroke '(#\i))
		       (inspect root))
		      ((and (equal keystroke '(#\l))
			    (not (null (cdr *stack*))))
		       (pop *stack*)
		       (setf (clim3:children *wrap*)
			     (inspect-object (car *stack*))))))))
	  (clim3:*button-handler*
	    (make-instance 'inspect-button-handler)))
      (clim3:event-loop port))))


