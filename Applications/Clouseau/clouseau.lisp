(in-package #:clouseau)

(defparameter *objects-to-inspect* nil) 
(defparameter *objects-on-display* nil)
(defparameter *inspect-fun* nil)
(defparameter *uninspect-fun* nil)

(defgeneric inspect-object (object))

(defmethod inspect-object (object)
  (let* ((color (clim3-color:make-color 0.0 1.0 0.0))
	 (opaque (clim3-graphics:opaque color)))
    (clim3-layout:brick 10 10 opaque)))

(defmethod inspect-object ((object number))
  (clim3-text:text (format nil "~s" object)
		   nil
		   (clim3-color:make-color 0.0 0.0 0.0)))

(defmethod inspect-object ((object symbol))
  (clim3-text:text (format nil "~s" object)
		   nil
		   (clim3-color:make-color 0.0 0.0 0.0)))

(defmethod inspect-object ((object string))
  (clim3-text:text (format nil "~s" object)
		   nil
		   (clim3-color:make-color 0.0 0.0 0.0)))

(defmethod inspect-object ((object cons))
  (clim3-layout:hbox*
   (clim3-layout:vbox*
;;    (clim3-layout:brick
;;     40 20
;;     (clim3-graphics:opaque (clim3-color:make-color 0.5 0.5 0.5)))
    (cons-cell (clim3-color:make-color 0.6 0.2 0.2))
    (inspect-object (car object))
    (clim3-layout:sponge))
   (inspect-object (cdr object))))

(defmethod inspect-object :around (object)
  (cond ((gethash object *objects-on-display*)
	 (let* ((color (clim3-color:make-color 1.0 0.0 0.0))
		(opaque (clim3-graphics:opaque color)))
	   (clim3-layout:vbox*
	    (clim3-layout:brick 10 10 opaque)
	    (clim3-layout:sponge))))
	((not (gethash object *objects-to-inspect*))
	 (let* ((color (clim3-color:make-color 0.0 0.0 1.0))
		(opaque (clim3-graphics:opaque color))
		(fun *inspect-fun*))
	   (clim3-layout:vbox*
	    (clim3-layout:pile*
	     (clim3-input:button
	      (lambda (&rest arguments)
		(declare (ignore arguments))
		(funcall fun object))
	      (lambda (&rest rest) (declare (ignore rest)) nil))
	     (clim3-layout:brick 10 10 opaque))
	    (clim3-layout:sponge))))
	(t (setf (gethash object *objects-on-display*) t)
	   (call-next-method))))

(defun inspect (obj)
  (let* ((objects-to-inspect (make-hash-table :test #'eq))
	 (port (clim3-port:make-port :clx-framebuffer))
	 (wrap (clim3-layout:wrap))
	 (root (clim3-layout:pile*
		wrap
		(clim3-graphics:opaque (clim3-color:make-color 0.9 0.9 0.9)))))
    (labels ((redisplay ()
	       (let ((*objects-to-inspect* objects-to-inspect)
		     (*objects-on-display* (make-hash-table :test #'eq))
		     (*inspect-fun* (lambda (obj)
				      (setf (gethash obj objects-to-inspect)
					    t)
				      (redisplay))))
		 (setf (clim3-zone:children wrap)
		       (inspect-object obj)))))
      (setf (gethash obj objects-to-inspect) t)
      (redisplay)
      (clim3-port:connect root port)
      (clim3-clx-framebuffer::event-loop port))))
