(in-package #:clouseau)

(defparameter *objects-to-inspect* nil) 
(defparameter *objects-on-display* nil)
(defparameter *inspect-fun* nil)
(defparameter *uninspect-fun* nil)

(defgeneric inspect-object (object))

(defmethod inspect-object (object)
  (let* ((color (clim3:make-color 0.0 1.0 0.0))
	 (opaque (clim3:opaque color)))
    (clim3:brick 10 10 opaque)))

(defmethod inspect-object ((object number))
  (clim3-text:text (format nil "~s" object)
		   (clim3-text-style:text-style :free :sans :roman 12)
		   (clim3:make-color 0.0 0.0 0.0)))

(defmethod inspect-object ((object symbol))
  (clim3-text:text (format nil "~s" object)
		   (clim3-text-style:text-style :free :sans :roman 12)
		   (clim3:make-color 0.0 0.0 0.0)))

(defmethod inspect-object ((object string))
  (clim3-text:text (format nil "~s" object)
		   (clim3-text-style:text-style :free :sans :roman 12)
		   (clim3:make-color 0.0 0.0 0.0)))

(defmethod inspect-object ((object cons))
  (clim3:hbox*
   (clim3:vbox*
    (cons-cell (clim3:make-color 0.6 0.2 0.2))
    (inspect-object (car object))
    (clim3:sponge))
   (inspect-object (cdr object))))

(defmethod inspect-object :around (object)
  (cond ((gethash object *objects-on-display*)
	 (let* ((color (clim3:make-color 1.0 0.0 0.0))
		(opaque (clim3:opaque color)))
	   (clim3:vbox*
	    (clim3:brick 10 10 opaque)
	    (clim3:sponge))))
	((not (gethash object *objects-to-inspect*))
	 (let* ((color (clim3:make-color 0.0 0.0 1.0))
		(opaque (clim3:opaque color))
		(fun *inspect-fun*))
	   (clim3:vbox*
	    (clim3:pile*
	     (clim3-input:button
	      (lambda (&rest arguments)
		(declare (ignore arguments))
		(funcall fun object))
	      (lambda (&rest rest) (declare (ignore rest)) nil))
	     (clim3:brick 10 10 opaque))
	    (clim3:sponge))))
	(t (setf (gethash object *objects-on-display*) t)
	   (call-next-method))))

(defun inspect (obj)
  (let* ((objects-to-inspect (make-hash-table :test #'eq))
	 (port (clim3-port:make-port :clx-framebuffer))
	 (wrap (clim3:wrap))
	 (root (clim3:pile*
		wrap
		(clim3:opaque (clim3:make-color 0.9 0.9 0.9)))))
    (labels ((redisplay ()
	       (let ((*objects-to-inspect* objects-to-inspect)
		     (*objects-on-display* (make-hash-table :test #'eq))
		     (*inspect-fun* (lambda (obj)
				      (setf (gethash obj objects-to-inspect)
					    t)
				      (redisplay))))
		 (setf (clim3:children wrap)
		       (inspect-object obj)))))
      (setf (gethash obj objects-to-inspect) t)
      (redisplay)
      (clim3-port:connect root port)
      (let ((clim3-port:*new-port* port))
	(loop for keystroke = (clim3-port:read-keystroke)
	      until (eql (car keystroke) #\q))))))
