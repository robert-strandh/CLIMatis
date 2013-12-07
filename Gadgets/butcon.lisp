(cl:in-package #:clim3-gadgets)

(defclass butcon (clim3:pile clim3:action)
  ((%action :initarg :action :reader clim3:action)
   (%highlight-wrap :accessor highlight-wrap)))
  

;;; BUTCON (subclass of PILE)
;;;   CENTER  
;;;     BORDER (surrounding label)
;;;       label
;;;   WRAP (for highlighting)
;;;   BASE

(defmethod initialize-instance :after
    ((gadget butcon) &key label &allow-other-keys)
  (let* ((base-color (clim3-gadgets:foreground-color clim3-gadgets:*theme*))
	 (base (clim3:opaque base-color))
	 (wrap (clim3:wrap))
	 (tt (clim3-gadgets:raise-border-thickness clim3-gadgets:*theme*))
	 (b (clim3:border tt label))
	 (c (clim3:center b)))
    (setf (clim3:depth base) 100)
    (setf (clim3:depth wrap) 100)
    (setf (highlight-wrap gadget) wrap)
    (setf (clim3:children gadget)
	  (list c wrap base))))

(defmethod clim3:highlight ((zone butcon))
  (let ((color (clim3-gadgets:highlight-color clim3-gadgets:*theme*))
	(opacity (clim3-gadgets:highlight-opacity clim3-gadgets:*theme*)))
    (setf (clim3:children (highlight-wrap zone))
	  (clim3:translucent color opacity))))

(defmethod clim3:unhighlight ((zone butcon))
  (setf (clim3:children (highlight-wrap zone))
	nil))

(defun butcon (label action)
  (make-instance 'butcon
    :label label
    :action action
    :command-name (if (symbolp action) action (car action))))
