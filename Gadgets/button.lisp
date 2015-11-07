(cl:in-package #:clim3-gadgets)

(defclass button (clim3:pile clim3:action)
  ((%action :initarg :action :reader clim3:action)
   (%highlight-wrap :accessor highlight-wrap)))

;;; BUTTON (subclass of PILE)
;;;   BORDER (same thickness as the raise border)
;;;     PILE
;;;       CENTER
;;;         BORDER (surrounding label)
;;;           label
;;;       WRAP (for highlighting)
;;;   RAISE
;;;   BASE

(defmethod initialize-instance :after
    ((gadget button) &key label &allow-other-keys)
  (let* ((base-color (clim3-gadgets:foreground-color clim3-gadgets:*theme*))
	 (base (clim3:opaque base-color))
	 (raise (clim3-gadgets:raise))
	 (wrap (clim3:wrap))
	 (t1 (clim3-gadgets:icon-border-thickness clim3-gadgets:*theme*))
	 (b1 (clim3:border t1 label))
	 (c (clim3:center b1))
	 (p1 (clim3:pile* c wrap))
	 (t2 (clim3-gadgets:raise-border-thickness clim3-gadgets:*theme*))
	 (b2 (clim3:border t2 p1)))
    (setf (clim3:depth base) 100)
    (setf (clim3:depth wrap) 100)
    (setf (highlight-wrap gadget) wrap)
    (setf (clim3:children gadget)
	  (list b2 raise base))))

(defmethod clim3:highlight ((zone button))
  (let ((color (clim3-gadgets:highlight-color clim3-gadgets:*theme*))
	(opacity (clim3-gadgets:highlight-opacity clim3-gadgets:*theme*)))
    (setf (clim3:children (highlight-wrap zone))
	  (clim3:translucent color opacity))))

(defmethod clim3:unhighlight ((zone button))
  (setf (clim3:children (highlight-wrap zone))
	nil))

(defun button (label action)
  (make-instance 'button
    :label label
    :action action
    :command-name (if (symbolp action) action (car action))))
