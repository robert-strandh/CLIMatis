(defparameter *z7*
    (let (top
	  (blue (clim3:brick*
		 50 50
		 (clim3:opaque (clim3:make-color 0.0 0.0 1.0)))))
      (setf top
	    (clim3:hbox*
	     (clim3:pile*
	      (clim3-input:button
	       (lambda (zone code modifiers)
		 (declare (ignore zone code modifiers))
		 (let ((hbox-children (clim3:children top)))
		   (if (= 3 (length hbox-children))
		       (format *debug-io* "blue zone already present~%")
		       (setf (clim3:children top)
			     (append hbox-children (list blue))))))
	       (lambda (zone code modifiers)
		 (declare (ignore zone code modifiers))
		 nil))
	      (clim3:brick 50 50)
	      (clim3:opaque (clim3:make-color 1.0 0.0 0.0)))
	     (clim3:pile*
	      (clim3-input:button
	       (lambda (zone code modifiers)
		 (declare (ignore zone code modifiers))
		 (let ((hbox-children (clim3:children top)))
		   (if (= 2 (length hbox-children))
		       (format *debug-io* "blue zone not present~%")
		       (setf (clim3:children top)
			     (butlast hbox-children)))))
	       (lambda (zone code modifiers)
		 (declare (ignore zone code modifiers))
		 nil))
	      (clim3:brick 50 50)
	      (clim3:opaque (clim3:make-color 0.0 1.0 0.0)))))
      top))
  
(defun test-zone (zone)
  (let ((port (clim3:make-port :clx-framebuffer)))
    (clim3:connect zone port)
    (clim3:event-loop port)))
