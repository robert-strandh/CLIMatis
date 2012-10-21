(defun slider ()
  (let* ((background-color (clim3-color:make-color 0.0 1.0 0.0))
	 (background (clim3-graphics:opaque background-color))
	 (button-color (clim3-color:make-color 1.0 0.0 0.0))
	 (button-background (clim3-graphics:opaque button-color))
	 (pile nil)
	 (pointer-vpos 0)
	 (button-vpos nil)
	 (motion-handler (lambda (zone hpos vpos)
			   (declare (ignore zone hpos))
			   (format t "motion ~s~%" vpos)
			   (if (null button-vpos)
			       (setf pointer-vpos vpos)
			       (setf (clim3-zone:vpos pile)
				     (max 10 (min 90 (- vpos button-vpos)))))))
	 (press-handler (lambda (zone code modifiers)
			  (declare (ignore zone code modifiers))
			  (format t "press~%")
			  (setf button-vpos pointer-vpos)))
	 (release-handler (lambda (zone code modifiers)
			    (declare (ignore zone code modifiers))
			    (format t "release~%")
			    (setf button-vpos nil)))
	 (button-button (clim3-input:button press-handler release-handler)))
    (setf (clim3-zone:depth button-background) 10)
    (setf pile
	  (clim3-layout:hframe*
	   40 40 40
	   (clim3-layout:vframe*
	    20 20 20
	    (clim3-layout:pile* button-button button-background))))
    (setf (clim3-zone:hpos pile) 0)
    (setf (clim3-zone:vpos pile) 80)
    (clim3-layout:hframe*
     40 40 40
     (clim3-layout:vframe*
      100 100 100
      (clim3-layout:pile*
       (clim3-input:motion motion-handler)
       (clim3-layout:bboard* pile)
       background)))))
	
       