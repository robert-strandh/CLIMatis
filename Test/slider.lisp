(defun slider ()
  (let* ((slider-height 200)
	 (slider-width 50)
	 (button-height 30)
	 (max-pos (- slider-height button-height))
	 (background-color (clim3:make-color 0.0 1.0 0.0))
	 (background (clim3:opaque background-color))
	 (button-color (clim3:make-color 1.0 0.0 0.0))
	 (button-background (clim3:opaque button-color))
	 (pile nil)
	 (pointer-vpos 0)
	 (pos-diff nil)
	 (motion-handler (lambda (zone hpos vpos)
			   (declare (ignore zone hpos))
			   (if (null pos-diff)
			       (setf pointer-vpos vpos)
			       (setf (clim3:vpos pile)
				     (max 0 (min max-pos (+ vpos pos-diff)))))))
	 (press-handler (lambda (zone code modifiers)
			  (declare (ignore zone code modifiers))
			  (setf pos-diff (- (clim3:vpos pile) pointer-vpos))))
	 (release-handler (lambda (zone code modifiers)
			    (declare (ignore zone code modifiers))
			    (setf pos-diff nil)))
	 (button-button (clim3-input:button press-handler release-handler)))
    (setf (clim3:depth background) 2)
    (setf (clim3:depth button-background) 10)
    (setf pile
	  (clim3:brick
	   slider-width button-height
	   (clim3:pile* button-button button-background)))
    (setf (clim3:hpos pile) 0)
    (setf (clim3:vpos pile) max-pos)
    (clim3:brick
     slider-width slider-height
     (clim3:pile*
      (clim3:motion motion-handler)
      (clim3:bboard* pile)
      background))))
	
       
