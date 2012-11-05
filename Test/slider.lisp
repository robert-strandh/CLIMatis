(defun slider ()
  (let* ((slider-height 200)
	 (slider-width 50)
	 (button-height 30)
	 (max-pos (- slider-height button-height))
	 (background-color (clim3-color:make-color 0.0 1.0 0.0))
	 (background (clim3-graphics:opaque background-color))
	 (button-color (clim3-color:make-color 1.0 0.0 0.0))
	 (button-background (clim3-graphics:opaque button-color))
	 (pile nil)
	 (pointer-vpos 0)
	 (pos-diff nil)
	 (motion-handler (lambda (zone hpos vpos)
			   (declare (ignore zone hpos))
			   (if (null pos-diff)
			       (setf pointer-vpos vpos)
			       (setf (clim3-zone:vpos pile)
				     (max 00 (min max-pos (+ vpos pos-diff)))))))
	 (press-handler (lambda (zone code modifiers)
			  (declare (ignore zone code modifiers))
			  (setf pos-diff (- (clim3-zone:vpos pile) pointer-vpos))))
	 (release-handler (lambda (zone code modifiers)
			    (declare (ignore zone code modifiers))
			    (setf pos-diff nil)))
	 (button-button (clim3-input:button press-handler release-handler)))
    (setf (clim3-zone:depth background) 2)
    (setf (clim3-zone:depth button-background) 10)
    (setf pile
	  (clim3-layout:brick
	   slider-width button-height
	   (clim3-layout:pile* button-button button-background)))
    (setf (clim3-zone:hpos pile) 0)
    (setf (clim3-zone:vpos pile) max-pos)
    (clim3-layout:brick
     slider-width slider-height
     (clim3-layout:pile*
      (clim3-input:motion motion-handler)
      (clim3-layout:bboard* pile)
      background))))
	
       