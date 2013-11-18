(in-package #:common-lisp-user)

(defparameter *z2*
  (clim3:hbox*
   (clim3:brick
    200 200
    (clim3:pile*
     (clim3:opaque (clim3:make-color 1.0 0.0 0.0))
     (clim3-input:button
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "red: button press: ~a ~a~%" code modifiers)
	(force-output *debug-io*))
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "red: button release: ~a ~a~%" code modifiers)
	(force-output *debug-io*)))
     (clim3:visit
      (lambda (zone)
	(declare (ignore zone))
	(format *debug-io* "red: enter~%")
	(force-output *debug-io*))
      (lambda (zone)
	(declare (ignore zone))
	(format *debug-io* "red: leave~%")
	(force-output *debug-io*))
      (lambda (hpos vpos)
	(< (+ (expt (- hpos 100) 2) (expt (- vpos 100) 2))
	   (expt 50 2))))
     (clim3:motion
      (lambda (zone hpos vpos)
	(declare (ignore zone))
	(format *debug-io* "red: motion ~a ~a~%" hpos vpos)
	(force-output *debug-io*)))))
   (clim3:brick
    200 200
    (clim3:pile*
     (clim3:opaque (clim3:make-color 0.0 1.0 0.0))
     (clim3-input:button
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "green: button press: ~a ~a~%" code modifiers)
	(force-output *debug-io*))
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "green: button release: ~a ~a~%" code modifiers)
	(force-output *debug-io*)))
     (clim3:visit
      (lambda (zone)
	(declare (ignore zone))
	(format *debug-io* "green: enter~%")
	(force-output *debug-io*))
      (lambda (zone)
	(declare (ignore zone))
	(format *debug-io* "green: leave~%")
	(force-output *debug-io*)))
     (clim3:motion
      (lambda (zone hpos vpos)
	(declare (ignore zone))
	(format *debug-io* "green: motion ~a ~a~%" hpos vpos)
	(force-output *debug-io*)))))))

(defun test2 ()
  (let ((port (clim3:make-port :clx-framebuffer)))
    (clim3:connect *z2* port)
    (clim3:event-loop port)))
