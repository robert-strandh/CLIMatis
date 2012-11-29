(in-package #:common-lisp-user)

(defparameter *z2*
  (clim3-layout:hbox*
   (clim3-layout:brick*
    200 200
    (clim3-layout:pile*
     (clim3-graphics:opaque (clim3-color:make-color 1.0 0.0 0.0))
     (clim3-input:key-press
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "red: key press: ~a ~a~%" code modifiers)
	(force-output *debug-io*)))
     (clim3-input:key-release
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "red: key release: ~a ~a~%" code modifiers)
	(force-output *debug-io*)))
     (clim3-input:button
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "red: button press: ~a ~a~%" code modifiers)
	(force-output *debug-io*))
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "red: button release: ~a ~a~%" code modifiers)
	(force-output *debug-io*)))
     (clim3-input:visit
      (lambda (zone)
	(declare (ignore zone))
	(format *debug-io* "red: enter~%")
	(force-output *debug-io*))
      (lambda (zone)
	(declare (ignore zone))
	(format *debug-io* "red: leave~%")
	(force-output *debug-io*)))
     (clim3-input:motion
      (lambda (zone hpos vpos)
	(declare (ignore zone))
	(format *debug-io* "red: motion ~a ~a~%" hpos vpos)
	(force-output *debug-io*)))))
   (clim3-layout:brick*
    200 200
    (clim3-layout:pile*
     (clim3-graphics:opaque (clim3-color:make-color 0.0 1.0 0.0))
     (clim3-input:key-press
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "green: key press: ~a ~a~%" code modifiers)
	(force-output *debug-io*)))
     (clim3-input:key-release
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "green: key release: ~a ~a~%" code modifiers)
	(force-output *debug-io*)))
     (clim3-input:button
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "green: button press: ~a ~a~%" code modifiers)
	(force-output *debug-io*))
      (lambda (zone code modifiers)
	(declare (ignore zone))
	(format *debug-io* "green: button release: ~a ~a~%" code modifiers)
	(force-output *debug-io*)))
     (clim3-input:visit
      (lambda (zone)
	(declare (ignore zone))
	(format *debug-io* "green: enter~%")
	(force-output *debug-io*))
      (lambda (zone)
	(declare (ignore zone))
	(format *debug-io* "green: leave~%")
	(force-output *debug-io*)))
     (clim3-input:motion
      (lambda (zone hpos vpos)
	(declare (ignore zone))
	(format *debug-io* "green: motion ~a ~a~%" hpos vpos)
	(force-output *debug-io*)))))))

(defun test2 ()
  (let ((port (clim3-port:make-port :clx-framebuffer)))
    (clim3-port:connect *z2* port)
    (clim3-port:event-loop port)))
