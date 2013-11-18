(in-package #:common-lisp-user)

(defparameter *z5*
  (let* ((red (clim3:make-color 1.0 0.0 0.0))
	 (blue (clim3:make-color 0.0 0.0 1.0))
	 (black (clim3:make-color 0.0 0.0 0.0))
	 (white (clim3:make-color 1.0 1.0 1.0))
	 (red-zone (clim3:opaque red))
	 (z1 (clim3:opaque black))
	 (z2 (clim3:opaque white)))
    (flet ((make-gray (x)
	     (clim3:make-color x x x)))
      (clim3:hbox*
       (clim3:brick
	100 100
	(clim3:pile*
	 (clim3:visit (lambda (zone) (setf (clim3:color red-zone) red))
			    (lambda (zone) (setf (clim3:color red-zone) blue)))
	 (clim3:motion (lambda (zone hpos vpos)
			       (setf (clim3:color z1)
				     (make-gray (max 0.0 (min 1.0 (/ hpos 100.0)))))
			       (setf (clim3:color z2)
				     (make-gray (max 0.0 (min 1.0 (/ vpos 100.0)))))))
	 red-zone))
       (clim3:brick
	70 100
	(clim3:opaque (clim3:make-color 0.0 1.0 0.0)))
       (clim3:brick 20 100 z1)
       (clim3:brick 20 100 z2)))))

(defun test-zone (zone)
  (let ((port (clim3-port:make-port :clx-framebuffer)))
    (clim3-port:connect zone port)
    (clim3-port:event-loop port)))
