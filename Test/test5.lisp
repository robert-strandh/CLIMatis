(in-package #:common-lisp-user)

(defparameter *z5*
  (let* ((red (clim3-color:make-color 1.0 0.0 0.0))
	 (blue (clim3-color:make-color 0.0 0.0 1.0))
	 (black (clim3-color:make-color 0.0 0.0 0.0))
	 (white (clim3-color:make-color 1.0 1.0 1.0))
	 (red-zone (clim3-graphics:opaque red))
	 (z1 (clim3-graphics:opaque black))
	 (z2 (clim3-graphics:opaque white)))
    (flet ((make-gray (x)
	     (clim3-color:make-color x x x)))
      (clim3-layout:hbox*
       (clim3-layout:brick*
	100 100
	(clim3-layout:pile*
	 (clim3-input:leave (lambda (zone) (setf (clim3-graphics:color red-zone) blue)))
	 (clim3-input:enter (lambda (zone) (setf (clim3-graphics:color red-zone) red)))
	 (clim3-input:motion (lambda (zone hpos vpos)
			       (setf (clim3-graphics:color z1)
				     (make-gray (max 0.0 (min 1.0 (/ hpos 100.0)))))
			       (setf (clim3-graphics:color z2)
				     (make-gray (max 0.0 (min 1.0 (/ vpos 100.0)))))))
	 red-zone))
       (clim3-layout:brick*
	70 100
	(clim3-graphics:opaque (clim3-color:make-color 0.0 1.0 0.0)))
       (clim3-layout:brick* 20 100 z1)
       (clim3-layout:brick* 20 100 z2)))))

(defun test-zone (zone)
  (let ((port (clim3-port:make-port :clx-framebuffer)))
    (clim3-port:connect zone port)
    (clim3-clx-framebuffer::event-loop port)))
