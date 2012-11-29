(in-package #:common-lisp-user)

(defparameter *z4*
  (let* ((red (clim3-color:make-color 1.0 0.0 0.0))
	 (blue (clim3-color:make-color 0.0 0.0 1.0))
	 (red-zone (clim3-graphics:opaque red)))
    (clim3-layout:hbox*
     (clim3-layout:brick
      100 100
      (clim3-layout:pile*
       (clim3-input:visit
	(lambda (zone) (setf (clim3-graphics:color red-zone) red))
	(lambda (zone) (setf (clim3-graphics:color red-zone) blue)))
       red-zone))
     (clim3-layout:brick
      70 100
      (clim3-graphics:opaque (clim3-color:make-color 0.0 1.0 0.0))))))

(defun test-zone (zone)
  (let ((port (clim3-port:make-port :clx-framebuffer)))
    (clim3-port:connect zone port)
    (clim3-port:event-loop port)))
