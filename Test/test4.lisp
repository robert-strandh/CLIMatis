(in-package #:common-lisp-user)

(defparameter *z4*
  (let* ((red (clim3:make-color 1.0 0.0 0.0))
	 (blue (clim3:make-color 0.0 0.0 1.0))
	 (red-zone (clim3:opaque red)))
    (clim3:hbox*
     (clim3:brick
      100 100
      (clim3:pile*
       (clim3-input:visit
	(lambda (zone) (setf (clim3:color red-zone) red))
	(lambda (zone) (setf (clim3:color red-zone) blue)))
       red-zone))
     (clim3:brick
      70 100
      (clim3:opaque (clim3:make-color 0.0 1.0 0.0))))))

(defun test-zone (zone)
  (let ((port (clim3-port:make-port :clx-framebuffer)))
    (clim3-port:connect zone port)
    (clim3-port:event-loop port)))
