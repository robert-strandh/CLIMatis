(in-package #:common-lisp-user)

(defparameter *z3*
  (clim3:hbox*
   (clim3:brick
    100 100
    (clim3:opaque (clim3:make-color 1.0 0.0 0.0)))
   (clim3:brick
    70 100
    (clim3:opaque (clim3:make-color 0.0 1.0 0.0)))))

(defparameter *z3b*
  (clim3:hbox*
   (clim3:opaque (clim3:make-color 1.0 0.0 0.0))
   (clim3:opaque (clim3:make-color 0.0 1.0 0.0))
   (clim3:brick 10 10)))

(defun test-zone (zone)
  (let ((port (clim3:make-port :clx-framebuffer)))
    (clim3:connect zone port)
    (clim3:event-loop port)))
