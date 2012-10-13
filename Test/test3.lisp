(in-package #:common-lisp-user)

(defparameter *z3*
  (clim3-layout:hbox*
   (clim3-layout:brick*
    100 100
    (clim3-graphics:opaque (clim3-color:make-color 1.0 0.0 0.0)))
   (clim3-layout:brick*
    70 100
    (clim3-graphics:opaque (clim3-color:make-color 0.0 1.0 0.0)))))

(defparameter *z3b*
  (clim3-layout:hbox*
   (clim3-graphics:opaque (clim3-color:make-color 1.0 0.0 0.0))
   (clim3-graphics:opaque (clim3-color:make-color 0.0 1.0 0.0))
   (clim3-layout:brick* 10 10)))

(defun test-zone (zone)
  (let ((port (clim3-port:make-port :clx-framebuffer)))
    (clim3-port:connect zone port)
    (clim3-clx-framebuffer::event-loop port)))
