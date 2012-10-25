(in-package #:clim3-graphics-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create a backend for testing.

(defclass test-port (clim3-port:port)
  ((%root :initform nil :accessor root)))

(defmethod clim3-port:connect (zone (port test-port))
  (setf (root port) zone)
  (setf (clim3-zone:parent zone) port))

(defun update (port)
  (clim3-zone:ensure-hsprawl-valid (root port))
  (clim3-zone:ensure-vsprawl-valid (root port))
  (multiple-value-bind (width height)
      (clim3-zone:natural-size (root port))
    ;; This port imposes a size slightly bigger than the natural one.
    (clim3-zone:impose-size (root port) (+ width 2) (+ height 4))))

(defmethod clim3-zone:notify-child-hsprawl-changed (zone (port test-port))
  nil)

(defmethod clim3-zone:notify-child-vsprawl-changed (zone (port test-port))
  nil)

(defun test1 ()
  (let ((z (clim3-graphics:masked (clim3-color:make-color 1.0 0.5 0.5)
				  #2A((0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
				      (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
				      (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
				      (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
				      (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)))))
    (assert (not (null (clim3-zone:hsprawl z))))
    (assert (not (null (clim3-zone:vsprawl z))))
    (multiple-value-bind (w h)
	(clim3-zone:natural-size z)
      (assert (= w 11))
      (assert (= h 5)))))

(defun test2 ()
  (let ((port (make-instance 'test-port))
	(zone (clim3-graphics:opaque (clim3-color:make-color 0.5 0.5 0.5))))
    (clim3-port:connect zone port)
    (assert (eq (clim3-zone:parent zone) port))
    (assert (eq (clim3-zone:client zone) port))
    (update port)
    (assert (= (clim3-zone:width zone) 2))
    (assert (= (clim3-zone:height zone) 4))))

(defun test3 ()
  (let ((port (make-instance 'test-port))
	(zone (clim3-graphics:masked
	       (clim3-color:make-color 0.5 0.5 0.5)
	       (make-array '(50 30) :initial-element 0.5))))
    (clim3-port:connect zone port)
    (assert (eq (clim3-zone:parent zone) port))
    (assert (eq (clim3-zone:client zone) port))
    (update port)
    (assert (= (clim3-zone:width zone) 32))
    (assert (= (clim3-zone:height zone) 54))))

(defun test ()
  (test1)
  (test3)
  (test2))