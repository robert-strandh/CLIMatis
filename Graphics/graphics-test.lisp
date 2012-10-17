(in-package #:clim3-graphics-test)

(defun test ()
  (let ((z (clim3-graphics:opaque (clim3-color:make-color 0.5 0.5 0.5))))
    (assert (clim3-zone:gives-valid-p z))
    (multiple-value-bind (result condition)
	(ignore-errors (clim3-zone:invalidate-gives z))
      (assert (and (null result) (typep condition 'error)))))
  (let ((z (clim3-graphics:masked (clim3-color:make-color 1.0 0.5 0.5)
				  #2A((0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
				      (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
				      (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
				      (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
				      (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)))))
    (assert (clim3-zone:gives-valid-p z))
    (multiple-value-bind (w h)
	(clim3-zone:natural-size z)
      (assert (= w 11))
      (assert (= h 5)))
    (multiple-value-bind (result condition)
	(ignore-errors (clim3-zone:invalidate-gives z))
      (assert (and (null result) (typep condition 'error))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create a backend for testing.

(defclass test-port (clim3-port:port)
  ((%root :initform nil :accessor root)))

(defmethod clim3-port:connect (zone (port test-port))
  (setf (root port) zone)
  (setf (clim3-zone:parent zone) port))

(defun update (port)
  (clim3-zone:ensure-gives-valid (root port))
  (multiple-value-bind (width height)
      (clim3-zone:natural-size (root port))
    ;; This port imposes a size slightly bigger than the natural one.
    (clim3-zone:impose-size (root port) (+ width 2) (+ height 4))))

(defmethod clim3-zone:notify-child-gives-changed (zone (port test-port))
  nil)

(defmethod clim3-zone:notify-child-gives-invalid (zone (port test-port))
  nil)

(defun test2 ()
  (let ((port (make-instance 'test-port))
	(zone (clim3-graphics:opaque (clim3-color:make-color 0.5 0.5 0.5))))
    (clim3-port:connect zone port)
    (assert (eq (clim3-zone:parent zone) port))
    (assert (eq (clim3-zone:find-client zone) port))
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
    (assert (eq (clim3-zone:find-client zone) port))
    (update port)
    (assert (= (clim3-zone:width zone) 32))
    (assert (= (clim3-zone:height zone) 54))))

