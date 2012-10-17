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

(defclass test-port (clim3-port:port) ())

(defmethod clim3-port:connect (zone (port test-port))
  (declare (ignore zone)))

(defmethod clim3-zone:notify-child-gives-changed (zone (port test-port))
  nil)

(defmethod clim3-zone:notify-child-gives-invalid (zone (port test-port))
  nil)