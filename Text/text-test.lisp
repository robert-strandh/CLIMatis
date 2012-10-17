(in-package #:clim3-text-test)

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

(defmethod clim3-port:text-style-ascent ((port test-port) text-style)
  (declare (ignore text-style))
  25)

(defmethod clim3-port:text-style-descent ((port test-port) text-style)
  (declare (ignore text-style))
  15)

(defmethod clim3-port:text-ascent ((port test-port) text-style string)
  (declare (ignore text-style string))
  20)

(defmethod clim3-port:text-descent ((port test-port) text-style string)
  (declare (ignore text-style string))
  10)

(defmethod clim3-port:text-width ((port test-port) text-style string)
  (declare (ignore text-style))
  (* 8 (length (clim3-text:chars string))))
