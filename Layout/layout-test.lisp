(in-package #:clim3-layout-test)

(defgeneric update (port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Port of type 1

(defclass test-port-1 (clim3-port:port)
  ((%root :initform nil :accessor root)))

(defmethod clim3-port:connect (zone (port test-port-1))
  (setf (root port) zone)
  (setf (clim3-zone:parent zone) port))

(defmethod clim3-port:disconnect (zone (port test-port-1))
  (setf (root port) nil)
  (setf (clim3-zone:parent zone) nil))

(defmethod update ((port test-port-1))
  (clim3-zone:ensure-gives-valid (root port))
  (multiple-value-bind (width height)
      (clim3-zone:natural-size (root port))
    ;; This port imposes a size slightly bigger than the natural one.
    (clim3-zone:impose-size (root port) (+ width 2) (+ height 4))))

(defmethod clim3-zone:notify-child-gives-changed (zone (port test-port-1))
  nil)

(defmethod clim3-zone:notify-child-gives-invalid (zone (port test-port-1))
  nil)

(defmethod clim3-port:text-style-ascent ((port test-port-1) text-style)
  (declare (ignore text-style))
  25)

(defmethod clim3-port:text-style-descent ((port test-port-1) text-style)
  (declare (ignore text-style))
  15)

(defmethod clim3-port:text-ascent ((port test-port-1) text-style string)
  (declare (ignore text-style string))
  20)

(defmethod clim3-port:text-descent ((port test-port-1) text-style string)
  (declare (ignore text-style string))
  10)

(defmethod clim3-port:text-width ((port test-port-1) text-style string)
  (declare (ignore text-style))
  (* 8 (length string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Port of type 2

(defclass test-port-2 (clim3-port:port)
  ((%root :initform nil :accessor root)))

(defmethod clim3-port:connect (zone (port test-port-2))
  (setf (root port) zone)
  (setf (clim3-zone:parent zone) port))

(defmethod clim3-port:disconnect (zone (port test-port-2))
  (setf (root port) nil)
  (setf (clim3-zone:parent zone) nil))

(defmethod update ((port test-port-2))
  (clim3-zone:ensure-gives-valid (root port))
  (multiple-value-bind (width height)
      (clim3-zone:natural-size (root port))
    ;; This port imposes a size slightly bigger than the natural one.
    (clim3-zone:impose-size (root port) (+ width 3) (+ height 5))))

(defmethod clim3-zone:notify-child-gives-changed (zone (port test-port-2))
  nil)

(defmethod clim3-zone:notify-child-gives-invalid (zone (port test-port-2))
  nil)

(defmethod clim3-port:text-style-ascent ((port test-port-2) text-style)
  (declare (ignore text-style))
  21)

(defmethod clim3-port:text-style-descent ((port test-port-2) text-style)
  (declare (ignore text-style))
  11)

(defmethod clim3-port:text-ascent ((port test-port-2) text-style string)
  (declare (ignore text-style string))
  16)

(defmethod clim3-port:text-descent ((port test-port-2) text-style string)
  (declare (ignore text-style string))
  6)

(defmethod clim3-port:text-width ((port test-port-2) text-style string)
  (declare (ignore text-style))
  (* 9 (length string)))

(defun test1 ()
  (let ((port1 (make-instance 'test-port-1))
	(port2 (make-instance 'test-port-2)))
    nil))

(defun test ()
  (test1))