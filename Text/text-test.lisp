(in-package #:clim3-text-test)

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
  (clim3-zone:ensure-sprawls-valid (root port))
  (multiple-value-bind (width height)
      (clim3-zone:natural-size (root port))
    ;; This port imposes a size slightly bigger than the natural one.
    (clim3-zone:impose-size (root port) (+ width 2) (+ height 4))))

(defmethod clim3-zone:notify-child-sprawls-changed (zone (port test-port-1))
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
  (clim3-zone:ensure-sprawls-valid (root port))
  (multiple-value-bind (width height)
      (clim3-zone:natural-size (root port))
    ;; This port imposes a size slightly bigger than the natural one.
    (clim3-zone:impose-size (root port) (+ width 3) (+ height 5))))

(defmethod clim3-zone:notify-child-sprawls-changed (zone (port test-port-2))
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
	(port2 (make-instance 'test-port-2))
	(zone (clim3-text:text "hi" nil (clim3-color:make-color 1.0 0.0 0.0))))
    (clim3-port:connect zone port1)
    (assert (null (clim3-zone:hsprawl zone)))
    (assert (null (clim3-zone:vsprawl zone)))
    (assert (eq (clim3-zone:parent zone) port1))
    (assert (eq (clim3-zone:client zone) port1))
    (update port1)
    (assert (= (clim3-zone:width zone) 18))
    (assert (= (clim3-zone:height zone) 44))
    (clim3-port:disconnect zone port1)
    (assert (null (clim3-zone:client zone)))
    (assert (null (clim3-zone:hsprawl zone)))
    (assert (null (clim3-zone:vsprawl zone)))
    (clim3-port:connect zone port2)
    (assert (eq (clim3-zone:client zone) port2))
    (update port2)
    (assert (not (null (clim3-zone:hsprawl zone))))
    (assert (not (null (clim3-zone:vsprawl zone))))
    (assert (= (clim3-zone:width zone) 21))
    (assert (= (clim3-zone:height zone) 37))))

(defun test ()
  (test1))