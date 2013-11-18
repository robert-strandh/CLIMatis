(in-package #:clim3-text-test)

(defgeneric update (port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Port of type 1

(defclass test-port-1 (clim3:port)
  ((%root :initform nil :accessor root)))

(defmethod clim3:connect (zone (port test-port-1))
  (setf (root port) zone)
  (setf (clim3-ext:parent zone) port))

(defmethod clim3:disconnect (zone (port test-port-1))
  (setf (root port) nil)
  (setf (clim3-ext:parent zone) nil))

(defmethod update ((port test-port-1))
  (clim3-ext:ensure-hsprawl-valid (root port))
  (clim3-ext:ensure-vsprawl-valid (root port))
  (multiple-value-bind (width height)
      (clim3:natural-size (root port))
    ;; This port imposes a size slightly bigger than the natural one.
    (clim3-ext:impose-size (root port) (+ width 2) (+ height 4))))

(defmethod clim3-ext:notify-child-hsprawl-changed (zone (port test-port-1))
  nil)

(defmethod clim3-ext:notify-child-vsprawl-changed (zone (port test-port-1))
  nil)

(defmethod clim3:text-style-ascent ((port test-port-1) text-style)
  (declare (ignore text-style))
  25)

(defmethod clim3:text-style-descent ((port test-port-1) text-style)
  (declare (ignore text-style))
  15)

(defmethod clim3:text-ascent ((port test-port-1) text-style string)
  (declare (ignore text-style string))
  20)

(defmethod clim3:text-descent ((port test-port-1) text-style string)
  (declare (ignore text-style string))
  10)

(defmethod clim3:text-width ((port test-port-1) text-style string)
  (declare (ignore text-style))
  (* 8 (length string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Port of type 2

(defclass test-port-2 (clim3:port)
  ((%root :initform nil :accessor root)))

(defmethod clim3:connect (zone (port test-port-2))
  (setf (root port) zone)
  (setf (clim3-ext:parent zone) port))

(defmethod clim3:disconnect (zone (port test-port-2))
  (setf (root port) nil)
  (setf (clim3-ext:parent zone) nil))

(defmethod update ((port test-port-2))
  (clim3-ext:ensure-hsprawl-valid (root port))
  (clim3-ext:ensure-vsprawl-valid (root port))
  (multiple-value-bind (width height)
      (clim3:natural-size (root port))
    ;; This port imposes a size slightly bigger than the natural one.
    (clim3-ext:impose-size (root port) (+ width 3) (+ height 5))))

(defmethod clim3-ext:notify-child-hsprawl-changed (zone (port test-port-2))
  nil)

(defmethod clim3-ext:notify-child-vsprawl-changed (zone (port test-port-2))
  nil)

(defmethod clim3:text-style-ascent ((port test-port-2) text-style)
  (declare (ignore text-style))
  21)

(defmethod clim3:text-style-descent ((port test-port-2) text-style)
  (declare (ignore text-style))
  11)

(defmethod clim3:text-ascent ((port test-port-2) text-style string)
  (declare (ignore text-style string))
  16)

(defmethod clim3:text-descent ((port test-port-2) text-style string)
  (declare (ignore text-style string))
  6)

(defmethod clim3:text-width ((port test-port-2) text-style string)
  (declare (ignore text-style))
  (* 9 (length string)))

(defun test1 ()
  (let ((port1 (make-instance 'test-port-1))
	(port2 (make-instance 'test-port-2))
	(zone (clim3-text:text "hi" nil (clim3:make-color 1.0 0.0 0.0))))
    (clim3:connect zone port1)
    (assert (null (clim3:hsprawl zone)))
    (assert (null (clim3:vsprawl zone)))
    (assert (eq (clim3-ext:parent zone) port1))
    (assert (eq (clim3-ext:client zone) port1))
    (update port1)
    (assert (= (clim3:width zone) 18))
    (assert (= (clim3:height zone) 44))
    (clim3:disconnect zone port1)
    (assert (null (clim3-ext:client zone)))
    (assert (null (clim3:hsprawl zone)))
    (assert (null (clim3:vsprawl zone)))
    (clim3:connect zone port2)
    (assert (eq (clim3-ext:client zone) port2))
    (update port2)
    (assert (not (null (clim3:hsprawl zone))))
    (assert (not (null (clim3:vsprawl zone))))
    (assert (= (clim3:width zone) 21))
    (assert (= (clim3:height zone) 37))))

(defun test ()
  (test1))
