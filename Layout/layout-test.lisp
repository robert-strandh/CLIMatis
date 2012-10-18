(in-package #:clim3-layout-test)

(defgeneric update (port))

(defgeneric render (zone port hstart vstart hend vend))

(defgeneric call-with-translation (port dv dh function))

;;; The coordinates are in the coordinate system of the parent of the
;;; zone.   The coordinates are not ouside the parent. 
(defun clip-to-child (zone hstart vstart hend vend)
  (let ((hpos (clim3-zone:hpos zone))
	(vpos (clim3-zone:vpos zone))
	(width (clim3-zone:width zone))
	(height (clim3-zone:height zone)))
    ;;; Translate the coordinates into the coordinate system of the
    ;;; zone.
    (let ((hs (- hstart hpos))
	  (vs (- vstart vpos))
	  (he (- hend hpos))
	  (ve (- vend vpos)))
      (values (max 0 hs)
	      (max 0 vs)
	      (min width he)
	      (min height ve)))))

(defun clip-non-empty-p (hstart vstart hend vend)
  (and (> hend hstart) (> vend vstart)))

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

(defparameter *hpos* 0)
(defparameter *vpos* 0)

(defun port-render (hstart vstart hend vend)
  (list (+ hstart *hpos*)
	(+ vstart *vpos*)
	(+ hend *hpos*)
	(+ vend *vpos*)))

(defmacro with-translation ((port dh dv) &body body)
  `(call-with-translation ,port ,dh ,dv (lambda () ,@body)))

(defmethod call-with-translation ((port test-port-1) dh dv function)
  (let ((*hpos* (+ *hpos* dh))
	(*vpos* (+ *vpos* dv)))
    (funcall function)))

(defmethod render
    ((zone clim3-zone:compound-zone) (port test-port-1) hstart vstart hend vend)
  (clim3-zone:map-over-children
   (lambda (child) (clim3-zone:ensure-gives-valid child))
   zone)
  (clim3-zone:ensure-child-layouts-valid zone)
  (let ((results '()))
    (clim3-zone:map-over-children-bottom-to-top
     (lambda (child)
       (multiple-value-bind (hs vs he ve)
	   (clip-to-child child hstart vstart hend vend)
	 (when (clip-non-empty-p hs vs he ve)
	   (setf results
		 (append results
			 (with-translation (port
					    (clim3-zone:hpos child)
					    (clim3-zone:vpos child))
			   (render child port hs vs he ve)))))))
     zone)
    results))

(defmethod render
    ((zone clim3-zone:atomic-zone) (port test-port-1) hstart vstart hend vend)
  (list (port-render hstart vstart hend vend)))

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
  (let* ((port1 (make-instance 'test-port-1))
	 (port2 (make-instance 'test-port-2))
	 (z1 (clim3-graphics:masked
	      (clim3-color:make-color 0.5 0.5 0.5)
	      (make-array '(10 20) :initial-element 0.4)))
	 (z2 (clim3-layout:vbox* z1)))
    (declare (ignore port2))
    (clim3-port:connect z2 port1)
    (update port1)
    (let ((result (render z2 port1 0 0 (clim3-zone:width z2) (clim3-zone:height z2))))
      (assert (equal result '((0 0 22 14)))))))

(defun test2 ()
  (let* ((port1 (make-instance 'test-port-1))
	 (port2 (make-instance 'test-port-2))
	 (z1 (clim3-graphics:masked
	      (clim3-color:make-color 0.5 0.5 0.5)
	      (make-array '(10 20) :initial-element 0.4)))
	 (z2 (clim3-graphics:masked
	      (clim3-color:make-color 0.5 0.5 0.5)
	      (make-array '(30 20) :initial-element 0.4)))
	 (z3 (clim3-layout:vbox* z1 z2)))
    (declare (ignore port2))
    ;; Make sure the second zone is rendered first
    (setf (clim3-zone:depth z1) 0)
    (setf (clim3-zone:depth z2) 1)
    (clim3-port:connect z3 port1)
    (update port1)
    (let ((result (render z3 port1 0 0 (clim3-zone:width z3) (clim3-zone:height z3))))
      (assert (= (length result) 2))
      (let ((r1 (car result))
	    (r2 (cadr result)))
	(assert (= (car r1) 0))
	(assert (= (car r2) 0)))
      result)))

(defun test ()
  (test1))