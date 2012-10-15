(in-package #:clim3-zone-test)

(defclass some-compound-zone (clim3-zone:compound-simple-zone
			      clim3-zone:any-number-of-children-mixin)
  ())

(defun test-parent-after-setf-children ()
  (let ((zone1 (make-instance 'some-compound-zone :children '()))
	(zone2 (make-instance 'clim3-zone:atomic-zone)))
    (assert (null (clim3-zone:parent zone2)))
    (setf (clim3-zone:children zone1) (list zone2))
    (assert (eq (clim3-zone:parent zone2) zone1))
    (setf (clim3-zone:children zone1) '())
    (assert (null (clim3-zone:parent zone2)))))
