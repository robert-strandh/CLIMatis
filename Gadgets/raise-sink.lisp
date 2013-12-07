(cl:in-package #:clim3-gadgets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Raise

(defclass clim3-gadgets:raise (clim3:standard-zone
			       clim3-ext:atomic-mixin)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod clim3-ext:paint ((zone clim3-gadgets:raise))
  (let ((black (clim3:make-color 0.0 0.0 0.0))
	(white (clim3:make-color 1.0 1.0 1.0))
	(opacity (clim3-gadgets:raise-border-opacity clim3-gadgets:*theme*))
	(thickness (clim3-gadgets:raise-border-thickness clim3-gadgets:*theme*))
	(width (clim3:width zone))
	(height (clim3:height zone)))
    ;; Paint the left edge.
    (loop for hpos from 0
	  for vpos from 0
	  for h downfrom height by 2
	  repeat thickness
	  do (clim3:with-area (hpos vpos 1 h)
	       (clim3:paint-translucent white opacity)))
    ;; Paint the right edge.
    (loop for hpos downfrom (1- width)
	  for vpos from 0
	  for h downfrom height by 2
	  repeat thickness
	  do (clim3:with-area (hpos vpos 1 h)
	       (clim3:paint-translucent black opacity)))
    ;; Paint the top edge.
    (loop for hpos from 0
	  for vpos from 0
	  for w downfrom width by 2
	  repeat thickness
	  do (clim3:with-area (hpos vpos w 1)
	       (clim3:paint-translucent white opacity)))
    ;; Paint the bottom edge.
    (loop for vpos downfrom (1- height)
	  for hpos from 0
	  for w downfrom width by 2
	  repeat thickness
	  do (clim3:with-area (hpos vpos w 1)
	       (clim3:paint-translucent black opacity)))))

(defun clim3-gadgets:raise ()
  (make-instance 'clim3-gadgets:raise))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sink

(defclass clim3-gadgets:sink (clim3:standard-zone
			      clim3-ext:atomic-mixin)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod clim3-ext:paint ((zone clim3-gadgets:sink))
  (let ((black (clim3:make-color 0.0 0.0 0.0))
	(white (clim3:make-color 1.0 1.0 1.0))
	(opacity (clim3-gadgets:sink-border-opacity clim3-gadgets:*theme*))
	(thickness (clim3-gadgets:sink-border-thickness clim3-gadgets:*theme*))
	(width (clim3:width zone))
	(height (clim3:height zone)))
    ;; Paint the left edge.
    (loop for hpos from 0
	  for vpos from 0
	  for h downfrom height by 2
	  repeat thickness
	  do (clim3:with-area (hpos vpos 1 h)
	       (clim3:paint-translucent black opacity)))
    ;; Paint the right edge.
    (loop for hpos downfrom (1- width)
	  for vpos from 0
	  for h downfrom height by 2
	  repeat thickness
	  do (clim3:with-area (hpos vpos 1 h)
	       (clim3:paint-translucent white opacity)))
    ;; Paint the top edge.
    (loop for hpos from 0
	  for vpos from 0
	  for w downfrom width by 2
	  repeat thickness
	  do (clim3:with-area (hpos vpos w 1)
	       (clim3:paint-translucent black opacity)))
    ;; Paint the bottom edge.
    (loop for vpos downfrom (1- height)
	  for hpos from 0
	  for w downfrom width by 2
	  repeat thickness
	  do (clim3:with-area (hpos vpos w 1)
	       (clim3:paint-translucent white opacity)))))

(defun clim3-gadgets:sink ()
  (make-instance 'clim3-gadgets:sink))
