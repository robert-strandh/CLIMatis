(cl:in-package #:clim3-layout)

(defclass clim3:scrollbar (clim3:standard-zone
			   clim3-ext:at-most-one-child-mixin
			   clim3-ext:changing-child-position-not-allowed-mixin
			   clim3-ext:child-depth-insignificant-mixin)
  ((%scroller :initarg :scroller :reader scroller))
  (:default-initargs :vsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :hsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defclass clim3:vscrollbar (clim3:scrollbar)
  ())

;;; The default method.  It can be overridden. 
(defmethod clim3-ext:impose-child-layouts ((zone clim3:vscrollbar))
  (let* ((bar (clim3:children zone))
	 (bar-width (clim3:width zone))
	 (bar-height nil)
	 (bar-pos nil)
	 (scroller (scroller zone))
	 (scroller-size (clim3:height scroller))
	 (scrollee (clim3:children scroller))
	 (scrollee-size (clim3:height scrollee))
	 (scrollee-pos (clim3:vpos scrollee))
	 (height (clim3:height zone)))
    (cond ((and (>= scrollee-pos 0)
		(<= (+ scrollee-pos scrollee-size)
		    scroller-size))
	   ;; The scrollee is entirely visible. 
	   (setf bar-height height)
	   (setf bar-pos 0))
	  ((and (<= scrollee-pos 0)
		(>= (+ scrollee-pos scrollee-size)
		    scroller-size))
	   ;; The entire scroller is covered vertically by the scrollee.
	   ;; We set the bar height so that it has the same fraction of
	   ;; the height of the scrollbar as the scroller size has to the
	   ;; scrollee size, except that we make a limit as to how small
	   ;; it can be. 
	   (setf bar-height
		 (max 10
		      (round (* (/ scroller-size (max 1 scrollee-size))
				height))))
	   (setf bar-pos
		 (round (* (/ (- scrollee-pos)
			      (max 1 (- scrollee-size scroller-size)))
			   (max 0
				(- height bar-height))))))
	  ((> scrollee-pos 0)
	   (setf bar-height
		 (max 10
		      (round (* (/ (- scroller-size scrollee-pos)
				   (max 1 scrollee-size))
				height))))
	   (setf bar-pos 0))
	  (t
	   (setf bar-height
		 (max 10
		      (round (* (/ (+ scrollee-pos scrollee-size)
				   (max 1 scrollee-size))
				height))))
	   (setf bar-pos
		 (- height bar-height))))
    (clim3-ext:impose-size bar bar-width bar-height)
    (clim3-ext:set-vpos bar-pos bar)))

(defun clim3:vscrollbar (scroller bar-zone)
  (make-instance 'clim3:vscrollbar
    :scroller scroller
    :children bar-zone))
