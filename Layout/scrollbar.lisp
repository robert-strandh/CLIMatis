(cl:in-package #:clim3-layout)

;;;; An abstract scrollbar is a zone that has two aspects to it.  
;;;; 
;;;; The first aspect is that it OBSERVES another zone, called the
;;;; SCROLLER.  The scroller is not a child of the scrollbar zone, but
;;;; the scrollbar zone contains a reference to the scroller.  The
;;;; scrollbar zone observes the relative position and size of the
;;;; single child of the scroller, called the SCROLLEE.
;;;; 
;;;; The second aspect has to do with how the relative position and
;;;; size of the SCROLLEE is reflected in the scrollbar zone itself.
;;;; More about this aspect below.
;;;;
;;;; For a scrollbar zone, one dimension is called the VARIABLE
;;;; dimension and the other dimension is called the FIXED dimension,
;;;; even though, of course, no dimension is ever fixed.  In the case
;;;; of a vertical scrollbar, the HEIGHT is the variable dimension and
;;;; the WIDTH is the fixed dimension.  In the horizontal case, it's
;;;; the other way around.
;;;;
;;;; As far as the fixed dimension is concerned, the scrollbar zone
;;;; always imposes a position of 0 and its own size on its child. 
;;;; 
;;;; In the variable dimension, the scrollbar zone imposes a size and
;;;; position that depends on the analogous position of the scroller.

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
	 (bar-vsprawl (clim3:vsprawl bar))
	 (bar-min-height (clim3-sprawl:min-size bar-vsprawl))
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
		 (min height
		      (max bar-min-height
			   (round (* (/ scroller-size (max 1 scrollee-size))
				     height)))))
	   (setf bar-pos
		 (round (* (/ (- scrollee-pos)
			      (max 1 (- scrollee-size scroller-size)))
			   (max 0
				(- height bar-height))))))
	  ((> scrollee-pos 0)
	   (setf bar-height
		 (min height
		      (max bar-min-height
			   (round (* (/ (- scroller-size scrollee-pos)
					(max 1 scrollee-size))
				     height)))))
	   (setf bar-pos 0))
	  (t
	   (setf bar-height
		 (min height
		      (max bar-min-height
			   (round (* (/ (+ scrollee-pos scrollee-size)
					(max 1 scrollee-size))
				     height)))))
	   (setf bar-pos
		 (- height bar-height))))
    (clim3-ext:impose-size bar bar-width bar-height)
    (setf (clim3-ext:vpos bar) bar-pos)))

(defun clim3:vscrollbar (scroller bar-zone)
  (make-instance 'clim3:vscrollbar
    :scroller scroller
    :children bar-zone))
