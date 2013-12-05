(cl:in-package #:clim3-layout)

(defclass clim3:scrollbar (clim3:standard-zone
			   clim3-ext:at-most-one-child-mixin
			   clim3-ext:changing-child-position-not-allowed-mixin
			   clim3-ext:child-depth-insignificant-mixin)
  ((%scroller-size :initform 1 :accessor clim3:scroller-size)
   (%scrollee-size :initform 1 :accessor clim3:scrollee-size)
   (%scrollee-pos :initform 0 :accessor clim3:scrollee-pos))
  (:default-initargs :vsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :hsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defgeneric clim3:adjust-scroll
    (scrollbar scroller-size scrollee-size scrollee-pos))

(defmethod clim3:adjust-scroll
    ((scrollbar null) scroller-size scrollee-size scrollee-pos)
  (declare (ignore scroller-size scrollee-size scrollee-pos))
  nil)

(defmethod clim3:adjust-scroll
    ((scrollbar clim3:scrollbar) scroller-size scrollee-size scrollee-pos)
  (setf (clim3:scroller-size scrollbar) (max 1 scroller-size)
	(clim3:scrollee-size scrollbar) (max 1 scrollee-size)
	(clim3:scrollee-pos scrollbar) scrollee-pos))

(defclass clim3:vscrollbar (scrollbar)
  ())

;;; The default method.  It can be overridden. 
(defmethod clim3-ext:impose-child-layouts ((zone clim3:vscrollbar))
  (let ((bar (clim3:children zone))
	(bar-width (clim3:width zone))
	(bar-height nil)
	(bar-pos nil))
    (cond ((and (>= (clim3:scrollee-pos zone) 0)
		(<= (+ (clim3:scrollee-pos zone) (clim3:scrollee-size zone))
		    (clim3:scroller-size zone)))
	   ;; The scrollee is entirely visible. 
	   (setf bar-height (clim3:height zone))
	   (setf bar-pos 0))
	  ((and (<= (clim3:scrollee-pos zone) 0)
		(>= (+ (clim3:scrollee-pos zone) (clim3:scrollee-size zone))
		    (clim3:scroller-size zone)))
	   ;; The entire scroller is covered vertically by the scrollee.
	   ;; We set the bar height so that it has the same fraction of
	   ;; the height of the scrollbar as the scroller size has to the
	   ;; scrollee size, except that we make a limit as to how small
	   ;; it can be. 
	   (setf bar-height
		 (max 10
		      (round (* (/ (clim3:scroller-size zone)
				   (clim3:scrollee-size zone))
				(clim3:height zone)))))
	   (setf bar-pos
		 (round (* (/ (- (clim3:scrollee-pos zone))
			      (- (clim3:scrollee-size zone)
				 (clim3:scroller-size zone)))
			   (max 0
				(- (clim3:height zone) bar-height))))))
	  ((> (clim3:scrollee-pos zone) 0)
	   (setf bar-height
		 (max 10
		      (round (* (/ (- (clim3:scroller-size zone)
				      (clim3:scrollee-pos zone))
				   (clim3:scrollee-size zone))
				(clim3:height zone)))))
	   (setf bar-pos 0))
	  (t
	   (setf bar-height
		 (max 10
		      (round (* (/ (+ (clim3:scrollee-pos zone)
				      (clim3:scrollee-size zone))
				   (clim3:scrollee-size zone))
				(clim3:height zone)))))
	   (setf bar-pos
		 (- (clim3:height zone) bar-height))))
    (clim3-ext:impose-size bar bar-width bar-height)
    (clim3-ext:set-vpos bar bar-pos)))

(defun vscrollbar (bar-zone)
  (make-instance 'vscrollbar :children bar-zone))
