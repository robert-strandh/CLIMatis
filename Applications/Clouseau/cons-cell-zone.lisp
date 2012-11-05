(in-package #:clouseau)

(defclass cons-cell-zone (clim3-graphics:monochrome)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 55 55 nil)
		     :vsprawl (clim3-sprawl:sprawl 35 35 nil)))

(defmethod clim3-paint:new-paint ((zone cons-cell-zone))
  (clim3-port:new-paint-trapezoids 
   `((00d0 02d0 00d0 00d0 40d0 40d0)
     (02d0 20d0 00d0 00d0 02d0 02d0)
     (02d0 20d0 19d0 19d0 21d0 21d0)
     (02d0 20d0 38d0 38d0 40d0 40d0)
     (20d0 22d0 00d0 00d0 40d0 40d0)
     (09d0 20d0 09d0 09d0 11d0 11d0)
     (09d0 11d0 29d0 29d0 38d0 38d0)
     (22d0 ,(coerce (- (clim3-zone:height zone) 10) 'double-float)
      09d0 09d0 11d0 11d0)
     (,(coerce (- (clim3-zone:height zone) 10) 'double-float)
      ,(coerce (clim3-zone:height zone) 'double-float)
      07d0 10d0 13d0 10d0)
     (09d0 11d0
      40d0 40d0
      ,(coerce (- (clim3-zone:width zone) 10) 'double-float)
      ,(coerce (- (clim3-zone:width zone) 10) 'double-float))
     (07d0 10d0
      ,(coerce (- (clim3-zone:width zone) 10) 'double-float)
      ,(coerce (- (clim3-zone:width zone) 10) 'double-float)
      ,(coerce (- (clim3-zone:width zone) 10) 'double-float)
      ,(coerce (clim3-zone:width zone) 'double-float))
     (10d0 13d0
      ,(coerce (- (clim3-zone:width zone) 10) 'double-float)
      ,(coerce (- (clim3-zone:width zone) 10) 'double-float)
      ,(coerce (clim3-zone:width zone) 'double-float)
      ,(coerce (- (clim3-zone:width zone) 10) 'double-float))
     )
   (clim3-graphics:color zone)))


(defun cons-cell (color)
  (make-instance 'cons-cell-zone :color color))
