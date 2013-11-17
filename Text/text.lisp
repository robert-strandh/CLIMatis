(in-package #:clim3-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text zone.

(defclass text (clim3:monochrome)
  ((%style :initarg :style :reader style)
   (%chars :initarg :chars
	   :initform (make-array 0 :element-type 'character)
	   :reader chars)))

(defmethod clim3-ext:print-components progn ((zone text) stream)
  (format stream "~s " (chars zone)))

(defmethod (setf clim3-ext:parent) :after ((new-parent null) (zone text))
  (setf (clim3:hsprawl zone) nil)
  (setf (clim3:vsprawl zone) nil))

(defmethod clim3-ext:compute-hsprawl ((zone text))
  (clim3-ext:set-hsprawl
   (let ((width (clim3-port:text-width (clim3-ext:client zone)
				       (style zone)
				       (chars zone))))
     (clim3-sprawl:sprawl width width width))
   zone))

(defmethod clim3-ext:compute-vsprawl ((zone text))
  (clim3-ext:set-vsprawl
   (let ((height (+ (clim3-port:text-style-ascent
		     (clim3-ext:client zone) (style zone))
		    (clim3-port:text-style-descent
		     (clim3-ext:client zone) (style zone)))))
     (clim3-sprawl:sprawl height height height))
   zone))

(defun text (string style color)
  (make-instance 'text :style style :chars string :color color))
  
(defmethod clim3-paint:new-paint ((zone text))
  (clim3-port:new-paint-text (chars zone) (style zone) (clim3:color zone)))

