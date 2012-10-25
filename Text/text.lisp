(in-package #:clim3-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text zone.
;;;
;;; For now, the text style is a dummy, because we have only one. 

(defclass text (clim3-graphics:monochrome)
  ((%style :initarg :style :reader style)
   (%chars :initarg :chars
	   :initform (make-array 0 :element-type 'character)
	   :reader chars)))

(defmethod clim3-zone:print-components progn ((zone text) stream)
  (format stream "~s " (chars zone)))

(defmethod (setf clim3-zone:parent) :after ((new-parent null) (zone text))
  (setf (clim3-zone:hsprawl zone) nil)
  (setf (clim3-zone:vsprawl zone) nil))

(defmethod clim3-zone:compute-sprawls ((zone text))
  (clim3-zone:set-hsprawl
   (let ((width (clim3-port:text-width (clim3-zone:client zone)
				       (style zone)
				       (chars zone))))
     (clim3-sprawl:sprawl width width width))
   zone)
  (clim3-zone:set-vsprawl
   (let ((height (+ (clim3-port:text-style-ascent
		     (clim3-zone:client zone) (style zone))
		    (clim3-port:text-style-descent
		     (clim3-zone:client zone) (style zone)))))
     (clim3-sprawl:sprawl height height height))
   zone))

(defun text (string style color)
  (make-instance 'text :style style :chars string :color color))
  

