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

(defmethod (setf clim3-zone:parent) :after ((new-parent null) (zone text))
  (clim3-zone:invalidate-gives zone))

(defmethod clim3-zone:mark-gives-invalid ((zone text))
  (clim3-zone:set-vgive nil zone)
  (clim3-zone:set-hgive nil zone))

(defmethod clim3-zone:gives-valid-p ((zone text))
  (and (not (null (clim3-zone:vgive zone)))
       (not (null (clim3-zone:hgive zone)))))

(defmethod clim3-zone:compute-gives ((zone text))
  (clim3-zone:set-hgive
   (rigidity:very-rigid
    (clim3-port:text-width (clim3-zone:find-client zone)
			   (style zone)
			   (chars zone)))
   zone)
  (clim3-zone:set-vgive
   (rigidity:very-rigid
    (+ (clim3-port:text-style-ascent
	(clim3-zone:find-client zone) (style zone))
       (clim3-port:text-style-descent
	(clim3-zone:find-client zone) (style zone))))
   zone))

(defun text (string style color)
  (make-instance 'text :style style :chars string :color color))
  

