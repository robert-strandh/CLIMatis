(in-package #:clim3-graphics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image zone.
;;;
;;; An image zone has individual color and alpha values for each
;;; pixel.

(defclass image (clim3-zone:atomic-zone)
  ((%pixels :initarg :pixels :reader pixels)))

(defun image (pixels)
  (make-instance 'image :pixels pixels))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Monochrome zone.
;;;
;;; A monochrome zone is one that uses only one color, but different
;;; parts of the zone may use different alpha values. 

(defclass monochrome (clim3-zone:atomic-zone)
  ((%color :initarg :color :accessor color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opaque zone.

(defclass opaque (monochrome)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defun opaque (color)
  (make-instance 'opaque :color color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Masked zone.
;;; 
;;; A masked zone is a monochrome zone where the alpha values are
;;; taken from a mask.  A mask is a 2-dimensional array of alpha
;;; values, represented as floating-point numbers between 0 and 1.

(defclass masked (monochrome)
  ((%opacities :initarg :opacities :reader opacities)))

(defmethod initialize-instance :after ((zone masked) &key)
  (let ((dim (array-dimensions (opacities zone))))
    (setf (clim3-zone:hsprawl zone)
	  (clim3-sprawl:sprawl (cadr dim) (cadr dim) (cadr dim)))
    (setf (clim3-zone:vsprawl zone)
	  (clim3-sprawl:sprawl (car dim) (car dim) (car dim)))))

(defun masked (color opacities)
  (make-instance 'masked
		 :color color
		 :opacities opacities))