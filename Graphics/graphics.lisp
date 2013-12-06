(in-package #:clim3-graphics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image zone.
;;;
;;; An image zone has individual color and alpha values for each
;;; pixel.

(defclass clim3:image (clim3:standard-zone
		       clim3-ext:atomic-mixin)
  ((%pixels :initarg :pixels :reader clim3:pixels)))

(defun clim3:image (pixels)
  (make-instance 'clim3:image :pixels pixels))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Monochrome zone.
;;;
;;; A monochrome zone is one that uses only one color, but different
;;; parts of the zone may use different alpha values. 

(defclass clim3:monochrome (clim3:standard-zone
			    clim3-ext:atomic-mixin)
  ((%color :initarg :color :accessor clim3:color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opaque zone.

(defclass clim3:opaque (clim3:monochrome)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defun clim3:opaque (color)
  (make-instance 'clim3:opaque :color color))

(defmethod clim3-ext:paint ((zone clim3:opaque))
  (clim3:paint-opaque (clim3:color zone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Masked zone.
;;; 
;;; A masked zone is a monochrome zone where the alpha values are
;;; taken from a mask.  A mask is a 2-dimensional array of alpha
;;; values, represented as floating-point numbers between 0 and 1.

(defclass clim3:masked (clim3:monochrome)
  ((%opacities :initarg :opacities :reader clim3:opacities)))

(defmethod initialize-instance :after ((zone clim3:masked) &key)
  (let ((dim (array-dimensions (clim3:opacities zone))))
    (setf (clim3:hsprawl zone)
	  (clim3-sprawl:sprawl (cadr dim) (cadr dim) (cadr dim)))
    (setf (clim3:vsprawl zone)
	  (clim3-sprawl:sprawl (car dim) (car dim) (car dim)))))

(defun clim3:masked (color opacities)
  (make-instance 'clim3:masked
		 :color color
		 :opacities opacities))

(defmethod clim3-ext:paint ((zone clim3:masked))
  (clim3::paint-mask
   (clim3:opacities zone) (clim3:color zone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Translucent zone.
;;; 
;;; A translucent zone is a monochrome zone with an opacity value that
;;; is uniform across the zone.

(defclass clim3:translucent (clim3:monochrome)
  ((%opacity :initarg :opacity :reader clim3:opacity))
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defun clim3:translucent (color opacity)
  (make-instance 'clim3:translucent
		 :color color
		 :opacity opacity))

(defmethod clim3-ext:paint ((zone clim3:translucent))
  (clim3:paint-translucent   (clim3:color zone) (clim3:opacity zone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Raised

(defclass clim3:raised (clim3:standard-zone
			clim3-ext:atomic-mixin)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod clim3-ext:paint ((zone clim3:raised))
  (let ((black (clim3:make-color 0.0 0.0 0.0))
	(white (clim3:make-color 1.0 1.0 1.0))
	(opacity 0.3)
	(thickness 3)
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

(defun clim3:raised ()
  (make-instance 'clim3:raised))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sunken

(defclass clim3:sunken (clim3:standard-zone
			clim3-ext:atomic-mixin)
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)
		     :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

(defmethod clim3-ext:paint ((zone clim3:sunken))
  (let ((black (clim3:make-color 0.0 0.0 0.0))
	(white (clim3:make-color 1.0 1.0 1.0))
	(opacity 0.3)
	(thickness 3)
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

(defun clim3:sunken ()
  (make-instance 'clim3:sunken))
