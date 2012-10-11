(in-package #:clim3-text-style)

(defgeneric family (text-style))
(defgeneric face (text-style))
(defgeneric size (text-style))

(defclass text-style ()
  ((%family :initarg :family :reader family)
   (%face :initarg :face :reader face)
   (%size :initarg :size :reader size)))

(defun make-text-style (family face size)
  (make-instance 'text-style
		 :family family
		 :face face
		 :size size))


