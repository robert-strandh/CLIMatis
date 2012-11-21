(in-package #:clim3-text-style)

(defgeneric foundry (text-style))
(defgeneric family (text-style))
(defgeneric face (text-style))
(defgeneric size (text-style))

(defclass text-style ()
  ((%foundry :initarg :foundry :reader foundry)
   (%family :initarg :family :reader family)
   (%face :initarg :face :reader face)
   (%size :initarg :size :reader size)))

(defun make-text-style (foundry family face size)
  (make-instance 'text-style
		 :foundry foundry
		 :family family
		 :face face
		 :size size))
