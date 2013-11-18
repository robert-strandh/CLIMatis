(in-package #:clim3-text-style)

(defgeneric clim3:foundry (text-style))
(defgeneric clim3:family (text-style))
(defgeneric clim3:face (text-style))
(defgeneric clim3:size (text-style))

(defclass clim3:text-style ()
  ((%foundry :initarg :foundry :reader clim3:foundry)
   (%family :initarg :family :reader clim3:family)
   (%face :initarg :face :reader clim3:face)
   (%size :initarg :size :reader clim3:size)))

(defun clim3:text-style (foundry family face size)
  (make-instance 'clim3:text-style
		 :foundry foundry
		 :family family
		 :face face
		 :size size))
