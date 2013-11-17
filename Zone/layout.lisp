(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function IMPOSE-CHILD-LAYOUTS.

(defgeneric clim3-ext:impose-child-layouts (compound-zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-CHILD-LAYOUTS-VALID.

(defun clim3-ext:ensure-child-layouts-valid (compound-zone)
  (clim3-ext:impose-child-layouts compound-zone))
