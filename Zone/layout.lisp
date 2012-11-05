(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function IMPOSE-CHILD-LAYOUTS.

(defgeneric impose-child-layouts (compound-zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-CHILD-LAYOUTS-VALID.

(defun ensure-child-layouts-valid (compound-zone)
  (impose-child-layouts compound-zone))
