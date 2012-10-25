(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPUTE-HSPRAWLS.
;;;
;;; This function is repsonsible for computing and setting the
;;; horizontal sprawls of a zone.  It is only called on a zone with
;;; invalid horizontal sprawls.
;;; 
;;; After a call to this function, the horizontal sprawls of the zone
;;; are valid.

(defgeneric compute-hsprawls (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPUTE-VSPRAWLS.
;;;
;;; This function is repsonsible for computing and setting the
;;; vertical sprawls of a zone.  It is only called on a zone with
;;; invalid vertical sprawls.
;;;
;;; After a call to this function, the vertical sprawls of the zone
;;; are valid.

(defgeneric compute-vsprawls (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-HSPRAWL-VALID.
;;;
;;; This function checks whether the horizontal sprawl of the zone is
;;; valid, and if so does nothing.  Otherwise, it calls
;;; COMPUTE-HSPRAWLS.

(defun ensure-hsprawl-valid (zone)
  (when (null (hsprawl zone))
    (compute-hsprawls zone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-VSPRAWL-VALID.
;;;
;;; This function checks whether the vertical sprawl of the zone is
;;; valid, and if so does nothing.  Otherwise, it calls
;;; COMPUTE-VSPRAWLS.

(defun ensure-vprawl-valid (zone)
  (when (null (vsprawl zone))
    (compute-vsprawls zone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function IMPOSE-SIZE.
;;;
;;; This function is called in order to set the size of a zone.  

(defgeneric impose-size (zone width height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CHILD-LAYOUTS-VALID-P.
;;;

(defgeneric child-layouts-valid-p (compound-zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF CHILD-LAYOUTS-VALID-P).
;;;

(defgeneric (setf child-layouts-valid-p) (new-value compound-zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function IMPOSE-CHILD-LAYOUTS.

(defgeneric impose-child-layouts (compound-zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-CHILD-LAYOUTS-VALID.

(defun ensure-child-layouts-valid (compound-zone)
  (unless (child-layouts-valid-p compound-zone)
    (impose-child-layouts compound-zone)
    (setf (child-layouts-valid-p compound-zone) t)))