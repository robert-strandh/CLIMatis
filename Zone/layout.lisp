(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPUTE-SPRAWLS.
;;;
;;; This function is repsonsible for computing and setting the sprawls
;;; of a zone.  It is only called on a zone with invalid sprawls.  
;;;
;;; There will be a method specialized for each atomic zone whose
;;; sprawls depend on the current client.  
;;;
;;; We also supply a default method specialized for COMPOUND-ZONE.
;;; That method calls ENSURE-SPRAWLS-VALID on each child, and then calls
;;; COMBINE-CHILD-SPRAWLS to combine the result. 
;;;
;;; After a call to this function, the sprawls of the zone are valid as
;;; reported by SPRAWLS-VALID-P. 

(defgeneric compute-sprawls (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMBINE-CHILD-SPRAWLS.
;;;
;;; This generic function is charged with combining the sprawl of each
;;; child of a compound zone, and calling the functions SET-HSPRAWL
;;; and SET-VSPRAWL to set the result for the zone.
;;;
;;; This function will be called only on compound zones with sprawls
;;; that depend on the sprawls of the children.
;;;
;;; After a call to this function, the sprawls of the zone are valid as
;;; reported by SPRAWLS-VALID-P.

(defgeneric combine-child-sprawls (compound-zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-SPRAWLS-VALID.
;;;
;;; This function checks whether the sprawls of the zone are valid, and
;;; if so does nothing.  Otherwise, it calles COMPUTE-SPRAWLS.

(defun ensure-sprawls-valid (zone)
  (unless (sprawls-valid-p zone)
    (compute-sprawls zone)))

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