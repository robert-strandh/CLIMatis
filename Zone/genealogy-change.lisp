(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILDREN-CHANGED.
;;;
;;; This function is called when the children of some compound zone
;;; have changed, such as when a child has been added or removed.  
;;; 
;;; The default method specialized on ZONE signals an error.  forcing
;;; the parent zone type to choose an action as a result of a call to
;;; this function.

(defgeneric notify-children-changed (zone))

