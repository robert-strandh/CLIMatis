(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILDREN-CHANGED.
;;;
;;; This function is called when the children of some compound zone
;;; have changed, such as when a child has been added or removed.  
;;; 
;;; FIXME: say more.

(defgeneric notify-children-changed (zone))

