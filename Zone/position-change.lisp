(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CHILD-POSITION-CHANGED.
;;;
;;; This generic function is called whenever the position of the zone
;;; is explicitly changed by a call to (SETF HPOS) or (SETF VPOS).
;;;
;;; A defult method is supplied that is specialized for a NULL parent,
;;; and it does nothing.  That way, it is possible to set the position
;;; of the root of a disconnected zone hierarchy.  
;;;
;;; When the zone hierarchy is connected to a port, the root zone of
;;; the hierarchy has the port as its parent.  Therefore, ports should
;;; supply a method for this function, specialized on that type of
;;; port.  Some ports may decide to ignore the notification.  Others
;;; may attempt to move the root window on the display. 
;;;
;;; When the parent is a layout zone, some appropriate action is
;;; required.  Layout zones that determine the position of the
;;; children themselves, such as the vbox and hbox zones provide a
;;; method on this function that signals an error.  Layout zones that
;;; allow their children to determine their own position, but that
;;; need to be informed about when that happens, such as the bboard
;;; zone, supply a method on this function that marks the layout zone
;;; as having children with invalid positions. 

(defgeneric notify-child-position-changed (child parent))
