(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function HPOS.
;;;
;;; Return the current horizontal position of the zone relative to its
;;; parent.  
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not reflect the position of the corresponding top-level
;;; window on the display. 

(defgeneric hpos (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF HPOS).
;;;
;;; Set the current horizontal position of the zone relative to its
;;; parent.  
;;;
;;; Calling this function is the normal way of setting the position of
;;; a zone that is the child of a layout zone that does not impose the
;;; position of its children, such as a bboard zone or a scroller
;;; zone.
;;;
;;; Calling this function when the zone is the child of a layout zone
;;; that determines the position of its children itself, will signal
;;; an error.
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not affect the position of the corresponding top-level window
;;; on the display.
;;;
;;; Calling this function triggers the position-change protocol,
;;; informing the parent that a change has taken place.  

(defgeneric (setf hpos) (new-hpos zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-HPOS.
;;;
;;; Set the current horizontal position of the zone relative to its
;;; parent, without triggering the position-change protocol.
;;;
;;; This function is similar to (SETF HPOS), with the difference that
;;; it does not trigger the position-change protocol.  It is used
;;; exclusively by internal protocols to avoid infinite recursions
;;; when a parent needs to set the position of a child as a result of
;;; some previous change.

(defgeneric set-hpos (new-hpos zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VPOS.
;;;
;;; Return the current vertical position of the zone relative to its
;;; parent.  
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not reflect the position of the corresponding top-level window
;;; on the display.

(defgeneric vpos (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF VPOS).
;;;
;;; Set the current vertical position of the zone relative to its
;;; parent.
;;;
;;; Calling this function is the normal way of setting the position of
;;; a zone that is the child of a layout zone that does not impose the
;;; position of its children, such as a bboard zone or a scroller
;;; zone.
;;;
;;; Calling this function when the zone is the child of a layout zone
;;; that determines the position of its children itself, will signal
;;; an error.
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not affect the position of the corresponding top-level window
;;; on the display.
;;;
;;; Calling this function triggers the position-change protocol,
;;; informing the parent that a change has taken place.  

(defgeneric (setf vpos) (new-vpos zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-VPOS.
;;;
;;; Set the current vertical position of the zone relative to its
;;; parent, without triggering the position-change protocol.
;;;
;;; This function is similar to (SETF VPOS), with the difference that
;;; it does not trigger the position-change protocol.  It is used
;;; exclusively by internal protocols to avoid infinite recursions
;;; when a parent needs to set the position of a child as a result of
;;; some previous change.

(defgeneric set-vpos (new-vpos zone))

;;; FIXME make hpos and vpos :reader.

(defclass position-mixin ()
  ((%hpos :initform 0 :initarg :hpos :accessor hpos :writer set-hpos)
   (%vpos :initform 0 :initarg :vpos :accessor vpos :writer set-vpos)))


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

(defmethod notify-child-position-changed ((child zone) (parent null))
  nil)

;;; After the horizontal position of a zone has been explicity
;;; modified, we notify the parent.
(defmethod (setf hpos) :after (new-hpos (zone zone))
  (notify-child-position-changed zone (parent zone)))

;;; After the vertical position of a zone has been explicity
;;; modified, we notify the parent.
(defmethod (setf vpos) :after (new-vpos (zone zone))
  (notify-child-position-changed zone (parent zone)))

