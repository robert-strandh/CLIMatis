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

;;; After the horizontal position of a zone has been explicity
;;; modified, we notify the parent.
(defmethod (setf hpos) :after (new-hpos (zone zone))
  (notify-child-position-changed zone (parent zone)))

;;; After the vertical position of a zone has been explicity
;;; modified, we notify the parent.
(defmethod (setf vpos) :after (new-vpos (zone zone))
  (notify-child-position-changed zone (parent zone)))

