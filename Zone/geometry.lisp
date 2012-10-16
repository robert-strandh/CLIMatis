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
;;; Setting the position of a zone may not have the effect that an
;;; application expects.  If the zone is the child of a layout zone
;;; that imposes the position of its children, then the change will be
;;; undone before the next time around the event loop, so that no
;;; visible effect can be detected.  
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not affect the position of the corresponding top-level window
;;; on the display.
;;;
;;; Calling this function triggers the geometry-change protocol,
;;; informing the parent that a change has taken place.  

(defgeneric (setf hpos) (new-hpos zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-HPOS.
;;;
;;; Set the current horizontal position of the zone relative to its
;;; parent, without triggering the geometry-change protocol.
;;;
;;; This function is similar to (SETF HPOS), with the difference that
;;; it does not trigger the geometry-change protocol.  It is used
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
;;; Setting the position of a zone may not have the effect that an
;;; application expects.  If the zone is the child of a layout zone
;;; that imposes the position of its children, then the change will be
;;; undone before the next time around the event loop, so that no
;;; visible effect can be detected.  
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not affect the position of the corresponding top-level window
;;; on the display.
;;;
;;; Calling this function triggers the geometry-change protocol,
;;; informing the parent that a change has taken place.  

(defgeneric (setf vpos) (new-vpos zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-VPOS.
;;;
;;; Set the current vertical position of the zone relative to its
;;; parent, without triggering the geometry-change protocol.
;;;
;;; This function is similar to (SETF VPOS), with the difference that
;;; it does not trigger the geometry-change protocol.  It is used
;;; exclusively by internal protocols to avoid infinite recursions
;;; when a parent needs to set the position of a child as a result of
;;; some previous change.

(defgeneric set-vpos (new-vpos zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WIDTH.
;;;
;;; Return the current width of the zone.  
;;; 
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not reflect the width of the corresponding top-level window.

(defgeneric width (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF WIDTH).
;;;
;;; Set the current width of the zone.
;;;
;;; Calling this function is the normal way of setting the width of a
;;; zone that is the child of a layout zone that does not impose the
;;; size of its children, such as a bboard zone or a scroller zone.
;;;
;;; Setting the position of a zone may not have the effect that an
;;; application expects.  If the zone is the child of a layout zone
;;; that imposes the size of its children, then the change will be
;;; undone before the next time around the event loop, so that no
;;; visible effect can be detected.
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not affect the width of the corresponding top-level window
;;; on the display.
;;;
;;; Calling this function triggers the geometry-change protocol,
;;; informing the parent that a change has taken place.  

(defgeneric (setf width) (new-width zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-WIDTH.
;;;
;;; Set the current width of the zone relative to its parent, without
;;; triggering the geometry-change protocol.
;;;
;;; This function is similar to (SETF WIDTH), with the difference that
;;; it does not trigger the geometry-change protocol.  It is used
;;; exclusively by internal protocols to avoid infinite recursions
;;; when a parent needs to set the width of a child as a result of
;;; some previous change.

(defgeneric set-width (new-width zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function HEIGHT.
;;;
;;; Return the current height of the zone.
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not reflect the height of the corresponding top-level window.

(defgeneric height (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF HEIGHT).
;;;
;;; Set the current height of the zone.
;;;
;;; Calling this function is the normal way of setting the height of a
;;; zone that is the child of a layout zone that does not impose the
;;; size of its children, such as a bboard zone or a scroller zone.
;;;
;;; Setting the position of a zone may not have the effect that an
;;; application expects.  If the zone is the child of a layout zone
;;; that imposes the size of its children, then the change will be
;;; undone before the next time around the event loop, so that no
;;; visible effect can be detected.
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then depending on the display server, this value may or
;;; may not affect the height of the corresponding top-level window on
;;; the display.
;;;
;;; Calling this function triggers the geometry-change protocol,
;;; informing the parent that a change has taken place.

(defgeneric (setf height) (new-height zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-HEIGHT.
;;;
;;; Set the current height of the zone relative to its parent, without
;;; triggering the geometry-change protocol.
;;;
;;; This function is similar to (SETF HEIGHT), with the difference that
;;; it does not trigger the geometry-change protocol.  It is used
;;; exclusively by internal protocols to avoid infinite recursions
;;; when a parent needs to set the height of a child as a result of
;;; some previous change.

(defgeneric set-height (new-height zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DEPTH.
;;;
;;; FIXME: add comment 

(defgeneric depth (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF DEPTH).
;;;
;;; FIXME: add comment 

(defgeneric (setf depth) (new-depth zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SET-DEPTH.
;;;
;;; FIXME: add comment 

(defgeneric SET-DEPTH (new-depth zone))

