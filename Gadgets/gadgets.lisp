(cl:in-package #:clim3-gadgets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function HIGHLIGHT.
;;;
;;; This function is called by the ENTER method specialized for the
;;; class HIGHLIGHT.  Client code should add a method on this generic
;;; function that highlights the zone.

(defgeneric clim3:highlight (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function UNHIGHLIGHT.
;;;
;;; This function is called by the LEAVE method specialized for the
;;; class HIGHLIGHT.  Client code should add a method on this generic
;;; function that unhighlights the zone.

(defgeneric clim3:unhighlight (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HIGHLIGHT.
;;;
;;; This class supplies a method on ENTER that calls HIGHLIGHT and a
;;; method on LEAVE that calls UNHIGHLIGHT.
;;;
;;; It should be a superclass of any zone that should be highlighted
;;; when the pointer enters it, and unhighlighted when the pointer
;;; leaves it.

(defclass clim3:highlight () ())

(defmethod clim3:enter progn ((zone clim3:highlight))
  (clim3:highlight zone))

(defmethod clim3:leave progn ((zone clim3:highlight))
  (clim3:unhighlight zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function VSCROLLBAR.

(defun vscrollbar (scroller)
  (make-instance (vscrollbar-class *theme*)
    :scroller scroller))


