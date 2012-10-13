(in-package #:clim3-input)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INPUT.
;;;
;;; An input zone is very elastic.  Client code will stick the input
;;; zone in a compound zone on top of some more rigid zones. 

(defclass input (clim3-zone:atomic-zone)
  ((%handler :initarg :handler :reader handler))
  (:default-initargs :vgive (rigidity:little-rigid)
		     :hgive (rigidity:little-rigid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class KEY-PRESS.
;;;
;;; The handler is called with the following arguments:
;;;
;;;   key-code   
;;;   modifiers

(defclass key-press (input) ())

(defun key-press (handler)
  (make-instance 'key-press :handler handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class KEY-RELEASE.
;;;
;;; The handler is called with the following arguments:
;;;
;;;   key-code   
;;;   modifiers

(defclass key-release (input) ())

(defun key-release (handler)
  (make-instance 'key-release :handler handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BUTTON-PRESS.
;;;
;;; The handler is called with the following arguments:
;;;
;;;   button-code   
;;;   modifiers

(defclass button-press (input) ())

(defun button-press (handler)
  (make-instance 'button-press :handler handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BUTTON-RELEASE.
;;;
;;; The handler is called with the following arguments:
;;;
;;;   button-code   
;;;   modifiers

(defclass button-release (input) ())

(defun button-release (handler)
  (make-instance 'button-release :handler handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ENTER.
;;;
;;; The handler is called with no arguments.

(defclass enter (input) ())

(defun enter (handler)
  (make-instance 'enter :handler handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEAVE.
;;;
;;; The handler is called with no arguments.

(defclass leave (input) ())

(defun leave (handler)
  (make-instance 'leave :handler handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MOTION.
;;;
;;; The handler is called with the following arguments:
;;;
;;; hpos
;;; vpos

(defclass motion (input) ())

(defun motion (handler)
  (make-instance 'motion :handler handler))

