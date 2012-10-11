(in-package :clim3)

(defclass event () ())

(defclass device-event (event)
  ((%pos-x :initarg :pos-x :accessor pos-x)
   (%pos-y :initarg :pos-y :accessor pos-y)
   (%modifier-state :initarg :modifier-state :reader modifier-state)))

(defclass keyboard-event (device-event)
  ((%interpretations :initarg :interpretations
		     :accessor interpretations)))

(defclass key-press-event (keyboard-event) ())

(defclass key-release-event (keyboard-event) ())

(defclass pointer-event (device-event) ())

(defclass pointer-button-event (pointer-event) ())

(defclass pointer-button-press-event (pointer-button-event) ())

(defclass pointer-button-release-event (pointer-button-event) ())

(defclass pointer-button-hold-event (pointer-button-event) ())

(defclass pointer-motion-event (pointer-event) ())

(defclass pointer-boundary-event (pointer-motion-event) ())

(defclass pointer-enter-event (pointer-boundary-event) ())

(defclass pointer-exit-event (pointer-boundary-event) ())

(defgeneric handle-event (zone event))