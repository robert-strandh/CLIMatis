(cl:in-package #:clim3-gadgets)

(defclass theme ()
  ((%background-color
    :initarg :background-color
    :accessor background-color)
   (%foreground-color
    :initarg :foreground-color
    :accessor foreground-color)
   (%highlight-opacity
    :initarg :highlight-opacity
    :accessor highlight-opacity)
   (%highlight-color
    :initarg :highlight-color
    :accessor highlight-color)
   (%raise-border-thickness
    :initarg :raise-border-thickness
    :accessor raise-border-thickness)
   (%raise-border-opacity
    :initarg :raise-border-opacity
    :accessor raise-border-opacity)
   (%sink-border-thickness
    :initarg :sink-border-thickness
    :accessor sink-border-thickness)
   (%sink-border-opacity
    :initarg :sink-border-opacity
    :accessor sink-border-opacity)
   (%gadget-text-style
    :initarg :gadget-text-style
    :accessor gadget-text-style)
   (%gadget-text-color
    :initarg :gadget-text-color
    :accessor gadget-text-color)
   (%icon-border-thickness
    :initarg :icon-border-thickness
    :accessor icon-border-thickness)
   (%vscrollbar-class
    :initarg :vscrollbar-class
    :accessor vscrollbar-class)
   (%hscrollbar-class
    :initarg :hscrollbar-class
    :accessor hscrollbar-class)))

(defvar *theme*)

