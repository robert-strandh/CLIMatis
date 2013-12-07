(in-package #:clim3-gadgets)

(defun text (string)
  (clim3-text:text
   string
   (clim3-gadgets:gadget-text-style clim3-gadgets:*theme*)
   (clim3-gadgets:gadget-text-color clim3-gadgets:*theme*)))
