(cl:in-package #:common-lisp-user)

(defpackage #:clim3-gadgets
  (:use #:common-lisp)
  (:export
   #:theme
   #:*theme*
   #:foreground-color #:background-color
   #:highlight-opacity #:highlight-color
   #:raise-border-thickness #:raise-border-opacity
   #:sink-border-thickness #:sink-border-opacity
   #:gadget-text-style #:gadget-text-color
   #:icon-border-thickness
   #:vscrollbar-class #:hscrollbar-class
   #:vscrollbar #:hscrollbar
   #:raise #:sink
   #:button #:butcon
   #:text
   ))

