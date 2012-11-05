(defpackage #:clim3-port
  (:use #:common-lisp)
  (:export
   #:port
   #:make-port
   #:connect
   #:disconnect
   #:text-ascent
   #:text-descent
   #:text-width
   #:text-style-ascent
   #:text-style-descent
   #:port-standard-key-processor
   #:standard-key-processor
   #:paint-pixel
   #:*new-port*
   #:new-paint-pixel
   #:new-port-paint-pixel
   #:new-paint-mask
   #:new-port-paint-mask
   #:new-paint-text
   #:new-port-paint-text
   #:new-paint-opaque
   #:new-port-paint-opaque
   #:new-paint-trapezoids
   #:new-port-paint-trapezoids
   ))
  