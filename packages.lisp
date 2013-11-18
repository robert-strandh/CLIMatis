(cl:in-package #:common-lisp-user)

(defpackage #:clim3
  (:use #:common-lisp)
  (:export
   #:zone #:children #:child #:hpos #:vpos #:depth #:width #:height
   #:zone-p
   #:standard-zone
   #:hsprawl #:vsprawl
   #:natural-size
   #:frame #:hframe #:vframe
   #:brick #:hbrick #:vbrick
   #:sponge #:hsponge #:vsponge
   #:hbox #:hbox* #:vbox #:vbox* #:grid
   #:bboard #:bboard*
   #:wrap #:border
   #:pile #:pile*
   #:scroll 
   #:color #:make-color #:red #:green #:blue #:find-color #:name-color
   #:pixel #:opacity
   #:monochrome
   #:opaque
   #:translucent
   #:masked #:opacities
   #:image #:pixels
   #:connect #:disconnect
   #:event-loop
   #:input
   #:visit
   #:enter-handler
   #:leave-handler
   #:inside-p
   #:motion
   #:handler
  ))

(defpackage #:clim3-ext
  (:use #:common-lisp)
  (:export
   #:set-hpos #:set-vpos
   #:set-hsprawl #:set-vsprawl
   #:set-depth
   #:set-clients
   #:atomic-mixin
   #:compound-mixin
   #:at-most-one-child-mixin
   #:several-children-mixin
   #:list-children-mixin
   #:vector-children-mixin
   #:matrix-children-mixin
   #:map-over-children
   #:map-over-children-top-to-bottom
   #:map-over-children-bottom-to-top
   #:notify-child-position-changed
   #:notify-child-depth-changed
   #:notify-child-hsprawl-changed
   #:notify-child-vsprawl-changed
   #:changing-child-hsprawl-changes-hsprawl-mixin
   #:changing-child-hsprawl-changes-nothing-mixin
   #:changing-child-vsprawl-changes-vsprawl-mixin
   #:changing-child-vsprawl-changes-nothing-mixin
   #:changing-children-changes-hsprawl-mixin
   #:changing-children-changes-vsprawl-mixin
   #:changing-children-changes-both-sprawls-mixin
   #:changing-children-changes-nothing-mixin
   #:changing-child-position-not-allowed-mixin
   #:changing-child-position-changes-hsprawl-mixin
   #:changing-child-position-changes-vsprawl-mixin
   #:changing-child-position-changes-both-sprawls-mixin
   #:horizontally-very-elastic-mixin
   #:vertically-very-elastic-mixin
   #:child-depth-insignificant-mixin
   #:child-depth-significant-mixin
   #:ensure-hsprawl-valid
   #:ensure-vsprawl-valid
   #:ensure-child-layouts-valid
   #:impose-child-layouts
   #:impose-size
   #:compute-hsprawl
   #:compute-vsprawl
   #:client
   #:parent
   #:print-components
   ))
