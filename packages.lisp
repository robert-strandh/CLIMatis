(cl:in-package #:common-lisp-user)

(defpackage #:clim3
  (:use #:common-lisp)
  (:shadow #:abort)
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
   #:enter
   #:leave
   #:clickable #:attention #:at-ease
   #:action
   #:enter-handler
   #:leave-handler
   #:inside-p
   #:motion
   #:handler
   #:port
   #:make-port
   #:event-loop
   #:connect
   #:disconnect
   #:text-ascent
   #:text-descent
   #:text-width
   #:text-prefix-length
   #:text-style-ascent
   #:text-style-descent
   #:with-zone
   #:with-area
   #:with-position
   #:paint-pixel
   #:paint-mask
   #:paint-text
   #:paint-opaque
   #:paint-translucent
   #:paint-trapezoids
   #:*port*
   #:standard-key-decoder
   #:standard-button-decoder
   #:key-handler
   #:*key-handler*
   #:handle-key-press
   #:handle-key-release
   #:null-key-handler
   #:*null-key-handler*
   #:read-keystroke
   #:button-handler
   #:*button-handler*
   #:handle-button-press
   #:handle-button-release
   #:button-press
   #:button-release
   #:null-button-handler
   #:*null-button-handler*
   #:text-style
   #:foundry
   #:family
   #:face
   #:size
   #:highlight #:unhighlight
   #:presentation #:gem
   #:ptypep
   #:zone-button-handler
   #:define-command
   #:command-table
   #:command-name-in-table-p
   #:hashed-command-table
   #:command-loop
   #:command-loop-iteration
   #:acquire-action
   #:active-command-p
   #:abort
   #:?
   #:application
   #:view
   #:current-view
   #:key-processor
   #:key-processor-key-handler
   #:submit-keystroke
   #:*application*
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
   #:call-with-zone #:call-with-area #:call-with-position
   #:paint
   #:paint-pixel
   #:paint-mask
   #:paint-text
   #:paint-opaque
   #:paint-translucent
   #:paint-trapezoids
   #:repaint
   #:standard-key-decoder
   #:standard-button-decoder
   #:*input-context*
   #:*command-table*
   #:all-children
   #:map-over-all-children
   ))
