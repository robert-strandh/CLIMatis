(in-package #:asdf-user)

(defsystem :climatis
  :depends-on (:climatis-packages
	       :trivial-benchmark
	       :2-3-tree
	       :clim3-fonts :clim3-sprawl :clim3-zone :clim3-layout
	       :clim3-color :clim3-clx-framebuffer :clim3-port
	       :clim3-graphics :clim3-text-style :clim3-text
	       :clim3-gadgets :clim3-command
	       :clim3-input
	       :clim3-application
	       :clim3-gadgets :clim3-gadgets-default))
