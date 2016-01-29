(in-package #:asdf-user)

(loop with cwd = (make-pathname
                  :directory (pathname-directory (load-time-value (or #.*compile-file-pathname*
                                                                      *load-pathname*))))
      for subdir in '("Fonts/MF/" "Fonts/Camfer/" "Fonts/Icons/"
		      "Fonts/TrueType/" "Trees/2-3/"
		      "Backends/CLX-Framebuffer/"
		      "Gadgets/" "Command/" "Application/"
		      "Color/" "Zone/" "Layout/" "Graphics/" "Text/" "Drawing/"
		      "Meter/" "Port/" "Input/" "Sprawl/" "Ostream/" "Paint/"
		      "Gadgets/" "Gadgets/Default/")
      do (pushnew (merge-pathnames subdir cwd) asdf:*central-registry* :test #'equal)
      finally (pushnew cwd asdf:*central-registry* :test #'equal))

(defsystem :climatis
  :depends-on (:climatis-packages
	       :2-3-tree
	       :camfer :clim3-icons :clim3-sprawl :clim3-zone :clim3-layout
	       :clim3-color :clim3-clx-framebuffer :clim3-port
	       :clim3-graphics :clim3-text-style :clim3-text
	       :clim3-gadgets :clim3-command
	       :clim3-input :clim3-meter
	       :clim3-application
	       :clim3-gadgets :clim3-gadgets-default))

		       
