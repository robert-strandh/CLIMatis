(in-package :common-lisp-user)

(loop for subdir in '("Fonts/MF" "Fonts/Camfer" "Fonts/Icons"
		      "Fonts/TrueType"
		      "Backends/CLX-Framebuffer"
		      "Gadgets" "Command" "Application"
		      "Color" "Zone" "Layout" "Graphics" "Text" "Drawing"
		      "Meter" "Port" "Input" "Sprawl" "Ostream" "Paint"
		      "Gadgets" "Gadgets/Default")
      do (pushnew (concatenate
		   'string
		   "/home/strandh/Lisp/My-Projects/GIT-ified/CLIMatis2/"
		   subdir
		   "/")
		  asdf:*central-registry*
		  :test #'equal))

(asdf:defsystem :climatis
  :depends-on (:climatis-packages
	       :camfer :clim3-icons :clim3-sprawl :clim3-zone :clim3-layout
	       :clim3-color :clim3-clx-framebuffer :clim3-port
	       :clim3-graphics :clim3-text-style :clim3-text
	       :clim3-gadgets :clim3-command
	       :clim3-input :clim3-meter
	       :clim3-application
	       :clim3-gadgets :clim3-gadgets-default))

		       
