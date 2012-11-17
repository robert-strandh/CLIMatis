(in-package :common-lisp-user)

(loop for subdir in '("Fonts/Camfer" "Backends/CLX-Framebuffer"
		      "Color" "Zone" "Layout" "Graphics" "Text" "Drawing"
		      "Meters" "Port" "Input" "Sprawl" "Ostream" "Paint")
      do (pushnew (concatenate
		   'string
		   "/home/strandh/Lisp/My-Projects/GIT-ified/CLIMatis2/"
		   subdir
		   "/")
		  asdf:*central-registry*
		  :test #'equal))

(asdf:defsystem :climatis
  :depends-on (:camfer :clim3-sprawl :clim3-zone :clim3-layout
	       :clim3-color :clim3-clx-framebuffer :clim3-port
	       :clim3-graphics :clim3-text :clim3-input :clim3-meters))

		       
