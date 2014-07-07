(defpackage #:app-config (:export #:*base-directory*))
(defparameter app-config:*base-directory*
    (make-pathname :name nil :type nil :defaults *load-truename*))

(in-package :common-lisp-user)

(loop for subdir in '(#P"Fonts/MF/" #P"Fonts/Camfer/" #P"Fonts/Icons/"
                      #P"Fonts/TrueType/"
                      #P"Backends/CLX-Framebuffer/"
                      #P"Gadgets/" #P"Command/" #P"Application/"
                      #P"Color/" #P"Zone/" #P"Layout/" #P"Graphics/" #P"Text/" #P"Drawing/"
                      #P"Meter/" #P"Port/" #P"Input/" #P"Sprawl/" #P"Ostream/" #P"Paint/"
                      #P"Gadgets/" #P"Gadgets/Default/"
                      #P"Trees/2-3/")
      do (pushnew (merge-pathnames subdir app-config:*base-directory*)
                  asdf:*central-registry*
                  :test #'equal))

(asdf:defsystem :climatis
  :depends-on (:climatis-packages
	       :2-3-tree
	       :camfer :clim3-icons :clim3-sprawl :clim3-zone :clim3-layout
	       :clim3-color :clim3-clx-framebuffer :clim3-port
	       :clim3-graphics :clim3-text-style :clim3-text
	       :clim3-gadgets :clim3-command
	       :clim3-input :clim3-meter
	       :clim3-application
	       :clim3-gadgets :clim3-gadgets-default))

		       
