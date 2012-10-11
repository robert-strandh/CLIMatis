(in-package #:common-lisp-user)

(asdf:defsystem :clim3-clx-framebuffer
  :depends-on (:clim3-port :clim3-zone :clim3-layout :clim3-graphics
	       :camfer)
  :components
  ((:file "clx-framebuffer-packages" :depends-on ())
   (:file "clx-framebuffer" :depends-on ("clx-framebuffer-packages"))))

	    