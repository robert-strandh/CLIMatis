(cl:in-package #:asdf-user)

(defsystem :clim3-text
  :depends-on (:climatis-packages
	       :clim3-zone
	       :clim3-graphics
	       :clim3-port
	       :clim3-paint)
  :serial t
  :components
  ((:file "text-packages" :depends-on ())
   (:file "text" :depends-on ("text-packages"))))
