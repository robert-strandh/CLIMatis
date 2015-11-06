(cl:in-package #:asdf-user)

(defsystem :clim3-gadgets
  :depends-on (:clim3-zone
	       :clim3-input
	       :clim3-layout
	       :clim3-text
	       :clim3-graphics)
  :serial t
  :components
  ((:file "packages")
   (:file "theme")
   (:file "gadgets")
   (:file "raise-sink")
   (:file "button")
   (:file "butcon")
   (:file "text")))
