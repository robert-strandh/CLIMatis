(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-gadgets
  :depends-on (:clim3-zone
	       :clim3-input
	       :clim3-layout
	       :clim3-text
	       :clim3-graphics)
  :components
  ((:file "packages")
   (:file "theme" :depends-on ("packages"))
   (:file "gadgets" :depends-on ("packages"))
   (:file "raise-sink" :depends-on ("packages" "theme"))
   (:file "button" :depends-on ("packages" "theme" "raise-sink"))
   (:file "butcon" :depends-on ("packages" "theme"))
   (:file "text" :depends-on ("packages" "theme"))))
