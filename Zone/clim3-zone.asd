(in-package #:common-lisp-user)

(asdf:defsystem :clim3-zone
  :depends-on (:climatis-packages :clim3-sprawl)
  :components
  ((:file "zone-packages" :depends-on ())
   (:file "protocol-class" :depends-on ("zone-packages"))
   (:file "parent" :depends-on ("zone-packages" "protocol-class"))
   (:file "children" :depends-on ("zone-packages" "parent"))
   (:file "client" :depends-on ("zone-packages" "protocol-class"))
   (:file "position" :depends-on ("zone-packages" "protocol-class"))
   (:file "size" :depends-on ("zone-packages" "protocol-class"))
   (:file "depth" :depends-on ("zone-packages" "protocol-class" "children"))
   (:file "sprawls" :depends-on ("zone-packages" "protocol-class"))
   (:file "layout" :depends-on ("zone-packages" "protocol-class"))
   (:file "zone" :depends-on ("zone-packages"
			      "protocol-class"
			      "parent"
			      "children"
			      "client"
			      "position"
			      "size"
			      "depth"
			      "sprawls"
			      "layout"))
   (:file "layout-mixins" :depends-on ("zone"))))
