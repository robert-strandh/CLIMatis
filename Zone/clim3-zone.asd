(in-package #:common-lisp-user)

(asdf:defsystem :clim3-zone
  :depends-on (:climatis-packages :clim3-sprawl)
  :components
  ((:file "zone-packages" :depends-on ())
   (:file "protocol-class" :depends-on ("zone-packages"))
   (:file "parent" :depends-on ("zone-packages" "protocol-class"))
   (:file "client" :depends-on ("zone-packages" "protocol-class"))
   (:file "position" :depends-on ("zone-packages" "protocol-class"))
   (:file "size" :depends-on ("zone-packages" "protocol-class"))
   (:file "depth" :depends-on ("zone-packages" "protocol-class"))
   (:file "sprawls" :depends-on ("zone-packages" "protocol-class"))
   (:file "genealogy" :depends-on ("zone-packages" "protocol-class"))
   (:file "layout" :depends-on ("zone-packages" "protocol-class"))
   (:file "zone" :depends-on ("zone-packages"
			      "protocol-class"
			      "parent"
			      "client"
			      "position"
			      "size"
			      "depth"
			      "sprawls"
			      "genealogy"
			      "layout"))
   (:file "children" :depends-on ("zone"))
   (:file "layout-mixins" :depends-on ("zone"))))
