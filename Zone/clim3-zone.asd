(in-package #:common-lisp-user)

(asdf:defsystem :clim3-zone
  :depends-on (:clim3-sprawl)
  :components
  ((:file "zone-packages" :depends-on ())
   (:file "parent" :depends-on ("zone-packages"))
   (:file "client" :depends-on ("zone-packages"))
   (:file "position" :depends-on ("zone-packages"))
   (:file "size" :depends-on ("zone-packages"))
   (:file "paint-protocol" :depends-on ("zone-packages"))
   (:file "gives-change" :depends-on ("zone-packages"))
   (:file "position-change" :depends-on ("zone-packages"))
   (:file "genealogy-change" :depends-on ("zone-packages"))
   (:file "genealogy" :depends-on ("zone-packages"))
   (:file "gives" :depends-on ("zone-packages"))
   (:file "layout" :depends-on ("zone-packages"))
   (:file "zone" :depends-on ("zone-packages"
			      "parent"
			      "client"
			      "position"
			      "size"
			      "gives-change"
			      "position-change"
			      "genealogy-change"
			      "genealogy"
			      "gives"
			      "layout"))
   (:file "layout-mixins" :depends-on ("zone"))))