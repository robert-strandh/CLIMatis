(in-package #:common-lisp-user)

(asdf:defsystem :clim3-zone
  :depends-on (:clim3-sprawl)
  :components
  ((:file "zone-packages" :depends-on ())
   (:file "gives-change" :depends-on ("zone-packages"))
   (:file "position-change" :depends-on ("zone-packages"))
   (:file "genealogy-change" :depends-on ("zone-packages"))
   (:file "genealogy" :depends-on ("zone-packages"))
   (:file "geometry" :depends-on ("zone-packages"))
   (:file "gives" :depends-on ("zone-packages"))
   (:file "layout" :depends-on ("zone-packages"))
   (:file "zone" :depends-on ("zone-packages"
			      "gives-change"
			      "position-change"
			      "genealogy-change"
			      "genealogy"
			      "geometry"
			      "gives"
			      "layout"))
   (:file "layout-mixins" :depends-on ("zone"))))