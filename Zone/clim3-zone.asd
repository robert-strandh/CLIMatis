(in-package #:common-lisp-user)

(asdf:defsystem :clim3-zone
  :depends-on (:rigidity)
  :components
  ((:file "zone-packages" :depends-on ())
   (:file "gives-change" :depends-on ("zone-packages"))
   (:file "geometry-change" :depends-on ("zone-packages"))
   (:file "genealogy-change" :depends-on ("zone-packages"))
   (:file "genealogy" :depends-on ("zone-packages"))
   (:file "geometry" :depends-on ("zone-packages"))
   (:file "gives" :depends-on ("zone-packages"))
   (:file "zone" :depends-on ("zone-packages"
			      "gives-change"
			      "geometry-change"
			      "genealogy-change"
			      "genealogy"
			      "geometry"
			      "gives"))))