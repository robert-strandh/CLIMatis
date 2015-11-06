(in-package #:asdf-user)

(defsystem :clim3-zone
  :depends-on (:climatis-packages :clim3-sprawl)
  :serial t
  :components
  ((:file "zone-packages")
   (:file "protocol-class")
   (:file "parent")
   (:file "children")
   (:file "client")
   (:file "position")
   (:file "size")
   (:file "depth")
   (:file "sprawls")
   (:file "layout")
   (:file "zone")
   (:file "layout-mixins")))
