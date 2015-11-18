(in-package #:asdf-user)

(defsystem :clim3-port
  :depends-on (:climatis-packages :clim3-zone)
  :serial t
  :components
  ((:file "port-packages")
   (:file "port")
   (:file "paint")))
