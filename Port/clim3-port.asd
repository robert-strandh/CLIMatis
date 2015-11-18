(in-package #:asdf-user)

(defsystem :clim3-port
  :depends-on (:climatis-packages :clim3-zone)
  :components
  ((:file "port-packages" :depends-on ())
   (:file "port" :depends-on ("port-packages"))
   (:file "paint" :depends-on ("port-packages" "port"))))
