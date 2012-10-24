(in-package #:common-lisp-user)

(asdf:defsystem :clim3-port
  :depends-on (:clim3-zone)
  :components
  ((:file "port-packages" :depends-on ())
   (:file "port" :depends-on ("port-packages"))))
