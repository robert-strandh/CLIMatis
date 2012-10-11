(in-package #:common-lisp-user)

(asdf:defsystem :clim3-port
  :depends-on ()
  :components
  ((:file "port-packages" :depends-on ())
   (:file "port" :depends-on ("port-packages"))))
