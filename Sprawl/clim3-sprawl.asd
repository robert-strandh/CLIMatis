(in-package #:common-lisp-user)

(asdf:defsystem :clim3-sprawl
  :components
  ((:file "sprawl-packages" :depends-on ())
   (:file "sprawl" :depends-on ("sprawl-packages"))))