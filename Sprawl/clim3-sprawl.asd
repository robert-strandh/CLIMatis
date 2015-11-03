(cl:in-package #:asdf-user)

(defsystem :clim3-sprawl
  :components
  ((:file "sprawl-packages" :depends-on ())
   (:file "sprawl" :depends-on ("sprawl-packages"))))
