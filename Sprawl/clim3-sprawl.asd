(cl:in-package #:asdf-user)

(defsystem :clim3-sprawl
  :serial t
  :components
  ((:file "sprawl-packages")
   (:file "sprawl")))
