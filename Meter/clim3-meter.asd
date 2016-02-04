(cl:in-package #:asdf-user)

(defsystem :clim3-meter
  :depends-on (:climatis-packages)
  :serial t
  :components
  ((:file "meter-packages")
   (:file "meter")))
