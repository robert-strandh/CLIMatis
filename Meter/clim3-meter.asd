(cl:in-package #:asdf-user)

(defsystem :clim3-meter
  :depends-on (:climatis-packages)
  :components
  ((:file "meter-packages" :depends-on ())
   (:file "meter" :depends-on ("meter-packages"))))
