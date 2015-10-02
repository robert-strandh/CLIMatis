(in-package #:asdf-user)

(defsystem :clim3-zone-test
  :components
  ((:file "zone-test-packages" :depends-on ())
   (:file "zone-test" :depends-on ("zone-test-packages"))))
