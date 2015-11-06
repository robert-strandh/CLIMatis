(in-package #:asdf-user)

(defsystem :clim3-zone-test
  :serial t
  :components
  ((:file "zone-test-packages")
   (:file "zone-test")))
