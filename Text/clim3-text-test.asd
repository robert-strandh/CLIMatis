(in-package #:asdf-user)

(defsystem :clim3-text-test
  :depends-on (:climatis-packages)
  :serial t
  :components
  ((:file "text-test-packages")
   (:file "text-test")))
