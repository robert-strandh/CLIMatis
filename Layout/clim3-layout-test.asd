(in-package #:common-lisp-user)

(asdf:defsystem :clim3-layout-test
  :depends-on (:climatis-packages)
  :components
  ((:file "layout-test-packages" :depends-on ())
   (:file "layout-test" :depends-on ("layout-test-packages"))))
