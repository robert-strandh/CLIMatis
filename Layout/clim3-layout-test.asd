(in-package #:common-lisp-user)

(asdf:defsystem :clim3-layout-test
  :components
  ((:file "layout-test-packages" :depends-on ())
   (:file "layout-test" :depends-on ("layout-test-packages"))))
