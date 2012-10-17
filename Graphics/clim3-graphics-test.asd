(in-package #:common-lisp-user)

(asdf:defsystem :clim3-graphics-test
  :components
  ((:file "graphics-test-packages" :depends-on ())
   (:file "graphics-test" :depends-on ("graphics-test-packages"))))
