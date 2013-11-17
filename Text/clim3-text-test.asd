(in-package #:common-lisp-user)

(asdf:defsystem :clim3-text-test
  :depends-on (:climatis-packages)
  :components
  ((:file "text-test-packages" :depends-on ())
   (:file "text-test" :depends-on ("text-test-packages"))))
