(in-package #:common-lisp-user)

(asdf:defsystem :clim3-color
  :depends-on (:climatis-packages)
  :components
  ((:file "packages" :depends-on ())
   (:file "color" :depends-on ("packages"))))
