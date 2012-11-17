(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-meter
  :components
  ((:file "meter-packages" :depends-on ())
   (:file "meter" :depends-on ("meter-packages"))))
