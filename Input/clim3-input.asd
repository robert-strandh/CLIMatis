(in-package #:common-lisp-user)

(asdf:defsystem :clim3-input
  :depends-on (:clim3-zone)
  :components
  ((:file "input-packages" :depends-on ())
   (:file "input" :depends-on ("input-packages"))))
