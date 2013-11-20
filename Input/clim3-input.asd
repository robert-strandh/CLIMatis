(in-package #:common-lisp-user)

(asdf:defsystem :clim3-input
  :depends-on (:climatis-packages :clim3-zone)
  :components
  ((:file "input-packages" :depends-on ())
   (:file "input" :depends-on ("input-packages"))
   (:file "presentation" :depends-on ("input-packages" "input"))))

