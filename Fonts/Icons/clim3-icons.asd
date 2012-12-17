(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-icons
  :depends-on (:mf)
  :components
  ((:file "packages" :depends-on ())
   (:file "icons" :depends-on ("packages"))))
