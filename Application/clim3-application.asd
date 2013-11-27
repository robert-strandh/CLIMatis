(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-application
  :depends-on (:climatis-packages)
  :components
  ((:file "application-packages")
   (:file "application" :depends-on ("application-packages"))))

