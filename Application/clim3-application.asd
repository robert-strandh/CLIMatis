(cl:in-package #:asdf-user)

(defsystem :clim3-application
  :depends-on (:climatis-packages)
  :components
  ((:file "application-packages")
   (:file "application" :depends-on ("application-packages"))))

