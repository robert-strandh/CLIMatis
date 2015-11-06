(cl:in-package #:asdf-user)

(defsystem :clim3-application
  :depends-on (:climatis-packages)
  :serial t
  :components
  ((:file "application-packages")
   (:file "application")))
