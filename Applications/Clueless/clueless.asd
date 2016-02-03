(cl:in-package #:asdf-user)

(defsystem :clueless
  :depends-on (:climatis)
  :components
  ((:file "packages")
   (:file "clueless" :depends-on ("packages"))))
