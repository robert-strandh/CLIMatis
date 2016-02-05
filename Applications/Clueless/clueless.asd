(cl:in-package #:asdf-user)

(defsystem :clueless
  :depends-on (:climatis)
  :serial t
  :components
  ((:file "packages")
   (:file "clueless")))
