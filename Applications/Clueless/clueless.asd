(cl:in-package #:common-lisp-user)

(asdf:defsystem :clueless
  :depends-on (:climatis)
  :components
  ((:file "packages")
   (:file "clueless" :depends-on ("packages"))))
