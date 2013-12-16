(cl:in-package #:common-lisp-user)

(asdf:defsystem :2-3-tree
  :components
  ((:file "packages")
   (:file "2-3-tree" :depends-on ("packages"))))
