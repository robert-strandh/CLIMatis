(cl:in-package #:asdf-user)

(defsystem :2-3-tree
  :components
  ((:file "packages")
   (:file "2-3-tree" :depends-on ("packages"))))
