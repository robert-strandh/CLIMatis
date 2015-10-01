(cl:in-package #:asdf-user)

(defsystem :2-3-tree-test
  :depends-on (:2-3-tree)
  :components
  ((:file "test-packages")
   (:file "test" :depends-on ("test-packages"))))
