(cl:in-package #:asdf-user)

(defsystem :2-3-tree-test
  :depends-on (:2-3-tree)
  :serial t
  :components
  ((:file "test-packages")
   (:file "test")))
