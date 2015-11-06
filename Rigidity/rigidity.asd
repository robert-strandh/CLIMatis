(cl:in-package :asdf-user)

(defsystem :rigidity
  :serial t
  :components
  ((:file "packages")
   (:file "rigidity")))
