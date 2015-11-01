(cl:in-package :asdf-user)

(defsystem :rigidity
  :components
  ((:file "packages" :depends-on ())
   (:file "rigidity" :depends-on ("packages"))))
