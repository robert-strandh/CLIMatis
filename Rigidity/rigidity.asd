(in-package :common-lisp-user)

(asdf:defsystem :rigidity
  :components
  ((:file "packages" :depends-on ())
   (:file "rigidity" :depends-on ("packages"))))
