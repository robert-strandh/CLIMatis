(cl:in-package #:common-lisp-user)

(asdf:defsystem :abbrev
  :components
  ((:file "abbrev-packages" :depends-on ())
   (:file "abbrev" :depends-on ("abbrev-packages"))))
