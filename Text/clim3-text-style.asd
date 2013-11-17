(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-text-style
  :depends-on (:climatis-packages)
  :components
  ((:file "text-style-packages" :depends-on ())
   (:file "text-style" :depends-on ("text-style-packages"))))
