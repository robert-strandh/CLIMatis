(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-command
  :components
  ((:file "command-packages" :depends-on ())
   (:file "command" :depends-on ("command-packages"))))
