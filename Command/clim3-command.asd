(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-command
  :depends-on (:climatis-packages :clim3-input :clim3-port)
  :components
  ((:file "command-packages" :depends-on ())
   (:file "command" :depends-on ("command-packages"))))
