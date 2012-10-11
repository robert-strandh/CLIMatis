(in-package #:common-lisp-user)

(asdf:defsystem :clim3-text
  :depends-on (:clim3-zone :clim3-graphics :clim3-port)
  :components
  ((:file "text-packages" :depends-on ())
   (:file "text" :depends-on ("text-packages"))))
