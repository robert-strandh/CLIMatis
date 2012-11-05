(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-paint
  :depends-on (:clim3-zone :clim3-port)
  :components
  ((:file "paint-packages" :depends-on ())
   (:file "paint" :depends-on ("paint-packages"))))
