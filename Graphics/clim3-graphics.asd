(in-package #:common-lisp-user)

(asdf:defsystem :clim3-graphics
  :depends-on (:clim3-zone :clim3-port)
  :components
  ((:file "graphics-packages" :depends-on ())
   (:file "graphics" :depends-on ("graphics-packages"))))
