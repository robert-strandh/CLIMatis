(in-package #:common-lisp-user)

(asdf:defsystem :clim3-graphics
  :depends-on (:climatis-packages :clim3-zone :clim3-port :clim3-paint)
  :components
  ((:file "graphics-packages" :depends-on ())
   (:file "graphics" :depends-on ("graphics-packages"))))
