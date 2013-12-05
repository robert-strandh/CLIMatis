(in-package #:common-lisp-user)

(asdf:defsystem :clim3-layout
  :depends-on (:climatis-packages :clim3-sprawl :clim3-zone)
  :components
  ((:file "layout-packages" :depends-on ())
   (:file "layout" :depends-on ("layout-packages"))
   (:file "trees" :depends-on ("layout-packages"))
   (:file "scrollbar" :depends-on ("layout-packages"))))
