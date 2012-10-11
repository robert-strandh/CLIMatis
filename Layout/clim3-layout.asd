(in-package #:common-lisp-user)

(asdf:defsystem :clim3-layout
  :depends-on (:rigidity :clim3-zone)
  :components
  ((:file "layout-packages" :depends-on ())
   (:file "layout" :depends-on ("layout-packages"))))
