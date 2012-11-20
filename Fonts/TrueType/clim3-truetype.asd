(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-truetype
  :depends-on (:zpb-ttf :clim3-rendering)
  :components
  ((:file "packages" :depends-on ())
   (:file "truetype" :depends-on ("packages"))))
