(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-rendering
  :components
  ((:file "packages" :depends-on ())
   (:file "trapezoids-from-polygons" :depends-on ("packages"))
   (:file "trapezoid-rendering" :depends-on ("packages"))
   (:file "polygon-rendering"
    :depends-on ("trapezoids-from-polygons" "trapezoid-rendering"))))


