(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-rendering
  :depends-on (:cl-vectors)
  :serial t
  :components
  ((:file "packages")
   (:file "trapezoids-from-polygons")
   (:file "trapezoid-rendering")
   (:file "polygon-rendering")
   (:file "path-rendering")))
