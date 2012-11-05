(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-clouseau
  :depends-on (:climatis)
  :components
  ((:file "packages" :depends-on ())
   (:file "cons-cell-zone" :depends-on ("packages"))
   (:file "clouseau" :depends-on ("packages" "cons-cell-zone"))))
