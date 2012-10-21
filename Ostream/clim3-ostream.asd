(in-package #:common-lisp-user)

(asdf:defsystem :clim3-ostream
  :depends-on (:clim3-zone :clim3-layout)
  :components
  ((:file "ostream-packages" :depends-on ())
   (:file "ostream" :depends-on ("ostream-packages"))))
