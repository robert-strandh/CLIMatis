(cl:in-package #:asdf-user)

(defsystem :clim3-paint
  :depends-on (:climatis-packages :clim3-zone :clim3-port :clim3-input)
  :components
  ((:file "paint-packages" :depends-on ())
   (:file "paint" :depends-on ("paint-packages"))))
