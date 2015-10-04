(cl:in-package #:asdf-user)

(defsystem :clim3-paint
  :depends-on (:climatis-packages :clim3-zone :clim3-port :clim3-input)
  :serial t
  :components
  ((:file "paint-packages")
   (:file "paint")))
