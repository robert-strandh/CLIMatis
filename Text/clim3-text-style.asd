(cl:in-package #:asdf-user)

(defsystem :clim3-text-style
  :depends-on (:climatis-packages)
  :serial t
  :components
  ((:file "text-style-packages")
   (:file "text-style")))
