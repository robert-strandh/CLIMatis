(cl:in-package #:asdf-user)

(defsystem :clim3-calendar
  :depends-on (:climatis)
  :serial t
  :components
  ((:file "calendar-packages")
   (:file "calendar")))
