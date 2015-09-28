(cl:in-package #:asdf-user)

(defsystem :clim3-calendar
  :depends-on (:climatis)
  :components
  ((:file "calendar-packages" :depends-on ())
   (:file "calendar" :depends-on ("calendar-packages"))))

