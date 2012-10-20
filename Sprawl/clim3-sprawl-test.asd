(defsystem :clim3-sprawl-test
  :depends-on (:clim3-sprawl)
  :components
  ((:file "sprawl-test-packages" :depends-on ())
   (:file "sprawl-test" :depends-on ("sprawl-test-packages"))))
