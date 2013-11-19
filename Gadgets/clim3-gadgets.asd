(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-gadgets
  :depends-on (:clim3-zone
	       :clim3-input)
  :components
  ((:file "packages")
   (:file "gadgets" :depends-on ("packages"))))

	       
