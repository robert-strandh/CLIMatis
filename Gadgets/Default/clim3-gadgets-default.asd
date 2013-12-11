(cl:in-package #:common-lisp-user)

(asdf:defsystem :clim3-gadgets-default
  :depends-on (:clim3-gadgets
	       :clim3-color
	       :clim3-text-style)
  :components
  ((:file "packages")
   (:file "theme" :depends-on ("packages"))
   (:file "scrollbar" :depends-on ("packages" "theme"))))

