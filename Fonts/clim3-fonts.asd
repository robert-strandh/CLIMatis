(in-package #:asdf-user)

(defsystem :clim3-fonts
  :depends-on (:mf
	       :camfer
	       :clim3-icons
	       :clim3-truetype
               :cl-fad)
  :serial t
  :components
  ((:file "packages")
   (:file "fonts")))
