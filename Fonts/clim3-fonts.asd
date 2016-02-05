(in-package #:asdf-user)

(loop with cwd = (make-pathname
                  :directory (pathname-directory (load-time-value (or #.*compile-file-pathname*
                                                                      *load-pathname*))))
      for subdir in '("MF/" "Camfer/" "Icons/" "TrueType/")
      do (pushnew (merge-pathnames subdir cwd) asdf:*central-registry* :test #'equal)
      finally (pushnew cwd asdf:*central-registry* :test #'equal))

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
