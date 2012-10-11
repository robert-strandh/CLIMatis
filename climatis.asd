(in-package :common-lisp-user)

(asdf:defsystem :climatis
  :depends-on (:camfer :png-read)
  :components
  ((:file "packages" :depends-on ())
   (:file "color" :depends-on ("packages"))
   (:file "text-style" :depends-on ("packages"))
   (:file "X11-colors" :depends-on ("packages" "color"))
   (:file "events" :depends-on ("packages"))
   (:file "input-method" :depends-on ("packages" "events"))
   (:file "elasticity" :depends-on ("packages"))
   (:file "zones" :depends-on ("packages" "port" "elasticity"))
   (:file "port" :depends-on ("packages"))
   (:file "command" :depends-on ("packages"))))

(asdf:defsystem :climatis-clx-framebuffer
  :depends-on (:climatis)
  :pathname #.(make-pathname :directory '(:relative "Backends" "CLX-Framebuffer"))
  :components
  ((:file "packages" :depends-on ())
   (:file "port" :depends-on ("packages"))))

(asdf:defsystem :climatis-png-file-reader
  :depends-on (:climatis)
  :pathname #.(make-pathname :directory '(:relative "Image-file-readers" "PNG"))
  :components
  ((:file "packages" :depends-on ())
   (:file "png" :depends-on ("packages"))))

(asdf:defsystem :climatis-test
  :depends-on (:climatis :climatis-clx-framebuffer :climatis-png-file-reader)
  :components
  ((:file "test" :depends-on ())))
