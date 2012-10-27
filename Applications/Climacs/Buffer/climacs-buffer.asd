(in-package #:common-lisp-user)

(asdf:defsystem :climacs-buffer
  :components
  ((:file "packages" :depends-on ())
   (:file "line" :depends-on ("packages"))
   (:file "cursor" :depends-on ("packages" "line"))
   (:file "buffer" :depends-on ("packages"))
   (:file "simple-line" :depends-on ("packages" "cursor" "line"))
   (:file "simple-buffer" :depends-on ("packages" "buffer"))))


