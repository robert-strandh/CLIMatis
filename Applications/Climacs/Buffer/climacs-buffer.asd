(in-package #:common-lisp-user)

(asdf:defsystem :climacs-buffer
  :components
  ((:file "packages" :depends-on ())
   (:file "cursor" :depends-on ("packages"))
   (:file "line" :depends-on ("packages" "cursor"))
   (:file "simple-line" :depends-on ("packages" "cursor" "line"))))


