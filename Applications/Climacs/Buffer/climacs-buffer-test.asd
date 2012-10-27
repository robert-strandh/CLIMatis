(in-package #:common-lisp-user)

(asdf:defsystem :climacs-buffer-test
  :depends-on (:climacs-buffer)
  :components
  ((:file "buffer-test-packages")
   (:file "buffer-test" :depends-on ("buffer-test-packages"))))


